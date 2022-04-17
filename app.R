#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Initialize ----
source("packages.R")
for(f in list.files("R/", full.names = TRUE)) source(f)

n_obs <- 1e6

set.seed(329)

dt_fake <- data.table(
  year_ABDHMO = sample(2007:2019, size = n_obs, replace = TRUE),
  Age = sample(1:5, size = n_obs, replace = TRUE),
  Gender = sample(1:2, size = n_obs, replace = TRUE),
  Race = sample(0:6, size = n_obs, replace = TRUE)
)

dt_fake[, Pre_index_statin_intensity := abs(rnorm(n_obs, mean = Age/2))]
dt_fake[, Pre_index_statin := rbinom(n_obs, size = 1, prob = Gender/3)]
dt_fake[, Post_index_statin := rbinom(n_obs, size = 1, prob = Race/sum(0:6))]
dt_fake[, Post_index_statin_days := abs(round(rnorm(n_obs, mean = 180, sd = 50)))]

dt_fake[, Age := factor(Age,
                        levels = 1:5,
                        labels = c("65-69",
                                   "70-74",
                                   "75-79",
                                   "80-84",
                                   "85+"))]

dt_fake[, Gender := factor(Gender,
                           levels = 1:2,
                           labels = c("Male", "Female"))]

dt_fake[, Race := factor(Race,
                         levels = 0:6,
                         labels = c("Unknown",
                                    "White",
                                    "Black",
                                    "Other",
                                    "Asian",
                                    "Hispanic",
                                    "Native"))]

dt_stroke <- copy(dt_fake)
dt_ami <- copy(dt_fake)
dt_racs <- copy(dt_fake)

# UI ----

dash_input_width <- '97.5%'
dash_sidebar_width <- 390

dash_sidebar <- dashboardSidebar(

  sidebarMenu(

    pickerInput(
      inputId = 'dataset',
      label = 'Select a dataset',
      choices = c("racs"   = "racs",
                  "ami"    = "ami",
                  "stroke" = "stroke"),
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(maxOptions = 1),
      width = dash_input_width
    ),

    conditionalPanel(
      condition = 'input.dataset.length > 0',
      prettyCheckboxGroup(
        inputId = "year",
        label = "Select index year(s)",
        inline = TRUE,
        selected = NULL,
        width = dash_input_width
      ),

      actionGroupButtons(
        inputIds = c('select_all_years', 'deselect_all_years'),
        labels = c("Select all", "Clear"),
        status = 'info'
      )

    ),

    conditionalPanel(
      condition = "input.year.length > 0",
      pickerInput(
        inputId = 'outcome',
        label = 'Select an outcome',
        choices = c(),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = dash_input_width
      )
    ),

    conditionalPanel(
      condition = 'input.year.length > 0 &
                   input.outcome.length > 0',
      pickerInput(
        inputId = 'exposure',
        label = 'Select an exposure',
        choices = c('None'),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = dash_input_width
      )
    ),

    conditionalPanel(
      condition = 'input.year.length > 0 &
                   input.outcome.length > 0 &
                   input.exposure.length > 0',
      pickerInput(
        inputId = 'subset_variable',
        label = 'Select a subsetting variable',
        choices = c('None'),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = dash_input_width
      )
    ),

    conditionalPanel(
      condition = "input.year.length > 0 &
                   input.outcome.length > 0 &
                   input.exposure.length > 0 &
                   input.subset_variable.length > 0 &
                   input.subset_variable != 'None'",
      prettyCheckboxGroup(
        inputId = 'subset_value',
        label = 'Include these subsets:',
        choices = c(),
        selected = NULL,
        width = dash_input_width
      )
    ),

    conditionalPanel(
      condition = 'input.year.length > 0 &
                   input.outcome.length > 0 &
                   input.exposure.length > 0 &
                   input.subset_variable.length > 0',
      pickerInput(
        inputId = 'group',
        label = 'Select a grouping variable',
        choices = c(),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = dash_input_width
      )
    ),

    conditionalPanel(
      condition = "(input.year.length > 0 &
                     input.outcome.length > 0 &
                     input.subset_variable.length == 0 | input.subset_variable == 'None') |
                   (input.year.length > 0 &
                     input.outcome.length > 0 &
                     input.subset_value.length > 0 & input.subset_variable.length > 0)",
      actionButton(
        inputId =  "do_computation",
        label = "Compute my results",
        width = dash_sidebar_width*6.01/7,
        icon = icon("cog"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )



  ),

  minified = FALSE,
  collapsed = FALSE,
  width = dash_sidebar_width

)


ui <- dashboardPage(

  options = list(sidebarExpandOnHover = TRUE),

  header = dashboardHeader(titleWidth = dash_sidebar_width),

  sidebar = dash_sidebar,

  body = dashboardBody(
    tabItem(
      tabName = 'dashboard',
      fluidRow(
        valueBoxOutput('result_overall')
      ),
      fluidRow(
        gt_output('result_table')
      )
    )
  ),

  controlbar = dashboardControlbar(),

  title = "DashboardPage"

)

# Server ----

server = function(input, output, session) {

  dt <- reactive({
    switch(input$dataset,
           "racs" = dt_racs,
           "ami" = dt_ami,
           "stroke" = dt_stroke)
  })

  dt_years <- reactive({get_unique(dt()$year_ABDHMO)})

  observeEvent(input$dataset, {

    # fix the current value so that freezing won't prevent ui update
    year_current <- input$year

    freezeReactiveValue(input, 'year')

    updatePrettyCheckboxGroup(
      inputId = 'year',
      choices = as.character(sort(dt_years())),
      selected = as.character(intersect(dt_years(), year_current)),
      inline = TRUE
    )

    outcome_inputs <- key_data |>
      filter(outcome,
             variable %in% names(dt())) |>
      select(label, variable) |>
      deframe()

    updatePickerInput(
      session = session,
      inputId = 'outcome',
      choices = outcome_inputs,
      selected = intersect(names(dt()), input$outcome)
    )

  })

  observeEvent(input$select_all_years, {
    updatePrettyCheckboxGroup(inputId = 'year',
                              selected = dt_years())
  })

  observeEvent(input$deselect_all_years, {
    updatePrettyCheckboxGroup(inputId = 'year',
                              selected = character(0))
  })

  observeEvent(input$outcome, {

    exposure_inputs <- key_data |>
      filter(exposure,
             variable %in% setdiff(names(dt()),
                                   input$outcome)) |>
      select(label, variable) |>
      deframe()

    updatePickerInput(
      session = session,
      inputId = 'exposure',
      choices = c("None", exposure_inputs),
      selected = setdiff(input$exposure, input$outcome)
    )

    if(!is.null(input$subset)){

      subset_inputs <- key_data |>
        filter(subset,
               variable %in% setdiff(names(dt()), c(input$outcome))) |>
        select(label, variable) |>
        deframe()

      subset_selected <- setdiff(input$subset_variable, c(input$outcome))

      updatePickerInput(
        session = session,
        inputId = 'subset_variable',
        choices = c("None", subset_inputs),
        selected = subset_selected
      )

    }

    if(!is.null(input$group)){

      group_inputs <- key_data |>
        filter(group,
               variable %in% setdiff(names(dt()),
                                     c(input$outcome,
                                       input$exposure))) |>
        select(label, variable) |>
        deframe()

      group_selected <- setdiff(input$group,
                                c(input$outcome,
                                  input$exposure))

      updatePickerInput(
        session = session,
        inputId = 'group',
        choices = c("None", group_inputs),
        selected = group_selected
      )

    }

  })

  observeEvent(input$exposure, {

    subset_inputs <- key_data |>
      filter(subset,
             variable %in% setdiff(names(dt()), c(input$outcome))) |>
      select(label, variable) |>
      deframe()

    subset_selected <- setdiff(input$subset_variable, c(input$outcome))

    updatePickerInput(
      session = session,
      inputId = 'subset_variable',
      choices = c("None", subset_inputs),
      selected = subset_selected
    )

    if(!is.null(input$group)){
      if(input$exposure == input$group){
        updatePickerInput(
          session = session,
          inputId = 'group',
          selected = character(0)
        )
      }
    }

  })


  observeEvent(input$subset_variable, {

    updatePrettyCheckboxGroup(
      inputId = 'subset_value',
      choices = get_unique(dt()[[input$subset_variable]]),
      selected = character(0) #subset_value_selected
    )

    # input$subset_variable, include or no?

    group_inputs <- key_data |>
      filter(group,
             variable %in% setdiff(names(dt()),
                                   c(input$outcome,
                                     input$exposure))) |>
      select(label, variable) |>
      deframe()

    group_selected <- setdiff(input$group,
                              c(input$outcome,
                                input$exposure))

    updatePickerInput(
      session = session,
      inputId = 'group',
      choices = c("None", group_inputs),
      selected = group_selected
    )


  })

  result <- reactive({

    dt_list <- reactiveValuesToList(input)[c('year',
                                             'outcome',
                                             'exposure',
                                             'subset_variable',
                                             'subset_value',
                                             'group')]

    dt_list$year <- I(as.numeric(dt_list$year))

    dt_list$f <- switch(key_list[[dt_list$outcome]]$type,
                        'intg' = mean,
                        'ctns' = mean,
                        'bnry' = mean,
                        'ttev' = mean,
                        'catg' = stop("not yet supported"))


    dt_make <- ".(outcome = f(outcome))"

    dt_subset <- "year_ABDHMO %in% year"

    if(is_used(dt_list$subset_variable)){

      dt_list$subset_value <- I(dt_list$subset_value)

      dt_subset <- paste(dt_subset,
                         "subset_variable %in% subset_value",
                         sep = ' & ')
    }

    dt_group <- c("year_ABDHMO")

    if(is_used(dt_list$exposure))
      dt_group <- c(dt_group, dt_list$exposure)

    if(is_used(dt_list$group))
      dt_group <- c(dt_group, dt_list$group)

    dt_group <- glue::glue(
      "keyby = .({paste(dt_group, collapse = ', ')})"
    )

    dt_call_overall <- rlang::parse_expr(
      glue::glue(
        "dt()[{dt_subset}, {dt_make}, , env = dt_list]"
      )
    )

    dt_call_grouped <- rlang::parse_expr(
      glue::glue(
        "dt()[{dt_subset}, {dt_make}, {dt_group}, env = dt_list]"
      )
    )

    # dont run the computations twice if there are no groups
    overall <- eval(expr = dt_call_overall)
    grouped <- eval(expr = dt_call_grouped)

    list(
      overall = overall,
      grouped = grouped
    )

  }) |>
    bindEvent(input$do_computation)


  output$result_overall <-
    renderValueBox(
      valueBox(
        table_value(result()$overall[[input$outcome]]),
        "Overall result",
        icon = icon("heart")
      )
    ) |>
    bindEvent(input$do_computation)

  output$result_table <-
    render_gt(
      {

        data_gt <- result()$grouped

        setnames(data_gt,
                 old = input$outcome,
                 new = key_list[[input$outcome]]$label)

        if(is_used(input$exposure)){

          dcast_formula <- glue(
            "year_ABDHMO ~ {input$exposure}"
          )

          if(is_used(input$group)){
            dcast_formula <- glue(
              "year_ABDHMO + {input$group} ~ {input$exposure}"
            )
          }

          data_gt <- data_gt |>
            dcast(
              formula = as.formula(dcast_formula),
              value.var = key_list[[input$outcome]]$label
            )

        }

        data_gt <- data_gt |>
          mutate(across(.cols = c(where(is.numeric), -year_ABDHMO),
                        .fns = table_value))

        if(is_used(input$group)){
          gt_out <- gt(data_gt,
                       rowname_col = 'year_ABDHMO',
                       groupname_col = input$group)
        } else {
          gt_out <- gt(data_gt,
                       rowname_col = 'year_ABDHMO')
        }

        if(is_used(input$exposure)){
          gt_out <- gt_out |>
            tab_spanner(
              label = key_list[[input$exposure]]$label,
              columns = as.character(
                get_unique(result()$grouped[[input$exposure]])
              )
            )
        }

        gt_out |>
          tab_stubhead(label = 'Calendar year') |>
          cols_align('center')

      }

    ) |>
    bindEvent(input$do_computation)



}

shinyApp(ui = ui, server = server)



