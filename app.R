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

key_data <- read_csv('data/key.csv')

key_list <- key_data |>
  table.glue::as_inline(
    tbl_variables = 'variable',
    tbl_values = setdiff(names(key_data), 'variable')
  )

for(f in list.files("R/", full.names = TRUE)) source(f)

n_obs <- 1e5

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

input_width <- '97.5%'
sidebar_width <- 390

ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # Application title
    introBox(
      titlePanel("Medicap"),
      data.step = 1,
      data.intro = "This is an application to explore Medicare data"
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(

        introBox(
          actionButton("help",
                       "Press for instructions",
                       icon = icon("question"),
                       width = '95%')
        ),

        br(),

        introBox(

          conditionalPanel(
            condition =
              "(input.year.length > 0 &
                input.outcome.length > 0 &
                (input.subset_variable.length == 0 | input.subset_variable == 'None'))
                |
              (input.year.length > 0 &
                input.outcome.length > 0 &
                input.subset_value.length > 0 &
                input.subset_variable.length > 0)",
            actionButton(
              inputId =  "do_computation",
              label = "Compute my results",
              width = '95%',
              icon = icon("cog"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          ),

          conditionalPanel(
            condition = "!((input.year.length > 0 &
                     input.outcome.length > 0 &
                     (input.subset_variable.length == 0 | input.subset_variable == 'None')) |
                   (input.year.length > 0 &
                     input.outcome.length > 0 &
                     input.subset_value.length > 0 & input.subset_variable.length > 0))",
            actionButton(
              inputId =  "wont_do_computation",
              label = "Compute my results",
              width = '95%',
              icon = icon("cog"),
              style = "color: #fff; background-color: #808080; border-color: #2e6da4"
            )
          ),

          data.step = 5,
          data.intro = paste(
            "When you have selected a dataset, an outcome",
            "and at least one index year, this button will turn blue,",
            "and clicking it will generate results in the main panel.",
            "If the button is grey, it means at least one required input",
            "is currently unspecified."
          )
        ),

        br(),

        introBox(
          pickerInput(
            inputId = 'dataset',
            label = 'Select a dataset',
            choices = c("racs"   = "racs",
                        "ami"    = "ami",
                        "stroke" = "stroke"),
            selected = 'racs',
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1),
            width = input_width
          ),
          data.step = 2,
          data.intro = paste(
            "Start by selecting which dataset you'd like to analyze."
          )
        ),

        prettyCheckboxGroup(
          inputId = "year",
          label = "Select index year(s)",
          inline = TRUE,
          selected = NULL,
          width = input_width
        ),

        actionGroupButtons(
          inputIds = c('select_all_years', 'deselect_all_years'),
          labels = c("Select all", "Clear"),
          status = 'info'
        ),

        br(),br(),

        introBox(
          pickerInput(
            inputId = 'outcome',
            label = 'Select an outcome',
            choices = c(),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1),
            width = input_width
          ),
          data.step = 3,
          data.intro = paste(
            "Next, select an outcome from one of the available options.",
            "The 'outcome' is the variable that will be summarized in results.",
            "In rare cases where a summary statistic for your outcome is",
            "based on < 12 people, that statistic will not be displayed."
          )
        ),

        introBox(
          pickerInput(
            inputId = 'exposure',
            label = 'Select an exposure',
            choices = c('None'),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1),
            width = input_width
          ),

          pickerInput(
            inputId = 'subset_variable',
            label = 'Select a subsetting variable',
            choices = c('None'),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1),
            width = input_width
          ),

          conditionalPanel(
            condition = "input.subset_variable.length > 0 &
                       input.subset_variable != 'None'",
            prettyCheckboxGroup(
              inputId = 'subset_value',
              label = 'Include these subsets:',
              choices = c(),
              selected = NULL,
              width = input_width
            )
          ),

          pickerInput(
            inputId = 'group',
            label = 'Select a grouping variable',
            choices = c(),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1),
            width = input_width
          ),
          data.step = 4,
          data.intro = paste(
            "Your possible choices for the exposure variable, the subset",
            "variable, and the grouping variable are dependent on what",
            "you select for the outcome variable and on each other. To",
            "keep things simple, you will not see any choices for these",
            "inputs unless you have selected a value for ALL the inputs",
            "above them."
          )
        )



      ),

      # Show a plot of the generated distribution
      mainPanel(

        # fluidRow(valueBoxOutput('result_overall')),

        fluidRow(
          gt_output('result_table')
        )
      )
    )
  )
)

  # Server ----

  server = function(input, output, session) {

    # initiate hints on startup with custom button and event
    hintjs(session,
           options = list("hintButtonLabel"="Hope this hint was helpful"),
           events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))

    dt <- reactive({
      switch(input$dataset,
             "racs" = dt_racs,
             "ami" = dt_ami,
             "stroke" = dt_stroke)
    })

  dt_years <- reactive({get_unique(dt()$year_ABDHMO)})

  observeEvent(input$dataset, {

    selected <- intersect(dt_years(), input$year)
    if(is_empty(selected))
      selected <- max(dt_years())

    updatePrettyCheckboxGroup(
      inputId = 'year',
      choices = as.character(sort(dt_years())),
      selected = as.character(selected),
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

  # the is_empty event has to come before the regular event.
  # Otherwise, the downstream inputs never show up. Why?
  observeEvent(is_empty(input$outcome), {

    updatePickerInput(
      session = session,
      inputId = 'exposure',
      choices = character(0),
      selected = character(0)
    )

    updatePickerInput(
      session = session,
      inputId = 'subset_variable',
      choices = character(0),
      selected = character(0)
    )

    updatePickerInput(
      session = session,
      inputId = 'group',
      choices = character(0),
      selected = character(0)
    )

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

    if(!is.null(input$subset_variable)){

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

  observeEvent(is_empty(input$exposure), {

   updatePickerInput(
      session = session,
      inputId = 'subset_variable',
      choices = character(0),
      selected = character(0)
    )

    updatePickerInput(
      session = session,
      inputId = 'group',
      choices = character(0),
      selected = character(0)
    )

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

  observeEvent(is_empty(input$subset_variable), {

    updatePickerInput(
      session = session,
      inputId = 'group',
      choices = character(0),
      selected = character(0)
    )

  })

  observeEvent(input$subset_variable, {

    # TODO: if subset_variable doesn't change, neither should subset values.
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

    dt_group_no_year <- glue::glue(
      "keyby = .({paste(setdiff(dt_group, 'year_ABDHMO'), collapse = ', ')})"
    )

    dt_group <- glue::glue(
      "keyby = .({paste(dt_group, collapse = ', ')})"
    )
    dt_call_no_year <- rlang::parse_expr(
      glue::glue(
        "dt()[{dt_subset}, {dt_make}, {dt_group_no_year}, env = dt_list]"
      )
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

    list(
      overall = eval(expr = dt_call_overall),
      no_year = eval(expr = dt_call_no_year),
      grouped = eval(expr = dt_call_grouped)
    )

  }) |>
    bindEvent(input$do_computation)


  # output$result_overall <-
  #   renderValueBox(
  #     valueBox(
  #       table_value(result()$overall[[input$outcome]]),
  #       "Overall result",
  #       icon = icon("heart")
  #     )
  #   ) |>
  #   bindEvent(input$do_computation)

  output$result_table <-
    render_gt({

        data_gt <- result()$no_year |>
          mutate(year_ABDHMO = 'Overall') |>
          bind_rows(
            mutate(result()$grouped,
                   year_ABDHMO = as.character(year_ABDHMO))
          ) |>
          mutate(
            year_ABDHMO = factor(
              year_ABDHMO,
              levels = c("Overall", paste(sort(input$year)))
            )
          )

        dcast_lhs_variables <- c("1")

        if(is_used(input$exposure)){
          dcast_lhs_variables <- c(dcast_lhs_variables, input$exposure)
        }

        if(is_used(input$group)){
          dcast_lhs_variables <- c(dcast_lhs_variables, input$group)
        }

        dcast_formula_lhs <- paste(
          dcast_lhs_variables,
          collapse = ' + '
        )

        dcast_formula <- glue("{dcast_formula_lhs} ~ year_ABDHMO")


        data_gt <- data_gt |>
          dcast(
            formula = as.formula(dcast_formula),
            value.var = input$outcome
          )

        if('.' %in% names(data_gt)) data_gt[['.']] <- NULL


        data_gt <- data_gt |>
          mutate(across(.cols = where(is.numeric),
                        .fns = table_value))


        gt_args <- list(data = data_gt)

        if(is_used(input$exposure)) gt_args$rowname_col = input$exposure
        if(is_used(input$group)) gt_args$groupname_col = input$group

        gt_out <- do.call(gt, args = gt_args)

        if(is_used(input$exposure)){
          gt_out <- gt_out |>
            tab_stubhead(label = key_list[[input$exposure]]$label)
        }

        gt_out <- gt_out |>
          tab_spanner(
            label = 'Calendar year',
            columns = as.character(input$year)
          )

        gt_out |>
          cols_align('center')

      }) |>
    bindEvent(input$do_computation)

  # start introjs when button is pressed with custom options and events
  observeEvent(
    input$help, {

      if(is.null(input$dataset)){
        updatePickerInput(
          session = session,
          inputId = 'dataset',
          selected = 'racs'
        )
      }
    }

  )

  observeEvent(
    input$help, {

      introjs(session,
              options = list("nextLabel"="Next",
                             "prevLabel"="Previous",
                             "skipLabel"="Skip"))

    }

  )

  }

shinyApp(ui = ui, server = server)



