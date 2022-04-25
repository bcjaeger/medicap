#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Initialize ----

source("packages.R")

for(f in list.files("R/", full.names = TRUE)) source(f)

key_data <- read_csv('data/key.csv',
                     col_names = TRUE,
                     col_types = cols(variable = col_character(),
                                      type = col_character(),
                                      label = col_character(),
                                      outcome = col_logical(),
                                      exposure = col_logical(),
                                      subset = col_logical(),
                                      group = col_logical(),
                                      time = col_character()))

key_time <- key_data |>
  filter(type == 'time') |>
  slice(1) |>
  pull(variable)

key_list <- key_data |>
  table.glue::as_inline(
    tbl_variables = 'variable',
    tbl_values = setdiff(names(key_data), 'variable')
  )

ttev_outcome <- key_data |>
  filter(type == 'ttev', outcome) |>
  pull(variable)

ttev_condition <- paste0("'", ttev_outcome, "'")

ttev_condition <- paste0("input.outcome == ",
                         ttev_condition,
                         collapse = ' | ')

ttev_condition <- paste0('input.outcome.length > 0 & (',
                         ttev_condition,
                         ')')

ctns_exposure <- key_data |>
  filter(type == 'ctns', exposure) |>
  pull(variable)

ctns_condition <- paste0("'", ctns_exposure, "'")

ctns_condition <- paste0("input.exposure == ",
                         ctns_condition,
                         collapse = ' | ')

ctns_condition <- paste0('input.exposure.length > 0 & (',
                         ctns_condition,
                         ')')



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

stat_recoder <- c(
  'n_event' = 'Number of events',
  'n_total' = 'Number of patients',
  'prevalence' = 'Prevalence',
  'odds' = 'Odds',
  'mean' = 'Mean',
  'quant_0' = 'Minimum',
  'quant_25' = '25th percentile',
  'quant_50' = '50th percentile',
  'quant_75' = '75th percentile',
  'quant_100' = 'Maximum',
  'cuminc' = 'Cumulative incidence',
  'sd' = 'Standard deviation',
  'se' = 'Standard error'
)

# UI ----


ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # Application title
    introBox(
      titlePanel("Medicap"),
      data.step = 1,
      data.intro = "This is an application to explore Medicare data"
    ),

    # Sidebar
    sidebarLayout(
      sidebarPanel(

        introBox(
          actionButton("help",
                       "Press for instructions",
                       icon = icon("question"),
                       width = '95%')
        ),

        br(),

        dataSummarizerInput('summarizer_inputs',
                            input_width = '97.5%',
                            ttev_condition = ttev_condition,
                            ctns_condition = ctns_condition)

      ),

      mainPanel(

        tabsetPanel(
          type = "pills",

          tabPanel(
            "Summary data",
            br(),
            DTOutput('result_table')
          ),

          tabPanel(
            "Tabulate",
            br(),
            wellPanel(tabulate_input_list),
            gt_output('gt_table')
          )
        )


      )
    )
  )
)



# Server ----

server = function(input, output, session) {

  result <- dataSummarizerServer('summarizer_inputs',
                                 dt_racs = dt_racs,
                                 dt_stroke = dt_stroke,
                                 dt_ami = dt_ami,
                                 key_list = key_list,
                                 key_data = key_data,
                                 key_time = key_time)

  observeEvent(

    result(), {

      output$result_table <- DT::renderDataTable(result())

      choices_stat <- names(result()) |>
        recode(!!!stat_recoder, .default = NA_character_) |>
        na.omit()

      if(input$statistic %!in% choices_stat || is_empty(input$statistic)){

        updatePickerInput(
          session = session,
          inputId = 'statistic',
          choices = choices_stat,
          selected = character(0)
        )

      }



    }

  )

  output$gt_table <-
    render_gt({

      .stat <- enframe(stat_recoder) |>
        filter(value == input$statistic) |>
        slice(1) |>
        pull(name)

      .gt_cols <- c(key_time, .stat)

      .exposure <- .group <- "None"

      .year <- setdiff(levels(result()[[key_time]]), 'Overall')

      if(is_used(input$`summarizer_inputs-exposure`)){

        .exposure <- input$`summarizer_inputs-exposure`
        .gt_cols <- c(.gt_cols, .exposure)

      }

      if(is_used(input$`summarizer_inputs-group`)){

        .group <- input$`summarizer_inputs-group`
        .gt_cols <- c(.gt_cols, .group)

      }

      data_gt <- result()

      if(.stat == 'cuminc'){

        .by <- setdiff(.gt_cols, .stat)

        data_gt <- data_gt[
          result()[, .I[which.max(time)], by = .by]$V1
        ]

      }

      if(.stat %in% c('prevalence', 'cuminc')){
        data_gt[, stat := stat * 100, env = list(stat = .stat)]
      }

      data_gt <- data_gt |>
        select(any_of(.gt_cols)) |>
        mutate(across(all_of(.stat), table_value))


      dcast_lhs_variables <- c("1")

      if(is_used(.exposure)){
        dcast_lhs_variables <- c(dcast_lhs_variables, .exposure)
      }

      if(is_used(.group)){
        dcast_lhs_variables <- c(dcast_lhs_variables, .group)
      }

      dcast_formula_lhs <- paste(dcast_lhs_variables, collapse = ' + ')

      dcast_formula <- glue("{dcast_formula_lhs} ~ {key_time}")

      data_gt <- data_gt |>
        dcast(formula = as.formula(dcast_formula),
              value.var = .stat)

      if('.' %in% names(data_gt)) data_gt[['.']] <- NULL

      gt_args <- list(data = data_gt)

      if(is_used(.exposure)) gt_args$rowname_col = .exposure
      if(is_used(.group)) gt_args$groupname_col = .group

      gt_out <- do.call(gt, args = gt_args)

      if(is_used(.exposure)){
        gt_out <- gt_out |>
          tab_stubhead(label = key_list[[.exposure]]$label)
      }

      gt_out <- gt_out |>
        tab_spanner(label = 'Calendar year', columns = all_of(.year))

      gt_out |>
        cols_align('center')

    }) |>
    bindEvent(input$do_tabulate)



  observeEvent(
    input$help, {

      if(is.null(input$dataset)){
        updatePickerInput(
          session = session,
          inputId = 'dataset',
          selected = 'racs'
        )
      }

      introjs(session,
              options = list("nextLabel"="Next",
                             "prevLabel"="Previous",
                             "skipLabel"="Skip"))

    }
  )

}

shinyApp(ui = ui, server = server)



