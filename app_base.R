#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Initialize ----

source("packages.R")

for(f in list.files("R/", full.names = TRUE)) source(f)

rspec <- round_spec() |>
  round_using_magnitude(digits = c(2, 1, 0), breaks = c(10, 100, Inf))

names(rspec) <- paste('table.glue.', names(rspec), sep = '')

options(rspec)

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

use_fake <- FALSE

if(use_fake){

  # using fake data: ----

  n_obs <- 1e5

  dt_list <- load_dt_fake(n_obs)
  dt_racs <- dt_list$dt_racs
  dt_ami <- dt_list$dt_ami
  dt_stroke <- dt_list$dt_stroke

} else {

  # using real data: ----

  path_to_data <- file.path("Z:", "Users", "Ligong", "Shiny app", "shiny")

  dt_racs <- load_dt_racs(path_to_data)
  dt_ami <- load_dt_ami(path_to_data)
  dt_stroke <- load_dt_stroke(path_to_data)

}

dt_racs %>%
  select(-starts_with("Post_index"), -ends_with("days"), -ends_with("dt")) %>%
  map(unique)

# UI ----

ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # title ----
    introBox(
      titlePanel("Medicap"),
      data.step = 1,
      data.intro = "This is an application to explore Medicare data"
    ),

    # sidebar ----
    sidebarLayout(
      sidebarPanel(

        introBox(
          actionButton("help",
                       "Press for instructions",
                       icon = icon("question"),
                       width = '95%')
        ),

        br(),

        dataSummarizerInput(
          'summarizer_inputs',
          input_width = '97.5%',
          ttev_condition = jsc_write_cpanel(key_data, 'ttev', 'outcome'),
          ctns_condition = jsc_write_cpanel(key_data, 'ctns', 'exposure')
        )

      ),

      mainPanel(

        # panels ----

        tabsetPanel(
          type = "pills",

          tabPanel(
            # button to navigate to this panel
            # the introbox points new users to this button
            introBox(
              "Summary data",
              data.step = 6,
              data.intro = "In the 'Summary data' section, you will see data tables printed whenever you click 'compute'."
            ),
            br(),
            # button to download data shown in this panel
            uiOutput('dl_summary_data'),
            br(),
            # output for this panel - raw data from summaries
            DTOutput('result_table'),
          ),
          tabPanel(
            # button to navigate to this panel
            # the introbox points new users to this button
            introBox(
              "Tabulate",
              data.step = 7,
              data.intro = "In the 'Tabulate' section, you can generate more readable tables of the summary data by clicking 'tabulate'."
            ),
            br(),
            # inputs for users to create a table
            wellPanel(tabulate_input_list),
            # output table with summary values requested
            gt_output('gt_table')
          )

        )
      )
    )
  )
)

# Server ----

server = function(input, output, session) {

  # creates summary data, result() is reactive
  result <- dataSummarizerServer('summarizer_inputs',
                                 dt_racs = dt_racs,
                                 dt_stroke = dt_stroke,
                                 dt_ami = dt_ami,
                                 key_list = key_list,
                                 key_data = key_data,
                                 key_time = key_time)

  # output summary data ----
  # whenever a new result is computed, this triggers
  observeEvent(

    result(), {

      # light up the download summary data button
      output$dl_summary_data <- renderUI(
        downloadButton("dl_summary_data_active",
                       label = "Download these summary data",
                       style = "width:100%;")
      )

      # handle the download if user clicks
      output$dl_summary_data_active <- downloadHandler(
        filename = function() {
          paste('summary_data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write_csv(result(), con)
        }
      )

      # DataTable output of result(), no modifications needed
      output$result_table <- renderDataTable(result())

      # the statistics that a user can tabulate are updated to
      # reflect the statistic names in the new result() object
      choices_stat <- names(result()) |>
        recode(!!!stat_recoder, .default = NA_character_) |>
        na.omit()

      # but the updated choices are only applied if they need to be.
      # if there is no current statistic selected -> update
      # if the current statistic is not one of the new choices -> update
      # TODO: make statistic names (not labels) unique based on outcome type
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

  # generate summary table ----
  # whenever the tabulate button is pushed, tbl() is updated
  tbl <- reactive({

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

        if(key_list[[input$`summarizer_inputs-outcome`]]$type == 'ttev'){

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

  # output summary table ----
  # whenever tbl() is updated, this triggers.
  observeEvent(

    tbl(), {

      output$gt_table <- render_gt(tbl())

      output$dl_table <- renderUI(
        downloadButton("dl_table_active",
                       label = "Download this table",
                       style = "width:95%;")
      )

      output$dl_table_active <- downloadHandler(
        filename = function() {
          paste('summary_table-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          gtsave(tbl(), con)
        }
      )

    }

  )

  # instructions ----
  # when user asks for instructions, this triggers
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



