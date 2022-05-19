#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Initialize ----

# labels: RACS = Recent acute coronary syndrome
#         AMI = Acute myocardial infarction

help_intro_init <- c(
  "The data you select here will be summarized in the main window" = "box_dataset",
  "Beneficiaries with index dates in the years you select will be included in your analysis." = "box_year",
  "The 'outcome' will be summarized in your results" = "box_outcome",
  "Summarized values of the outcome will be presented overall and among groups defined by the 'exposure' variable" = "box_exposure",
  "You may restrict the analysis to subsets of beneficiaries defined by the 'subset' variable" = "box_subset_variable",
  "You may stratify results based on the 'group' variable." = "box_group",
  "When you are ready to compute your results, click here! If the button is blue, all the required inputs have been filled in. If it is grey, at least one required input is currently unspecified." = "box_do_computation"
)

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

  # Note bcjaeger uses O, ligongc uses Z

  peer_drive <- if(dir.exists('O:/users/Ligong')) "O:" else "Z:"

  path_to_data <-
    file.path(peer_drive, "Users", "Ligong", "Shiny app", "shiny")

  if(!exists('dt_racs'))
    dt_racs <- load_dt(path_to_data, 'cohort_RACS_preprocessed.csv')

  if(!exists('dt_ami'))
    dt_ami <- load_dt(path_to_data, 'cohort_AMI_100_preprocessed.csv')

  if(!exists('dt_stroke'))
    dt_stroke <- load_dt(path_to_data, 'cohort_stroke_preprocessed.csv')

}


# UI ----

ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # title ----
    titlePanel("Medicapp"),

    navbarPage(
      title = "Select a module",
      tabPanel(
        'Explore',
        sidebarLayout(
          sidebarPanel(
            exploreInput(
              'explore_inputs',
              input_width = '97.5%',
              ttev_condition = jsc_write_cpanel(key_data, 'ttev', 'outcome'),
              ctns_condition = jsc_write_cpanel(key_data, 'ctns', 'exposure'),
              do_compute_label = 'Explore'
            )
          ),
          mainPanel = mainPanel(
            DTOutput('explore_output')
          )
        )
      ),
      tabPanel(
        "Tabulate",
        sidebarLayout(
          sidebarPanel(
            tabulateInput(
              'tabulate_inputs',
              input_width = '97.5%',
              ttev_condition = jsc_write_cpanel(key_data, 'ttev', 'outcome'),
              ctns_condition = jsc_write_cpanel(key_data, 'ctns', 'exposure'),
              do_compute_label = 'Tabulate'
            )
          ),
          mainPanel = mainPanel(
            gt_output('tabulate_output')
          )
        )
      ),
      tabPanel(
        "Visualize",
        sidebarLayout(
          sidebarPanel(
            visualizeInput(
              'visualize_inputs',
              input_width = '97.5%',
              ttev_condition = jsc_write_cpanel(key_data, 'ttev', 'outcome'),
              ctns_condition = jsc_write_cpanel(key_data, 'ctns', 'exposure'),
              do_compute_label = 'Visualize'
            )
          ),
          mainPanel = mainPanel(
            uiOutput('visualize_output')
          )
        )
      )
    )


  )
)

# Server ----

server = function(input, output, session) {

  output$explore_output <- exploreServer(
    'explore_inputs',
    dt_racs = dt_racs,
    dt_stroke = dt_stroke,
    dt_ami = dt_ami,
    key_list = key_list,
    key_data = key_data,
    key_time = key_time,
    help_intro = help_intro_init
  )


  output$tabulate_output <- tabulateServer(
    'tabulate_inputs',
    dt_racs = dt_racs,
    dt_stroke = dt_stroke,
    dt_ami = dt_ami,
    key_list = key_list,
    key_data = key_data,
    key_time = key_time,
    help_intro = insert_element(
      help_intro_init,
      element_value = 'box_statistic',
      element_name = 'You can tabulate one statistic at a time, for now.',
      insert_before = 'box_exposure'
    )
  )

  output$visualize_output <- visualizeServer(
    'visualize_inputs',
    dt_racs = dt_racs,
    dt_stroke = dt_stroke,
    dt_ami = dt_ami,
    key_list = key_list,
    key_data = key_data,
    key_time = key_time,
    help_intro = help_intro_init |>
      insert_element(
        element_value = 'box_statistic',
        element_name = 'You can visualize one statistic at a time.',
        insert_before = 'box_exposure'
      ) |>
      insert_element(
        element_value = 'box_geom',
        element_name = "Currently we support bar charts for all outcomes and line charts for post-index outcomes. We plan to add support for boxplots, maps, and more as the app continues to develop.",
        insert_before = 'box_exposure'
      )
  )


}

shinyApp(ui = ui, server = server)



