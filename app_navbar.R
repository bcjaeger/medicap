#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Initialize ----

help_intro_init <- c(
  "First, select a dataset to analyze" = "box_dataset",
  "Next, select a set of index years. Results will be presented using data from the years you select." = "box_year",
  "Pick an outcome from the list of available options. This variable will be summarized in your results" = "box_outcome",
  "We assess whether summarized values of the outcome differ across groups defined by an exposure variable. If you don't need to do this, you can select 'None'. You must pick a value for this input before accessing inputs below it!" = "box_exposure",
  "Results will be presented using subsets that you select based on this variable. If you don't need to do this, you can select 'None'. You must pick a value for this input before accessing inputs below it!" = "box_subset_variable",
  "Results will be replicated overall and in each group, separately. If you don't need this, you can select 'None' for group" = "box_group",
  "When you are ready to compute your results, click here! If the button is blue, then you are good to go. If it is grey, it means a required input has not been specified." = "box_do_computation"
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

use_fake <- TRUE

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

  if(!exists('dt_racs')) dt_racs <- load_dt_racs(path_to_data)
  if(!exists('dt_ami')) dt_ami <- load_dt_ami(path_to_data)
  if(!exists('dt_stroke')) dt_stroke <- load_dt_stroke(path_to_data)

}

# UI ----

ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # title ----
    titlePanel("Medicap"),

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
            plotlyOutput('visualize_output')
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
    help_intro = insert_element(
      help_intro_init,
      element_value = 'box_statistic',
      element_name = 'You can visualize one statistic at a time.',
      insert_before = 'box_exposure'
    )
  )


}

shinyApp(ui = ui, server = server)



