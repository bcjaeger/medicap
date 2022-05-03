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

  path_to_data <- file.path("Z:", "Users", "Ligong", "Shiny app", "shiny")

  dt_racs <- load_dt_racs(path_to_data)
  dt_ami <- load_dt_ami(path_to_data)
  dt_stroke <- load_dt_stroke(path_to_data)

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
              do_compute_label = 'Compute statistics'
            )
          ),
          mainPanel = mainPanel(
            gt_output('tabulate_output')
          )
        )
      ),
      tabPanel("Visualize")
    )


  )
)

# Server ----

server = function(input, output, session) {

  output$explore_output <- exploreServer('explore_inputs',
                                         dt_racs = dt_racs,
                                         dt_stroke = dt_stroke,
                                         dt_ami = dt_ami,
                                         key_list = key_list,
                                         key_data = key_data,
                                         key_time = key_time)


  output$tabulate_output <- tabulateServer('tabulate_inputs',
                                           dt_racs = dt_racs,
                                           dt_stroke = dt_stroke,
                                           dt_ami = dt_ami,
                                           key_list = key_list,
                                           key_data = key_data,
                                           key_time = key_time)


}

shinyApp(ui = ui, server = server)



