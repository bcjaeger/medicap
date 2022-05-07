
visualizeInput <- function(id,
                           input_width = '97.5%',
                           ttev_condition,
                           ctns_condition,
                           do_compute_label) {

  ns <- NS(id)

  tagList(

    dataSummarizerInput(ns("summarizer_inputs"),
                        input_width = input_width,
                        ttev_condition = ttev_condition,
                        ctns_condition = ctns_condition,
                        do_compute_label = do_compute_label,
                        include_stat_picker = TRUE,
                        include_viz_inputs = TRUE),

  )

}

visualizeServer <- function(
  id,
  dt_racs,
  dt_ami,
  dt_stroke,
  key_data,
  key_list,
  key_time,
  help_intro
) {

  moduleServer(id, function(input, output, session) {

    result <-
      dataSummarizerServer('summarizer_inputs',
                           dt_racs = dt_racs,
                           dt_stroke = dt_stroke,
                           dt_ami = dt_ami,
                           key_list = key_list,
                           key_data = key_data,
                           key_time = key_time,
                           help_intro = help_intro,
                           include_stat_picker = TRUE)

    renderPlotly({

      .input <- reactiveValuesToList(x = input)

      names(.input) <- names(.input) |>
        str_remove(pattern = fixed('summarizer_inputs-'))


      .stat <- names(stat_recoder)[
        which(names(stat_recoder) == .input$statistic)
      ]


      aes_args <- list(x = key_time, y = .stat)

      if(is_used(.input$exposure)){
        aes_args$color <- .input$exposure
      }

      data_fig <- result()[
        time != I('Overall'), env = list(time = key_time)
      ]

      if(is_used(.input$exposure)){
        data_fig <- data_fig[
          exposure != I('Overall'), env = list(exposure = .input$exposure)
        ]
      }

      if(is_used(.input$group)){
        data_fig <- data_fig[
          group != I('All groups'), env = list(group = .input$group)
        ]
      }

      plotly_x <- as.formula(glue("~{key_time}"))
      plotly_y <- as.formula(glue("~{.stat}"))

      plot_ly(data_fig,
              x = plotly_x,
              y = plotly_y) |>
        add_bars()

    }) |>
      bindEvent(input[["summarizer_inputs-do_computation"]])

  })

}



