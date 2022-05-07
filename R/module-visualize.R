
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

      data_fig <- result()[
        time != I('Overall'), env = list(time = key_time)
      ]

      if(key_list[[.input$outcome]]$type == 'ttev'){

        .by <- c(key_time)
        if(is_used(.input$exposure)) .by <- c(.by, .input$exposure)
        if(is_used(.input$group)) .by <- c(.by, .input$group)

        data_fig <- data_fig[, .SD[.N], by = .by]

      }

      if(is_used(.input$exposure)){


        switch(
          .input$geom,
          'bars' = aes_args$fill <- .input$exposure,
          'points' = aes_args$color <- .input$exposure
        )


        data_fig <- data_fig[
          exposure != I('Overall'), env = list(exposure = .input$exposure)
        ]

      }

      n_group = 1

      if(is_used(.input$group)){

        data_fig[[.input$group]] <-
          as.character(data_fig[[.input$group]])

        data_fig <- data_fig[
          g != I('All groups'),
          env = list(g = .input$group)
        ]

        data_fig <- data_fig[
          ,
          g := paste(key_list[[.input$group]]$label, g, sep = ': '),
          env = list(g = .input$group)
        ]

        n_group <- length(unique(data_fig[[.input$group]]))

      }

      base_fig <- ggplot(data_fig) +
        do.call(aes_string, args = aes_args)

      ps <- position_identity()

      if(is_used(.input$exposure)){
        ps <- position_dodge(width = 2/3)
      }

      base_fig <- switch(
        .input$geom,

        'bars' = base_fig +
          geom_bar(position = 'dodge', stat = 'identity'),

        'points' = base_fig +
          geom_point(position = ps, size = 2.5)

      )

      base_fig <- base_fig +
        theme_bw() +
        theme(panel.grid = element_blank(),
              panel.spacing = unit(2, "lines")) +
        labs(
          y = stat_recoder[.stat],
          title = key_list[[.input$outcome]]$label
        )


      if(.stat %in% c('bnry_prevalence', 'ttev_inc_cumulative_est')){
        base_fig <- base_fig +
          scale_y_continuous(limits = c(0, 1))
      } else {
        base_fig <- base_fig +
          scale_y_continuous(
            expand = expansion(
              mult = switch(.input$geom, 'points'= 0.5, 'bars' = c(0, 0.5))
            )
          )
      }


      n_rows <- ceiling(n_group / 2)

      if(is_used(.input$group)) base_fig <- base_fig +
        facet_wrap(as.formula(glue("~{.input$group}")),
                   scales = 'free',
                   ncol = 2)

      ggplotly(base_fig,
               height = if(n_group==1) 600  else 400 * n_rows)

    }) |>
      bindEvent(input[["summarizer_inputs-do_computation"]])

  })

}



