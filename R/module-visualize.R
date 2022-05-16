
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

    renderUI({

      .input <- reactiveValuesToList(x = input)

      names(.input) <- names(.input) |>
        str_remove(pattern = fixed('summarizer_inputs-'))

      .stat_type <- str_sub(.input$statistic, start = 1, end = 4)

      .stat_indx <- str_detect(names(stat_recoder), .stat_type)

      .stat_options <- stat_recoder[.stat_indx]

      .stat_selected <- .stat_options[
        which(names(.stat_options) == .input$statistic)
      ]

      .stat_lab <- as.character(.stat_selected)
      .stat_col <- names(.stat_selected)

      switch(
        .input$pool,

        'pool' = {
          data_fig <- result()[
            time == I('Overall'), env = list(time = key_time)
          ] |>
            droplevels()
        },

        'stratify' = {
          data_fig <- result()[
            time != I('Overall'), env = list(time = key_time)
          ] |>
            droplevels()
        },

        'everything' = {
          data_fig <- result()
        }

      )

      stat_mult_by <- 1
      stat_append <- ''
      dcast_time <- key_time

      if(.input$statistic %in% c('bnry_prevalence',
                                 'ttev_inc_cumulative_est')){
        stat_mult_by <- 100
        stat_append <- '%'
      }

      data_fig <- data_fig[,
                           outcome := outcome * stat_mult_by,
                           env = list(outcome = .stat_col)]

      data_fig <- data_fig[,
                           label := paste0(table_value(outcome), stat_append),
                           env = list(outcome = .stat_col)]


      if(key_list[[.input$outcome]]$type == 'ttev' &&
         .input$geom == 'bar'){

        .by <- c(key_time)
        if(is_used(.input$exposure)) .by <- c(.by, .input$exposure)
        if(is_used(.input$group)) .by <- c(.by, .input$group)

        data_fig <- data_fig[, .SD[.N], by = .by]

      }

      y_cols <- .stat_col
      lab_cols <- "label"
      exposure_levels <- NULL
      legend_args <- list()
      y_min_max <- range(data_fig[[.input$statistic]])

      add_year_to_exposure <- FALSE

      if(!is_used(.input$group)){

        .input$group <- '._.fake._.group._.'
        data_fig[[.input$group]] <- 1

      } else {

        data_fig <- data_fig[
          group != 'All groups',
          env = list(group = .input$group)
        ] |>
          droplevels()

      }

      y_min_max <- range(data_fig[[.input$statistic]])

      data_split <- split(data_fig, data_fig[[.input$group]])

      plots <- vector(mode = 'list', length = length(data_split))

      if(.input$geom == 'line' &&
         .input$statistic == 'ttev_inc_cumulative_est' &&
         !is_used(.input$exposure)){

        if(.input$pool != 'pool'){
          .input$exposure <- key_time
        } else {
          dcast_time <- 'ttev_time'
        }

      }



      for(i in seq_along(plots)){

        if(is_used(.input$exposure)){

          data_split[[i]] <- data_split[[i]][
            exposure != I("Overall"),
            env = list(exposure = .input$exposure)
          ]

          # needs to be re-calculated b/c the overall group
          # shouldn't be taken into account

          if(.input$geom == 'line' &&
             .input$statistic == 'ttev_inc_cumulative_est'){

            add_year_to_exposure <- .input$pool != 'pool' &&
              .input$exposure != key_time

            dcast_time <- 'ttev_time'

            dcast_formula <- as.formula(
              paste(dcast_time, .input$exposure, sep = ' ~ ')
            )

            data_split[[i]] <- data_split[[i]][
              ,
              .SD[.N], by = .(time, exposure),
              env = list(exposure = .input$exposure,
                         time = dcast_time)
            ]

            if(.input$pool != 'pool' && .input$exposure != key_time){
              data_split[[i]][
                ,
                exposure := paste(exposure, key_time, sep = ', '),
                env = list(exposure = .input$exposure,
                           key_time = key_time)
              ]
            }

            data_split[[i]] <- data_split[[i]] |>
              dcast(formula = dcast_formula,
                    value.var = c(names(.stat_options), "label"))

            data_split[[i]] <- data_split[[i]][
              , lapply(.SD, zoo::na.locf, na.rm = FALSE)
            ]

          } else {

            dcast_formula <- as.formula(
              paste(dcast_time, .input$exposure, sep = ' ~ ')
            )

            data_split[[i]] <- data_split[[i]] |>
              dcast(formula = dcast_formula,
                    value.var = c(names(.stat_options), "label"))

          }

          exposure_levels <- result() |>
            getElement(.input$exposure)|>
            droplevels() |>
            levels() |>
            setdiff('Overall')

          if(add_year_to_exposure && .input$exposure != key_time){

            exposure_levels <-
              expand.grid(exposure_levels,
                          .input$year) |>
              glue_data("{Var1}, {Var2}") |>
              as.character()

          }

          y_cols <- paste(.stat_col,
                          exposure_levels,
                          sep = '_')

          lab_cols <- paste("label",
                            exposure_levels,
                            sep = '_')

          legend_title <- key_list[[.input$exposure]]$label |>
            str_replace_all(" ", "\n")

          legend_args$title <- list(
            text = glue("<b>{legend_title}</b>")
          )

        }

        fig <- plot_ly(data = data_split[[i]],
                       height = 500)

        for(j in seq_along(y_cols)){

          hover_cols <- paste(
            names(.stat_options),
            exposure_levels[j],
            sep = ifelse(is_used(.input$exposure), '_', '')
          )

          hover_labs <- as.character(.stat_options)

          hover_text <- table_glue(
            "{hover_labs[1]}: {data_split[[i]][[hover_cols[1]]]}"
          )

          for(k in seq(2, length(hover_labs))){
            hover_text <-
              paste(
                hover_text,
                table_glue(
                  "{hover_labs[k]}: {data_split[[i]][[hover_cols[k]]]}"
                ),
                sep = '<br>'
              )
          }

          if(is_used(.input$exposure)){
            hover_text <- paste0(
              "<b>",
              exposure_levels[j],
              "</b>",
              "<br>",
              hover_text
            )
          }


          if(.input$geom == 'line'){

            # shouldn't try to add data if there are none.
            if(!is.null(data_split[[i]][[dcast_time]]) &&
               !is.null(data_split[[i]][[y_cols[j]]])){

              fig <- try(
                fig |>
                  add_lines(
                    x = data_split[[i]][[dcast_time]],
                    y = data_split[[i]][[y_cols[j]]],
                    text = data_split[[i]][[lab_cols[j]]],
                    textposition = 'top middle',
                    name = exposure_levels[j],
                    hoverinfo = 'text',
                    hovertext = hover_text
                  )
              )

            }



            if(is_try_error(fig)) browser()

          }

          if(.input$geom == 'bar'){
            fig <- fig |>
              add_trace(
                type = .input$geom,
                x = data_split[[i]][[dcast_time]],
                y = data_split[[i]][[y_cols[j]]],
                text = data_split[[i]][[lab_cols[j]]],
                textposition = 'top middle',
                name = exposure_levels[j],
                hoverinfo = 'text',
                hovertext = hover_text
              )
          }



        }


        yplot_min <- NULL
        yplot_max <- NULL

        if(.input$statistic %in% c('bnry_prevalence',
                                   'ttev_inc_cumulative_est')){
          yplot_min <- 0
          yplot_max <- 101 # so you can see the grid at 100

        } else {

          # if(.input$geom == 'scatter'){
          #   y_range <- diff(y_min_max)
          #   yplot_min <- y_min_max[1] - y_range * 0.15
          #   yplot_max <- y_min_max[2] + y_range * 0.15
          # }

          if(.input$geom == 'bar'){
            yplot_min <- 0
            yplot_max <- y_min_max[2] * 1.15
          }

        }

        if(!is.null(key_list[[.input$group]])){

          fig_title <- paste0(key_list[[.input$outcome]]$label,
                              "; ",
                              key_list[[.input$group]]$label,
                              "=",
                              names(data_split)[i])

        } else {

          fig_title <- key_list[[.input$outcome]]$label

        }

        plots[[i]] <- fig |>
          layout(
            title = list(
              text = fig_title,
              x = 0.1
            ),
            font = list(size = 16),
            barmode = 'group',
            margin = list(
              t = 100,
              b = 100,
              l = 50,
              r = 50
            ),
            xaxis = list(
              title = if(dcast_time == key_time)
                key_list[[key_time]]$label
              else if(dcast_time == 'ttev_time')
                "Days since index date"
            ),
            yaxis = list(title = .stat_lab,
                         range = c(yplot_min, yplot_max)),
            legend = legend_args
          )

      }


      do.call(tagList, plots)

    }) |>
      bindEvent(input[["summarizer_inputs-do_computation"]])

  })

}


# if(is_used(.input$exposure)){
#
#   data_fig <- data_fig[
#     exposure != I('Overall'), env = list(exposure = .input$exposure)
#   ]
#
#   switch(
#     .input$geom,
#
#     'bars' = {
#
#       aes_args$fill <- .input$exposure
#
#       if(.input$showtext) aes_args$color <- .input$exposure
#
#       lab_args$fill <- key_list[[.input$exposure]]$label |>
#         str_replace_all(" ", "\n")
#
#     },
#
#     'points' = {
#
#       aes_args$color <- .input$exposure
#       aes_args$group <- "plot_line_group"
#       lab_args$color <- key_list[[.input$exposure]]$label |>
#         str_replace_all(" ", "\n")
#
#       data_fig[
#         ,
#         plot_line_group := fifelse(
#           test = time == 'Overall',
#           yes = paste(exposure, 'overall', sep = '-'),
#           no = as.character(exposure)
#         ),
#         env = list(exposure = .input$exposure,
#                    time = key_time)
#       ]
#
#     }
#   )
#
# } else if (.input$geom == 'points'){
#
#   aes_args$group <- "plot_line_group"
#   data_fig[
#     ,
#     plot_line_group := fifelse(
#       test = time == 'Overall',
#       yes = 0,
#       no = 1
#     ),
#     env = list(time = key_time)
#   ]
#
# }
#
# n_group = 1
#
# if(is_used(.input$group)){
#
#   data_fig[[.input$group]] <-
#     as.character(data_fig[[.input$group]])
#
#   data_fig <- data_fig[
#     g != I('All groups'),
#     env = list(g = .input$group)
#   ]
#
#   data_fig <- data_fig[
#     ,
#     g := paste(key_list[[.input$group]]$label, g, sep = ': '),
#     env = list(g = .input$group)
#   ]
#
#   n_group <- length(unique(data_fig[[.input$group]]))
#
# }
#
# if(.input$showtext){
#
#   data_fig[
#     ,
#     label := table_value(outcome),
#     env = list(outcome = .stat_label)
#   ]
#
#   aes_args$label <- 'label'
#
# }
#
#
#
#
#
# base_fig <- ggplot(data_fig) +
#   do.call(aes_string, args = aes_args)
#
# ps <- position_identity()
#
# if(is_used(.input$exposure)){
#
#   n_group_exposure <- length(unique(data_fig[[.input$exposure]]))
#
#   ps <- position_dodge(width = (n_group_exposure-2) / n_group_exposure)
#
# }
#
# base_fig <- switch(
#   .input$geom,
#
#   'bars' = base_fig +
#     geom_bar(position = 'dodge', stat = 'identity'),
#
#   'points' = base_fig +
#     geom_point(position = ps, size = 2.5) +
#     geom_line(position = ps)
#
# )
#
# if(.input$showtext) base_fig <- base_fig +
#   geom_text(
#     vjust = 0,
#     check_overlap = TRUE,
#     position = if(is_used(.input$exposure)){
#       position_dodge(width = 1)
#     } else {
#       position_identity()
#     })
#
# base_fig <- base_fig +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.spacing = unit(2, "lines")) +
#   do.call(labs, args = lab_args)
#
#
#
#
# if(.stat_colname %in% c('bnry_prevalence', 'ttev_inc_cumulative_est')){
#   base_fig <- base_fig +
#     # limits include 0 and 1 in case a value is 0 or 1
#     scale_y_continuous(limits = c(-0.01, 1.01),
#                        breaks = seq(0, 1, by = 0.1),
#                        labels = scales::percent)
# } else {
#   base_fig <- base_fig +
#     scale_y_continuous(
#       expand = expansion(
#         mult = switch(.input$geom, 'points'= 0.5, 'bars' = c(0, 0.5))
#       )
#     )
# }
#
#
# # 2 plots per row or 1?
# # n_rows <- ceiling(n_group / 2)
# n_rows <- n_group
#
# if(is_used(.input$group)) base_fig <- base_fig +
#   facet_wrap(as.formula(glue("~{.input$group}")),
#              scales = 'free',
#              ncol = 1)
#
#
# out <- ggplotly(base_fig,
#                 tooltip = c(key_time, .stat_label),
#                 height = if(n_group==1) 600  else 400 * n_rows)
#
# out
