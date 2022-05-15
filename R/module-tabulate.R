
tabulateInput <- function(id,
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
                        include_tbl_inputs = TRUE),

  )

}

tabulateServer <- function(
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


    render_gt({

      .input <- reactiveValuesToList(x = input)

      names(.input) <- names(.input) |>
        str_remove(pattern = fixed('summarizer_inputs-'))


      .stat <- names(stat_recoder)[
        which(names(stat_recoder) == .input$statistic)
      ]

      .gt_cols <- c(key_time, .stat)

      .exposure <- .group <- "None"

      .year <- setdiff(levels(result()[[key_time]]), 'Overall')

      if(is_used(.input$exposure)){

        .exposure <- .input$exposure
        .gt_cols <- c(.gt_cols, .exposure)

      }

      if(is_used(.input$group)){

        .group <- .input$group
        .gt_cols <- c(.gt_cols, .group)

      }

      data_gt <- result()

      if(key_list[[.input$outcome]]$type == 'ttev'){

        .by <- setdiff(.gt_cols, .stat)

        data_gt <- data_gt[, .SD[.N], by = .by]

      }

      if(.stat %in% c('bnry_prevalence', 'ttev_inc_cumulative_est')){
        data_gt[, stat := stat * 100, env = list(stat = .stat)]
      }

      domain <- range(data_gt[[.stat]], na.rm = TRUE)

      data_gt <- data_gt |>
        select(any_of(.gt_cols))
      # |>
      #   mutate(across(all_of(.stat), table_value))


      dcast_lhs_variables <- c("1")

      if(is_used(.exposure)){
        dcast_lhs_variables <- c(dcast_lhs_variables, .exposure)
      }

      if(is_used(.group)){

        dcast_lhs_variables <- c(dcast_lhs_variables, .group)

        data_gt[
          ,
          group := fifelse(test = group == 'All groups',
                           yes = as.character(group),
                           no = paste(key_list[[.group]]$label,
                                      group, sep = ': ')),
          env = list(group = .group)
        ]

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

      cols <- setdiff(names(gt_args$data), c(.exposure, .group))

      gt_out <- gt_out |>
        tab_spanner(label = 'Calendar year', columns = all_of(.year))

      if(is_used(.input$tbl_color)){
        gt_out <- gt_out |>
          data_color(
            columns = all_of(cols),
            colors = scales::col_numeric(
              palette = c(
                "white",
                switch(.input$tbl_color,
                       'Blue' = "#337ab7",
                       "Earthy" = "bisque3")
              ),
              domain = domain
            )
          )
      }

      gt_out |>
        cols_align('center') |>
        text_transform(
          locations = cells_body(columns = all_of(cols)),
          fn = function(x) table_value(as.numeric(x))
        )



    }) |>
      bindEvent(input[["summarizer_inputs-do_computation"]])

  })

}



