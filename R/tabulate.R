
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
                        include_stat_picker = TRUE),

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
    help_intro = c(
      "A helping button" = "help",
      "A computing button" = "box_do_computation"
    )
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
      bindEvent(input[["summarizer_inputs-do_computation"]])

  })

}



