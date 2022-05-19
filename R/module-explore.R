

exploreInput <- function(id,
                         input_width = '97.5%',
                         ttev_condition,
                         ctns_condition,
                         do_compute_label) {

  tagList(
    dataSummarizerInput(NS(id, "summarizer_inputs"),
                        input_width = input_width,
                        ttev_condition = ttev_condition,
                        ctns_condition = ctns_condition,
                        do_compute_label = do_compute_label)
  )

}

exploreServer <- function(
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

    result <- dataSummarizerServer('summarizer_inputs',
                                   dt_racs = dt_racs,
                                   dt_stroke = dt_stroke,
                                   dt_ami = dt_ami,
                                   key_list = key_list,
                                   key_data = key_data,
                                   key_time = key_time,
                                   help_intro = help_intro)

    data_tbl <- reactive({

      if(key_list[[input$`summarizer_inputs-outcome`]]$type == 'ttev'){

        result()[!is.na(ttev_inc_crude_est)]

      } else {

        result()

      }

    }) |>
      bindEvent(result())


    # browser()

    numeric_cols <- reactive(
      names(data_tbl())[sapply(data_tbl(), is_double_ish)]
    )



    data_tbl() |>
      datatable(
        options = list(
          lengthMenu = list(c(5,15,20),c('5','15','20')),
          pageLength = 10,
          columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = recode(names(result()), !!!stat_recoder)
      ) |>
      formatRound(columns = numeric_cols()) |>
      renderDataTable()

  })

}


# exploreInput <- function(id,
#                          input_width = '97.5%',
#                          ttev_condition,
#                          ctns_condition,
#                          do_compute_label) {
#
#   tagList(
#     dataSummarizerInput(NS(id, "summarizer_inputs"),
#                         input_width = input_width,
#                         ttev_condition = ttev_condition,
#                         ctns_condition = ctns_condition,
#                         do_compute_label = do_compute_label)
#   )
#
# }
#
# exploreServer <- function(
#     id,
#     dt_racs,
#     dt_ami,
#     dt_stroke,
#     key_data,
#     key_list,
#     key_time,
#     help_intro = c(
#       "A helping button" = "help",
#       "A computing button" = "box_do_computation"
#     )) {
#
#   moduleServer(id, function(input, output, session) {
#
#     result <- dataSummarizerServer('summarizer_inputs',
#                                    dt_racs = dt_racs,
#                                    dt_stroke = dt_stroke,
#                                    dt_ami = dt_ami,
#                                    key_list = key_list,
#                                    key_data = key_data,
#                                    key_time = key_time)
#
#     data_tbl <- reactive({
#
#       if(key_list[[input$`summarizer_inputs-outcome`]]$type == 'ttev'){
#
#         result()[!is.na(ttev_inc_crude_est)]
#
#       } else {
#
#         result()
#
#       }
#
#     }) |>
#       bindEvent(result())
#
#
#
#     data_tbl() |>
#       datatable(
#         options = list(
#           lengthMenu = list(c(5,15,20),c('5','15','20')),
#           pageLength = 10,
#           initComplete = JS(
#             "function(settings, json) {",
#             "$(this.api().table().header()).css({'background-color': '#337ab7', 'color': '#FFFFFF'});",
#             "}"
#           ),
#           columnDefs=list(list(className='dt-center',targets="_all"))
#         ),
#         filter = "top",
#         selection = 'multiple',
#         style = 'bootstrap',
#         class = 'cell-border stripe',
#         rownames = FALSE,
#         colnames = recode(names(result()), !!!stat_recoder)
#       ) |>
#       renderDataTable()
#
#   })
#
# }
