
## Not currently used
#
# computeButtonUI <- function(id, data.step) {
#
#   ns <- NS(id)
#
#   compute_ready <-
#     "(
#        input.year.length > 0 &
#        input.outcome.length > 0 &
#        (input.subset_variable.length == 0 | input.subset_variable == 'None')
#      )
#   |
#     (
#        input.year.length > 0 &
#        input.outcome.length > 0 &
#        input.subset_value.length > 0 &
#        input.subset_variable.length > 0
#      )"
#
#   introBox(
#     tagList(
#       conditionalPanel(
#         condition = compute_ready,
#         ns = ns,
#         actionButton(
#           inputId =  ns("do_computation"),
#           label = "Compute my results",
#           width = '95%',
#           icon = icon("cog"),
#           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
#         )
#       ),
#
#       conditionalPanel(
#         condition = paste("!(", compute_ready, ")", sep = ''),
#         ns = ns,
#         actionButton(
#           inputId =  ns("wont_do_computation"),
#           label = "Compute my results",
#           width = '95%',
#           icon = icon("cog"),
#           style = "color: #fff; background-color: #808080; border-color: #2e6da4"
#         )
#       )
#     ),
#     data.step = data.step,
#     data.intro = paste(
#       "When you have selected a dataset, an outcome",
#       "and at least one index year, this button will turn blue,",
#       "and clicking it will generate results in the main panel.",
#       "If the button is grey, it means at least one required input",
#       "is currently unspecified."
#     )
#   )
#
#
#
#
# }
#
# computeButtonServer <- function(id) {
#
#   moduleServer(
#     id,
#     function(input, output, session) {
#
#       reactive(input$do_computation)
#
#     }
#   )
#
# }
