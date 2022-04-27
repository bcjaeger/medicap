
library(shinyWidgets)

tabulate_input_list <- list(
  pickerInput(
    inputId = 'statistic',
    label = 'Select a statistic to tabulate',
    choices = c(),
    selected = NULL,
    multiple = TRUE,
    options = pickerOptions(maxOptions = 1),
    width = "97%"
  ),
  conditionalPanel(
    condition = "input.statistic.length > 0",
    actionButton(
      inputId =  "do_tabulate",
      label = "Tabulate",
      width = '94.5%',
      icon = icon("cog"),
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  ),
  conditionalPanel(
    condition = 'input.statistic.length == 0',
    actionButton(
      inputId =  "wont_do_tabulate",
      label = "Tabulate",
      width = '94.5%',
      icon = icon("cog"),
      style = "color: #fff; background-color: #808080; border-color: #2e6da4"
    )
  ),
  br(),
  uiOutput('dl_table')
)



