
dataSummarizerInput <- function(
    id,
    input_width = '97.5%',
    ttev_condition,
    ctns_condition,
    do_compute_label,
    include_stat_picker = FALSE,
    include_viz_inputs = FALSE
) {

  ns <- NS(id)

  stat_picker_condition <- tolower(as.character(include_stat_picker))
  viz_input_condition <- tolower(as.character(include_viz_inputs))

  stat_picker_compute_addon <- switch(stat_picker_condition,
                                      'true' = 'input.statistic.length > 0 &',
                                      'false' = '')

  ctns_condition_parenthetical <- gsub("[\\(\\)]", "",
       regmatches(ctns_condition,
                  gregexpr("\\(.*?\\)", ctns_condition))[[1]])


  compute_ready <- glue(
    "(input.year.length > 0 &
       input.outcome.length > 0 &
       {stat_picker_compute_addon}
       (input.subset_variable.length == 0 | input.subset_variable == 'None'))
    |
    (input.year.length > 0 &
       input.outcome.length > 0 &
       input.subset_value.length > 0 &
       {stat_picker_compute_addon}
       input.subset_variable.length > 0)"
  )

  if(!is_empty(ctns_condition_parenthetical)){
    compute_ready <- glue(
      "( {compute_ready} ) &
       ( !({ctns_condition_parenthetical}) | input.n_group.length > 0 )"
    )
  }

  tagList(

    introjsUI(),

    actionButton(inputId = ns("help"),
                 "Press for instructions",
                 icon = icon("question"),
                 width = '95%'),

    HTML('<br>'), HTML('<br>'),

    div(
      id = ns("box_dataset"),
      pickerInput(
        inputId = ns('dataset'),
        label = 'Select a dataset',
        choices = c("racs"   = "racs",
                    "ami"    = "ami",
                    "stroke" = "stroke"),
        selected = 'racs',
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    div(
      id = ns("box_year"),
      prettyCheckboxGroup(
        inputId = ns("year"),
        label = "Select index year(s)",
        inline = TRUE,
        selected = NULL,
        width = input_width
      )
    ),


    actionGroupButtons(
      inputIds = ns(c('select_all_years',
                      'select_last_5',
                      'deselect_all_years')),
      labels = c("All",
                 "Last 5",
                 "None"),
      status = 'info'
    ),

    HTML('<br>'),HTML('<br>'),


    div(
      id = ns("box_outcome"),
      pickerInput(
        inputId = ns('outcome'),
        label = 'Select an outcome',
        choices = c(),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    conditionalPanel(
      condition = ttev_condition, #'input.outcome.length > 0',
      ns = ns,
      sliderInput(
        inputId = ns("horizon"),
        label = "Select a max follow-up time",
        min = 0,
        max = 360,
        step = 10,
        value = 90,
        round = TRUE,
        width = '95%'
      )
    ),

    div(
      id = ns("box_statistic"),
      conditionalPanel(
        condition = stat_picker_condition,
        ns = ns,
        pickerInput(
          inputId = ns('statistic'),
          label = 'Select a statistic to tabulate',
          choices = c(),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1),
          width = "97%"
        )
      )
    ),

    div(
      id = ns("box_exposure"),
      pickerInput(
        inputId = ns('exposure'),
        label = 'Select an exposure',
        choices = c('None'),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    conditionalPanel(
      condition = ctns_condition,
      ns = ns,
      pickerInput(
        inputId = ns('n_group'),
        label = 'Select number of groups',
        choices = c(2, 3, 4),
        selected = 3,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    div(
      id = ns("box_subset_variable"),
      pickerInput(
        inputId = ns('subset_variable'),
        label = 'Select a subsetting variable',
        choices = c('None'),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    div(
      id = ns("box_do_subset"),
      conditionalPanel(
        condition = "input.subset_variable.length > 0 &
                     input.subset_variable != 'None'",
        ns = ns,
        prettyCheckboxGroup(
          inputId = ns('subset_value'),
          label = 'Include these subsets:',
          choices = c(),
          selected = NULL,
          width = input_width
        )
      )
    ),

    div(
      id = ns("box_group"),
      pickerInput(
        inputId = ns('group'),
        label = 'Select a grouping variable',
        choices = c(),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(maxOptions = 1),
        width = input_width
      )
    ),

    div(
      id = ns("box_do_computation"),
      conditionalPanel(
        condition = compute_ready,
        ns = ns,
        actionButton(
          inputId =  ns("do_computation"),
          label = do_compute_label,
          width = '95%',
          icon = icon("cog"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),

      conditionalPanel(
        condition = paste("!(", compute_ready, ")", sep = ''),
        ns = ns,
        actionButton(
          inputId =  ns("wont_do_computation"),
          label = do_compute_label,
          width = '95%',
          icon = icon("cog"),
          style = "color: #fff; background-color: #808080; border-color: #2e6da4"
        )
      )
    ),

    conditionalPanel(
      condition = viz_input_condition,
      ns = ns,
      sliderInput(inputId = "height",
                  label = "Figure height, pixels",
                  min = 400,
                  max = 1000,
                  value = 500)
    )

  )
}

dataSummarizerServer <- function(
    id,
    dt_racs,
    dt_ami,
    dt_stroke,
    key_data,
    key_list,
    key_time,
    help_intro,
    include_stat_picker = FALSE
) {

  moduleServer(id, function(input, output, session) {

    intro <- reactive(
      data.frame(
        element = paste0(
          "#",
          session$ns(help_intro)
        ),
        intro = names(help_intro)
      )
    )

    dt <- reactive({
      switch(input$dataset,
             "racs" = dt_racs,
             "ami" = dt_ami,
             "stroke" = dt_stroke)
    })

    # maybe move this to the pre-amble? similar to the dt objects
    dt_years <- reactive({get_unique(dt()[[key_time]])})

    observeEvent(input$dataset, {

      selected <- intersect(dt_years(), input$year)
      if(is_empty(selected))
        selected <- max(dt_years())

      updatePrettyCheckboxGroup(
        inputId = 'year',
        choices = as.character(sort(dt_years())),
        selected = as.character(selected),
        inline = TRUE
      )

      outcome_inputs <- key_data |>
        filter(outcome,
               variable %in% names(dt())) |>
        select(label, variable) |>
        deframe()

      updatePickerInput(
        session = session,
        inputId = 'outcome',
        choices = outcome_inputs,
        selected = intersect(names(dt()), input$outcome)
      )

    })

    observeEvent(input$select_all_years, {
      updatePrettyCheckboxGroup(inputId = 'year',
                                selected = dt_years())
    })

    observeEvent(input$select_last_5, {

      fifth_or_last <- min(length(dt_years()), 5)

      updatePrettyCheckboxGroup(
        inputId = 'year',
        selected = sort(dt_years(), decreasing = TRUE)[seq(fifth_or_last)]
      )
    })

    observeEvent(input$deselect_all_years, {
      updatePrettyCheckboxGroup(inputId = 'year',
                                selected = character(0))
    })

    # the is_empty event has to come before the regular event.
    # Otherwise, the downstream inputs never show up. Why?
    observeEvent(is_empty(input$outcome), {

      updatePickerInput(
        session = session,
        inputId = 'exposure',
        choices = character(0),
        selected = character(0)
      )

      updatePickerInput(
        session = session,
        inputId = 'statistic',
        choices = character(0),
        selected = character(0)
      )

      updatePickerInput(
        session = session,
        inputId = 'subset_variable',
        choices = character(0),
        selected = character(0)
      )

      updatePickerInput(
        session = session,
        inputId = 'group',
        choices = character(0),
        selected = character(0)
      )

    })

    observeEvent(input$outcome, {

      if(include_stat_picker){

        # the statistics for tabulation are updated based on the outcome
        stat_inputs <- enframe(stat_recoder) |>
          filter(
            str_detect(
              string = name,
              pattern = paste0("^", key_list[[input$outcome]]$type)
            )
          ) |>
          relocate(name, .after = value) |>
          deframe()

        # but the updated choices are only applied if they need to be.
        # if there is no current statistic selected -> update
        # if the current statistic is not one of the new choices -> update
        # TODO: make statistic names (not labels) unique based on outcome type
        if(input$statistic %!in% stat_inputs || is_empty(input$statistic)){

          updatePickerInput(
            session = session,
            inputId = 'statistic',
            choices = stat_inputs,
            selected = character(0)
          )

        }


      } else {

        exposure_inputs <- key_data |>
          filter(exposure,
                 variable %in% setdiff(names(dt()),
                                       input$outcome)) |>
          select(label, variable) |>
          deframe()

        updatePickerInput(
          session = session,
          inputId = 'exposure',
          choices = c("None", exposure_inputs),
          selected = setdiff(input$exposure, input$outcome)
        )

        if(!is.null(input$subset_variable)){

          subset_inputs <- key_data |>
            filter(subset,
                   variable %in% setdiff(names(dt()), c(input$outcome))) |>
            select(label, variable) |>
            deframe()

          subset_selected <- setdiff(input$subset_variable, c(input$outcome))

          updatePickerInput(
            session = session,
            inputId = 'subset_variable',
            choices = c("None", subset_inputs),
            selected = subset_selected
          )

        }

        if(!is.null(input$group)){

          group_inputs <- key_data |>
            filter(group,
                   variable %in% setdiff(names(dt()),
                                         c(input$outcome,
                                           input$exposure))) |>
            select(label, variable) |>
            deframe()

          group_selected <- setdiff(input$group,
                                    c(input$outcome,
                                      input$exposure))

          updatePickerInput(
            session = session,
            inputId = 'group',
            choices = c("None", group_inputs),
            selected = group_selected
          )

        }

      }

    })

    observeEvent(input$statistic, {

      if(include_stat_picker){

        exposure_inputs <- key_data |>
          filter(exposure,
                 variable %in% setdiff(names(dt()),
                                       input$outcome)) |>
          select(label, variable) |>
          deframe()

        updatePickerInput(
          session = session,
          inputId = 'exposure',
          choices = c("None", exposure_inputs),
          selected = setdiff(input$exposure, input$outcome)
        )

        if(!is.null(input$subset_variable)){

          subset_inputs <- key_data |>
            filter(subset,
                   variable %in% setdiff(names(dt()), c(input$outcome))) |>
            select(label, variable) |>
            deframe()

          subset_selected <- setdiff(input$subset_variable, c(input$outcome))

          updatePickerInput(
            session = session,
            inputId = 'subset_variable',
            choices = c("None", subset_inputs),
            selected = subset_selected
          )

        }

        if(!is.null(input$group)){

          group_inputs <- key_data |>
            filter(group,
                   variable %in% setdiff(names(dt()),
                                         c(input$outcome,
                                           input$exposure))) |>
            select(label, variable) |>
            deframe()

          group_selected <- setdiff(input$group,
                                    c(input$outcome,
                                      input$exposure))

          updatePickerInput(
            session = session,
            inputId = 'group',
            choices = c("None", group_inputs),
            selected = group_selected
          )

        }

      }

    })

    observeEvent(is_empty(input$exposure), {

      updatePickerInput(
        session = session,
        inputId = 'subset_variable',
        choices = character(0),
        selected = character(0)
      )

      updatePickerInput(
        session = session,
        inputId = 'group',
        choices = character(0),
        selected = character(0)
      )

    })

    observeEvent(input$exposure, {

      subset_inputs <- key_data |>
        filter(subset,
               variable %in% setdiff(names(dt()), c(input$outcome))) |>
        select(label, variable) |>
        deframe()

      subset_selected <- setdiff(input$subset_variable, c(input$outcome))

      updatePickerInput(
        session = session,
        inputId = 'subset_variable',
        choices = c("None", subset_inputs),
        selected = subset_selected
      )

      if(!is.null(input$group)){
        if(input$exposure == input$group){
          updatePickerInput(
            session = session,
            inputId = 'group',
            selected = character(0)
          )
        }
      }

    })

    observeEvent(is_empty(input$subset_variable), {

      updatePickerInput(
        session = session,
        inputId = 'group',
        choices = character(0),
        selected = character(0)
      )

    })

    observeEvent(input$subset_variable, {

      # TODO: if subset_variable doesn't change, neither should subset values.
      updatePrettyCheckboxGroup(
        inputId = 'subset_value',
        choices = get_unique(dt()[[input$subset_variable]]),
        selected = character(0) #subset_value_selected
      )

      # input$subset_variable, include or no?

      group_inputs <- key_data |>
        filter(group,
               variable %in% setdiff(names(dt()),
                                     c(input$outcome,
                                       input$exposure))) |>
        select(label, variable) |>
        deframe()

      group_selected <- setdiff(input$group,
                                c(input$outcome,
                                  input$exposure))

      updatePickerInput(
        session = session,
        inputId = 'group',
        choices = c("None", group_inputs),
        selected = group_selected
      )


    })

    observeEvent(input$help, {

        introjs(session,
                options = list(nextLabel ="Next",
                               prevLabel ="Previous",
                               skipLabel ="Skip",
                               steps = intro()))

      })

    result <- reactive({

      dt_list <- reactiveValuesToList(input)[c('year',
                                               'outcome',
                                               'exposure',
                                               'subset_variable',
                                               'subset_value',
                                               'group',
                                               'n_group',
                                               'horizon')]

      # -- subsetting --

      # data.table subset requires the values be wrapped in I()
      dt_list$year <- I(as.numeric(dt_list$year))

      dt_subset <- glue("{key_time} %in% year")

      if(is_used(dt_list$subset_variable)){

        dt_list$subset_value <- I(dt_list$subset_value)

        dt_subset <- paste(dt_subset,
                           "subset_variable %in% subset_value",
                           sep = ' & ')
      }

      # only subset the data once, without copying any data
      dt_subsetted <- glue("dt()[{dt_subset}, env = dt_list]") |>
        parse_expr() |>
        eval()

      # -- transforming --

      # the exposure needs to be categorized if it is continuous.
      # Normally I'd keep it continuous but this is about simple
      # descriptive tables and figures, not a statistics seminar.

      # if(input$n_group == "4") browser()

      # browser()

      if(is_used(input$exposure)){

        if(key_list[[input$exposure]]$type == 'ctns'){

          dt_subsetted[,
                       exposure := cut_percentile(
                         exposure,
                         dt_list$n_group
                       ),
                       env = dt_list['exposure']]

          # modifying the subset by reference doesn't modify the parent
          # so this transform does not change values in the original dt.

        }

      }



      # -- summarizing --

      dt_list$f <- switch(key_list[[dt_list$outcome]]$type,
                          'intg' = smry_ctns,
                          'ordn' = smry_ctns,
                          'ctns' = smry_ctns,
                          'bnry' = smry_bnry,
                          'ttev' = smry_ttev,
                          stop("not yet supported"))

      dt_make <- ".(outcome = list(f(outcome)))"

      # if outcome is time-to-event:
      if(key_list[[dt_list$outcome]]$type == 'ttev'){

        dt_list$time <- key_list[[dt_list$outcome]]$time
        dt_make <- ".(outcome = list(f(outcome, time, horizon)))"

      }

      # -- grouping --
      dt_group_vars <- c(key_time)

      if(is_used(dt_list$exposure))
        dt_group_vars <- c(dt_group_vars, dt_list$exposure)

      if(is_used(dt_list$group))
        dt_group_vars <- c(dt_group_vars, dt_list$group)


      # the blank value entered here will be the overall summary (no groups)
      dt_grp_calls <- list("")

      # grouping by a single variable
      for(i in seq_along(dt_group_vars)){
        dt_grp_calls <- c(
          dt_grp_calls,
          glue::glue(
            "keyby = .({dt_group_vars[i]})"
          )
        )
      }

      # grouping by all the grouping variables
      if(length(dt_group_vars) > 1){
        dt_grp_calls <- c(
          dt_grp_calls,
          glue::glue(
            "keyby = .({paste(dt_group_vars, collapse = ', ')})"
          )
        )
      }

      # grouping by all combinations of pairs of variables
      # (only do this if you have 3 grouping variables)
      if(length(dt_group_vars) > 2){

        duos <- combn(dt_group_vars, m = 2)

        for(i in seq(ncol(duos))){
          dt_grp_calls <- c(
            dt_grp_calls,
            # group by the exposure / grouping variable
            glue::glue(
              "keyby = .({paste(duos[, i], collapse = ', ')})"
            )
          )
        }
      }


      #
      # if(!is_empty(dt_groups)){
      #
      #   dt_grp_calls <- c(
      #     dt_grp_calls,
      #     # group by the exposure / grouping variable
      #     glue::glue(
      #       "keyby = .({paste(dt_groups, collapse = ', ')})"
      #     ),
      #     # group by time
      #     glue::glue(
      #       "keyby = .({key_time})"
      #     )
      #   )
      # }
      #
      # dt_grp_calls <- c(
      #   dt_grp_calls,
      #   glue::glue(
      #     "keyby = .({paste(dt_group_vars, collapse = ', ')})"
      #   )
      # )


      # -- evaluate expressions and return data.table outputs --

      dt_output <- dt_grp_calls |>
        lapply(
          function(dt_group){
            eval(
              parse_expr(
                glue(
                  "dt_subsetted[, {dt_make}, {dt_group}, env = dt_list] |>
                dt_unnest({dt_list$outcome})"
                )
              )
            )
          }
        )

      dt_groups <- setdiff(dt_group_vars, key_time)

      for(i in seq_along(dt_output)){

        if(!key_time %in% names(dt_output[[i]]))
          set(dt_output[[i]], j = key_time, value = -Inf)

        if(is_used(dt_list$exposure) &&
           dt_list$exposure %!in% names(dt_output[[i]]))
          set(dt_output[[i]], j = dt_list$exposure, value = "Overall")

        if(is_used(dt_list$group) &&
           dt_list$group %!in% names(dt_output[[i]]))
          set(dt_output[[i]], j = dt_list$grou, value = "All groups")

      }

      out <- rbindlist(dt_output, fill = TRUE)

      out[, key_time := factor(key_time,
                               levels = c(-Inf, dt_list$year),
                               labels = c("Overall",
                                          paste(dt_list$year))),
          env = list('key_time' = key_time)]

      # set(
      #   out,
      #   j = key_time,
      #   value = factor(
      #     out[[key_time]],
      #     levels = c(-Inf, dt_list$year),
      #                labels = c("Overall", paste(dt_list$year))
      #   )
      # )

      move_to_front <- c(key_time, dt_groups)

      setcolorder(out, move_to_front)


    }) |>
      bindEvent(input$do_computation)

    result

  })

}
