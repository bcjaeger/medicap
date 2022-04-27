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


n_obs <- 1e5

set.seed(329)

path_to_data <- file.path("Z:", "Users", "Ligong", "Shiny app", "shiny")

dt_racs <- fread(file.path(path_to_data, 'cohort_RACS.csv')) %>%
  rename(Year=year,
         Age = age_cat,
         Gender = bene_sex_ident_cd,
         Race = bene_race_cd,
         Pre_index_statin = bl_statin,
         Pre_index_statin_intensity = pre_potency,
         Pre_index_ezetimibe = bl_Eze,
         Pre_index_PCSK9i = bl_PCSK9i,
         Pre_index_PCSK9i_dose = bl_PCSK9i_dose,
         Pre_index_alirocumab_dose = bl_Ali_dose,
         Pre_index_alirocumab = bl_Ali,
         Pre_index_evolocumab = bl_Evo,
         Pre_index_evolocumab_dose = bl_Evo_dose,
         Pre_index_medication_number = bl_med_count,
         Pre_index_hypertension = bl_hypertsn,
         Pre_index_depression = bl_depress,
         Pre_index_diabetes = bl_diab,
         Pre_index_CKD = bl_CKD,
         Pre_index_CHD = bl_CHD,
         Pre_index_MI = bl_MI,
         Pre_index_CABG_PCI = bl_CABG,
         Pre_index_smoking = bl_smoke,
         Pre_index_cardiologist_visit = bl_cardio,
         Pre_index_HF = bl_HF,
         Pre_index_LEAD = bl_lead,
         Pre_index_ischemic_stroke = bl_Isch_stroke,
         Pre_index_ASCVD = bl_ASCVD,
         Post_index_statin_intensity = fu_potency,
         Post_index_statin = fu_statin,
         Post_index_statin_days = fu_statin_days,
         Post_index_high_intensity_statin = fu_high_statin,
         Post_index_high_intensity_statin_days = fu_high_statin_days,
         Post_index_ezetimibe = fu_Eze,
         Post_index_ezetimibe_days = fu_Eze_days,
         Post_index_PCSK9_inhibitor = fu_PCSK9i,
         Post_index_PCSK9_inhibitor_days = fu_PCSK9i_days,
         Post_index_cardiac_rehab_number = fu_cardiac_rehab_count,
         Post_index_cardiac_rehab = fu_cardiac_rehab,
         Post_index_cardiologist_visit = fu_cardio,
         Post_index_cardiologist_visit_days = fu_cardio_days,
         Post_index_neurologist_visit = fu_neuro,
         Post_index_neurologist_visit_days = fu_neuro_days,
         Post_index_outpatient_visit = fu_outpatient_visit,
         Post_index_outpatient_visit_days = fu_outpatient_visit_days,
         Post_index_any_hospitalization = fu_any_hosp,
         Post_index_any_hospitalization_days = fu_any_hosp_days,
         Post_index_CHD_hospitalization = fu_CHD,
         Post_index_CHD_hospitalization_days = fu_CHD_days,
         Post_index_MI_hospitalization = fu_MI,
         Post_index_MI_hospitalization_days = fu_MI_days,
         Post_index_HF_hospitalization = fu_HF,
         Post_index_HF_hospitalization_days = fu_HF_days,
         Post_index_ischemic_stroke_hospitalization = fu_Isch_stroke,
         Post_index_ischemic_stroke_hospitalization_days = fu_Isch_stroke_days
  ) %>%
  mutate(
    Age=dplyr::recode(Age, "1"="65-69",
                      "2"="70-74",
                      "3"="75-79",
                      "4"="80-84",
                      "5"="85+"),
    Gender=dplyr::recode(Gender, "1"="Male",
                         "2"="Female"),
    Race=dplyr::recode(Race,
                       "0"="Unknown",
                       "1"="White",
                       "2"="Black",
                       "3"="Other",
                       "4"="Asian",
                       "5"="Hispanic",
                       "6"="Native"
    ),
    Pre_index_statin=dplyr::recode(Pre_index_statin,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_statin_intensity=dplyr::recode(Pre_index_statin_intensity,
                                             "3"="High",
                                             "2"="Moderate",
                                             "1"="Low"),
    Pre_index_statin_intensity=factor(Pre_index_statin_intensity,
                                      levels=c("Low", "Moderate","High")),
    Pre_index_ezetimibe=dplyr::recode(Pre_index_ezetimibe,
                                      "0"="No",
                                      "1"="Yes"),
    Pre_index_PCSK9i=dplyr::recode(Pre_index_PCSK9i,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_alirocumab=dplyr::recode(Pre_index_alirocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_evolocumab=dplyr::recode(Pre_index_evolocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_hypertension=dplyr::recode(Pre_index_hypertension,
                                         "0"="No",
                                         "1"="Yes"),
    Pre_index_depression=dplyr::recode(Pre_index_depression,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_diabetes=dplyr::recode(Pre_index_diabetes,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_CKD=dplyr::recode(Pre_index_CKD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_CHD=dplyr::recode(Pre_index_CHD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_MI=dplyr::recode(Pre_index_MI,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_CABG_PCI=dplyr::recode(Pre_index_CABG_PCI,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_smoking=dplyr::recode(Pre_index_smoking,
                                    "0"="No",
                                    "1"="Yes"),
    Pre_index_cardiologist_visit=dplyr::recode(Pre_index_cardiologist_visit,
                                               "0"="No",
                                               "1"="Yes"),
    Pre_index_HF=dplyr::recode(Pre_index_HF,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_LEAD=dplyr::recode(Pre_index_LEAD,
                                 "0"="No",
                                 "1"="Yes"),
    Pre_index_ischemic_stroke=dplyr::recode(Pre_index_ischemic_stroke,
                                            "0"="No",
                                            "1"="Yes"),
    Pre_index_ASCVD=dplyr::recode(Pre_index_ASCVD,
                                  "0"="No",
                                  "1"="Yes"),
    Post_index_cardiac_rehab=dplyr::recode(Post_index_cardiac_rehab,
                                           "0"="No",
                                           "1"="Yes")
  )


dt_ami <- fread(file.path(path_to_data, 'cohort_AMI.csv')) %>%
  rename(Year=year,
         Age = age_cat,
         Gender = bene_sex_ident_cd,
         Race = bene_race_cd,
         Pre_index_statin = bl_statin,
         Pre_index_statin_intensity = pre_potency,
         Pre_index_ezetimibe = bl_Eze,
         Pre_index_PCSK9i = bl_PCSK9i,
         Pre_index_PCSK9i_dose = bl_PCSK9i_dose,
         Pre_index_alirocumab_dose = bl_Ali_dose,
         Pre_index_alirocumab = bl_Ali,
         Pre_index_evolocumab = bl_Evo,
         Pre_index_evolocumab_dose = bl_Evo_dose,
         Pre_index_medication_number = bl_med_count,
         Pre_index_hypertension = bl_hypertsn,
         Pre_index_depression = bl_depress,
         Pre_index_diabetes = bl_diab,
         Pre_index_CKD = bl_CKD,
         Pre_index_CHD = bl_CHD,
         Pre_index_MI = bl_MI,
         Pre_index_CABG_PCI = bl_CABG,
         Pre_index_smoking = bl_smoke,
         Pre_index_cardiologist_visit = bl_cardio,
         Pre_index_HF = bl_HF,
         Pre_index_LEAD = bl_lead,
         Pre_index_ischemic_stroke = bl_Isch_stroke,
         Pre_index_ASCVD = bl_ASCVD,
         Post_index_statin_intensity = fu_potency,
         Post_index_statin = fu_statin,
         Post_index_statin_days = fu_statin_days,
         Post_index_high_intensity_statin = fu_high_statin,
         Post_index_high_intensity_statin_days = fu_high_statin_days,
         Post_index_ezetimibe = fu_Eze,
         Post_index_ezetimibe_days = fu_Eze_days,
         Post_index_PCSK9_inhibitor = fu_PCSK9i,
         Post_index_PCSK9_inhibitor_days = fu_PCSK9i_days,
         Post_index_cardiac_rehab_number = fu_cardiac_rehab_count,
         Post_index_cardiac_rehab = fu_cardiac_rehab,
         Post_index_cardiologist_visit = fu_cardio,
         Post_index_cardiologist_visit_days = fu_cardio_days,
         Post_index_neurologist_visit = fu_neuro,
         Post_index_neurologist_visit_days = fu_neuro_days,
         Post_index_outpatient_visit = fu_outpatient_visit,
         Post_index_outpatient_visit_days = fu_outpatient_visit_days,
         Post_index_any_hospitalization = fu_any_hosp,
         Post_index_any_hospitalization_days = fu_any_hosp_days,
         Post_index_CHD_hospitalization = fu_CHD,
         Post_index_CHD_hospitalization_days = fu_CHD_days,
         Post_index_MI_hospitalization = fu_MI,
         Post_index_MI_hospitalization_days = fu_MI_days,
         Post_index_HF_hospitalization = fu_HF,
         Post_index_HF_hospitalization_days = fu_HF_days,
         Post_index_ischemic_stroke_hospitalization = fu_Isch_stroke,
         Post_index_ischemic_stroke_hospitalization_days = fu_Isch_stroke_days
  ) %>%
  mutate(
    Age=dplyr::recode(Age, "1"="65-69",
                      "2"="70-74",
                      "3"="75-79",
                      "4"="80-84",
                      "5"="85+"),
    Gender=dplyr::recode(Gender, "1"="Male",
                         "2"="Female"),
    Race=dplyr::recode(Race,
                       "0"="Unknown",
                       "1"="White",
                       "2"="Black",
                       "3"="Other",
                       "4"="Asian",
                       "5"="Hispanic",
                       "6"="Native"
    ),
    Pre_index_statin=dplyr::recode(Pre_index_statin,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_statin_intensity=dplyr::recode(Pre_index_statin_intensity,
                                             "3"="High",
                                             "2"="Moderate",
                                             "1"="Low"),
    Pre_index_statin_intensity=factor(Pre_index_statin_intensity,
                                      levels=c("Low", "Moderate","High")),
    Pre_index_ezetimibe=dplyr::recode(Pre_index_ezetimibe,
                                      "0"="No",
                                      "1"="Yes"),
    Pre_index_PCSK9i=dplyr::recode(Pre_index_PCSK9i,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_alirocumab=dplyr::recode(Pre_index_alirocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_evolocumab=dplyr::recode(Pre_index_evolocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_hypertension=dplyr::recode(Pre_index_hypertension,
                                         "0"="No",
                                         "1"="Yes"),
    Pre_index_depression=dplyr::recode(Pre_index_depression,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_diabetes=dplyr::recode(Pre_index_diabetes,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_CKD=dplyr::recode(Pre_index_CKD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_CHD=dplyr::recode(Pre_index_CHD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_MI=dplyr::recode(Pre_index_MI,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_CABG_PCI=dplyr::recode(Pre_index_CABG_PCI,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_smoking=dplyr::recode(Pre_index_smoking,
                                    "0"="No",
                                    "1"="Yes"),
    Pre_index_cardiologist_visit=dplyr::recode(Pre_index_cardiologist_visit,
                                               "0"="No",
                                               "1"="Yes"),
    Pre_index_HF=dplyr::recode(Pre_index_HF,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_LEAD=dplyr::recode(Pre_index_LEAD,
                                 "0"="No",
                                 "1"="Yes"),
    Pre_index_ischemic_stroke=dplyr::recode(Pre_index_ischemic_stroke,
                                            "0"="No",
                                            "1"="Yes"),
    Pre_index_ASCVD=dplyr::recode(Pre_index_ASCVD,
                                  "0"="No",
                                  "1"="Yes"),
    Post_index_cardiac_rehab=dplyr::recode(Post_index_cardiac_rehab,
                                           "0"="No",
                                           "1"="Yes")
  )

dt_stroke <- fread(file.path(path_to_data, 'cohort_stroke.csv')) %>%
  rename(Year=year,
         Age = age_cat,
         Gender = bene_sex_ident_cd,
         Race = bene_race_cd,
         Pre_index_statin = bl_statin,
         Pre_index_statin_intensity = pre_potency,
         Pre_index_ezetimibe = bl_Eze,
         Pre_index_PCSK9i = bl_PCSK9i,
         Pre_index_PCSK9i_dose = bl_PCSK9i_dose,
         Pre_index_alirocumab_dose = bl_Ali_dose,
         Pre_index_alirocumab = bl_Ali,
         Pre_index_evolocumab = bl_Evo,
         Pre_index_evolocumab_dose = bl_Evo_dose,
         Pre_index_medication_number = bl_med_count,
         Pre_index_hypertension = bl_hypertsn,
         Pre_index_depression = bl_depress,
         Pre_index_diabetes = bl_diab,
         Pre_index_CKD = bl_CKD,
         Pre_index_CHD = bl_CHD,
         Pre_index_MI = bl_MI,
         Pre_index_CABG_PCI = bl_CABG,
         Pre_index_smoking = bl_smoke,
         Pre_index_cardiologist_visit = bl_cardio,
         Pre_index_HF = bl_HF,
         Pre_index_LEAD = bl_lead,
         Pre_index_ischemic_stroke = bl_Isch_stroke,
         Pre_index_ASCVD = bl_ASCVD,
         Post_index_statin_intensity = fu_potency,
         Post_index_statin = fu_statin,
         Post_index_statin_days = fu_statin_days,
         Post_index_high_intensity_statin = fu_high_statin,
         Post_index_high_intensity_statin_days = fu_high_statin_days,
         Post_index_ezetimibe = fu_Eze,
         Post_index_ezetimibe_days = fu_Eze_days,
         Post_index_PCSK9_inhibitor = fu_PCSK9i,
         Post_index_PCSK9_inhibitor_days = fu_PCSK9i_days,
         Post_index_cardiac_rehab_number = fu_cardiac_rehab_count,
         Post_index_cardiac_rehab = fu_cardiac_rehab,
         Post_index_cardiologist_visit = fu_cardio,
         Post_index_cardiologist_visit_days = fu_cardio_days,
         Post_index_neurologist_visit = fu_neuro,
         Post_index_neurologist_visit_days = fu_neuro_days,
         Post_index_outpatient_visit = fu_outpatient_visit,
         Post_index_outpatient_visit_days = fu_outpatient_visit_days,
         Post_index_any_hospitalization = fu_any_hosp,
         Post_index_any_hospitalization_days = fu_any_hosp_days,
         Post_index_CHD_hospitalization = fu_CHD,
         Post_index_CHD_hospitalization_days = fu_CHD_days,
         Post_index_MI_hospitalization = fu_MI,
         Post_index_MI_hospitalization_days = fu_MI_days,
         Post_index_HF_hospitalization = fu_HF,
         Post_index_HF_hospitalization_days = fu_HF_days,
         Post_index_ischemic_stroke_hospitalization = fu_Isch_stroke,
         Post_index_ischemic_stroke_hospitalization_days = fu_Isch_stroke_days
  ) %>%
  mutate(
    Age=dplyr::recode(Age, "1"="65-69",
                      "2"="70-74",
                      "3"="75-79",
                      "4"="80-84",
                      "5"="85+"),
    Gender=dplyr::recode(Gender, "1"="Male",
                         "2"="Female"),
    Race=dplyr::recode(Race,
                       "0"="Unknown",
                       "1"="White",
                       "2"="Black",
                       "3"="Other",
                       "4"="Asian",
                       "5"="Hispanic",
                       "6"="Native"
    ),
    Pre_index_statin=dplyr::recode(Pre_index_statin,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_statin_intensity=dplyr::recode(Pre_index_statin_intensity,
                                             "3"="High",
                                             "2"="Moderate",
                                             "1"="Low"),
    Pre_index_statin_intensity=factor(Pre_index_statin_intensity,
                                      levels=c("Low", "Moderate","High")),
    Pre_index_ezetimibe=dplyr::recode(Pre_index_ezetimibe,
                                      "0"="No",
                                      "1"="Yes"),
    Pre_index_PCSK9i=dplyr::recode(Pre_index_PCSK9i,
                                   "0"="No",
                                   "1"="Yes"),
    Pre_index_alirocumab=dplyr::recode(Pre_index_alirocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_evolocumab=dplyr::recode(Pre_index_evolocumab,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_hypertension=dplyr::recode(Pre_index_hypertension,
                                         "0"="No",
                                         "1"="Yes"),
    Pre_index_depression=dplyr::recode(Pre_index_depression,
                                       "0"="No",
                                       "1"="Yes"),
    Pre_index_diabetes=dplyr::recode(Pre_index_diabetes,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_CKD=dplyr::recode(Pre_index_CKD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_CHD=dplyr::recode(Pre_index_CHD,
                                "0"="No",
                                "1"="Yes"),
    Pre_index_MI=dplyr::recode(Pre_index_MI,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_CABG_PCI=dplyr::recode(Pre_index_CABG_PCI,
                                     "0"="No",
                                     "1"="Yes"),
    Pre_index_smoking=dplyr::recode(Pre_index_smoking,
                                    "0"="No",
                                    "1"="Yes"),
    Pre_index_cardiologist_visit=dplyr::recode(Pre_index_cardiologist_visit,
                                               "0"="No",
                                               "1"="Yes"),
    Pre_index_HF=dplyr::recode(Pre_index_HF,
                               "0"="No",
                               "1"="Yes"),
    Pre_index_LEAD=dplyr::recode(Pre_index_LEAD,
                                 "0"="No",
                                 "1"="Yes"),
    Pre_index_ischemic_stroke=dplyr::recode(Pre_index_ischemic_stroke,
                                            "0"="No",
                                            "1"="Yes"),
    Pre_index_ASCVD=dplyr::recode(Pre_index_ASCVD,
                                  "0"="No",
                                  "1"="Yes"),
    Post_index_cardiac_rehab=dplyr::recode(Post_index_cardiac_rehab,
                                           "0"="No",
                                           "1"="Yes")
  )





# UI ----


ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # Application title
    introBox(
      titlePanel("Medicap"),
      data.step = 1,
      data.intro = "This is an application to explore Medicare data"
    ),

    # Sidebar
    sidebarLayout(
      sidebarPanel(

        introBox(
          actionButton("help",
                       "Press for instructions",
                       icon = icon("question"),
                       width = '95%')
        ),

        br(),

        dataSummarizerInput(
          'summarizer_inputs',
          input_width = '97.5%',
          ttev_condition = jsc_write_cpanel(key_data, 'ttev', 'outcome'),
          ctns_condition = jsc_write_cpanel(key_data, 'ctns', 'exposure')
        )

      ),

      mainPanel(

        tabsetPanel(
          type = "pills",

          tabPanel(
            introBox(
              "Summary data",
              data.step = 6,
              data.intro = "In the 'Summary data' section, you will see data tables printed whenever you click 'compute'."
            ),
            br(),
            uiOutput('dl_summary_data'),
            br(),
            DTOutput('result_table'),
          ),
          tabPanel(
            introBox(
              "Tabulate",
              data.step = 7,
              data.intro = "In the 'Tabulate' section, you can generate more readable tables of the summary data by clicking 'tabulate'."
            ),
            br(),
            wellPanel(tabulate_input_list),
            gt_output('gt_table')
          )

        )
      )
    )
  )
)

# Server ----

server = function(input, output, session) {

  result <- dataSummarizerServer('summarizer_inputs',
                                 dt_racs = dt_racs,
                                 dt_stroke = dt_stroke,
                                 dt_ami = dt_ami,
                                 key_list = key_list,
                                 key_data = key_data,
                                 key_time = key_time)

  observeEvent(

    result(), {

      output$dl_summary_data <- renderUI(
        downloadButton("dl_summary_data_active",
                       label = "Download these summary data",
                       style = "width:100%;")
      )

      output$dl_summary_data_active <- downloadHandler(
        filename = function() {
          paste('summary_data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write_csv(result(), con)
        }
      )

      output$result_table <- DT::renderDataTable(result())

      choices_stat <- names(result()) |>
        recode(!!!stat_recoder, .default = NA_character_) |>
        na.omit()

      if(input$statistic %!in% choices_stat || is_empty(input$statistic)){

        updatePickerInput(
          session = session,
          inputId = 'statistic',
          choices = choices_stat,
          selected = character(0)
        )

      }

    }

  )

  tbl <- reactive({

        .stat <- enframe(stat_recoder) |>
          filter(value == input$statistic) |>
          slice(1) |>
          pull(name)

        .gt_cols <- c(key_time, .stat)

        .exposure <- .group <- "None"

        .year <- setdiff(levels(result()[[key_time]]), 'Overall')

        if(is_used(input$`summarizer_inputs-exposure`)){

          .exposure <- input$`summarizer_inputs-exposure`
          .gt_cols <- c(.gt_cols, .exposure)

        }

        if(is_used(input$`summarizer_inputs-group`)){

          .group <- input$`summarizer_inputs-group`
          .gt_cols <- c(.gt_cols, .group)

        }

        data_gt <- result()

        if(key_list[[input$`summarizer_inputs-outcome`]]$type == 'ttev'){

          .by <- setdiff(.gt_cols, .stat)

          data_gt <- data_gt[
            result()[, .I[which.max(time)], by = .by]$V1
          ]

        }

        if(.stat %in% c('prevalence', 'cuminc')){
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
    bindEvent(input$do_tabulate)

  observeEvent(

    tbl(), {

      output$gt_table <- render_gt(tbl())

      output$dl_table <- renderUI(
        downloadButton("dl_table_active",
                       label = "Download this table",
                       style = "width:95%;")
      )

      output$dl_table_active <- downloadHandler(
        filename = function() {
          paste('summary_table-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          gtsave(tbl(), con)
        }
      )

    }

  )



  observeEvent(
    input$help, {

      if(is.null(input$dataset)){
        updatePickerInput(
          session = session,
          inputId = 'dataset',
          selected = 'racs'
        )
      }

      introjs(session,
              options = list("nextLabel"="Next",
                             "prevLabel"="Previous",
                             "skipLabel"="Skip"))

    }
  )

}

shinyApp(ui = ui, server = server)



