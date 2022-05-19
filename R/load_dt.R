#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path_to_data
load_dt <- function(path_to_data, fname) {

  data_in <- fread(file.path(path_to_data, fname))

  data_in[Pre_index_statin_intensity == '',
          Pre_index_statin_intensity := 'None']

  data_in[,
          Pre_index_statin_intensity := factor(Pre_index_statin_intensity,
                                               levels = c("None",
                                                          "Low",
                                                          "Moderate",
                                                          "High"))]

  data_in[, Post_index_cardiologist_visit_90 := 0]

  data_in[
    Post_index_cardiologist_visit_days <= 90 & Post_index_cardiologist_visit == 1,
    Post_index_cardiologist_visit_90 := 1
  ]

  data_in[
    ,
    Post_index_cardiologist_visit_90 := factor(Post_index_cardiologist_visit_90,
                                               levels = c(0,1),
                                               labels = c("No", "Yes"))
  ]

  data_in

}
