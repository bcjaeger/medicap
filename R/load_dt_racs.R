#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path_to_data
load_dt_racs <- function(path_to_data) {
  fread(file.path(path_to_data, 'cohort_RACS_preprocessed.csv'))
}
