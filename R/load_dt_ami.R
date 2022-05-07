#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path_to_data
load_dt_ami <- function(path_to_data) {

  fread(file.path(path_to_data, 'cohort_AMI_preprocessed.csv'))

}
