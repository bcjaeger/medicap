#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param ref
relevel_coerce <- function(x, ref, ...){

  if(is.character(x)){
    x <- as.factor(x)
  }

  relevel(x, ref, ...)

}
