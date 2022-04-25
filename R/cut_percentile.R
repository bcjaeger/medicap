#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param exposure
#' @param
cut_percentile <- function(exposure, n_group) {

  # this will likely need to be coerced; input$n_group is a character
  n_group <- as.numeric(n_group)

  probs <- seq(0, 1, length.out = n_group + 1)

  breaks <- quantile(exposure, probs)

  labels <- vector(mode = 'character', length = n_group)

  for(i in seq(n_group)){

    if(i < n_group)
      labels[i] <- table_glue("{breaks[i]} to \u2264{breaks[i+1]}")

    if(i == n_group)
      labels[i] <- table_glue("\u2265{breaks[i]}")

  }

  out <- cut(exposure,
             breaks = breaks,
             include.lowest = TRUE,
             right = FALSE,
             labels = labels)





}
