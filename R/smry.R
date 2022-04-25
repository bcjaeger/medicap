

smry_ctns <- function(x,
                      stat_names = c('mean',
                                     'quantile',
                                     'se',
                                     'sd')){

  out <- list()

  if('mean' %in% stat_names) out$mean <- mean(x, na.rm = TRUE)

  if('quantile' %in% stat_names){

    qs <- quantile(x, na.rm = TRUE)
    names(qs) <- paste('quant',
                       str_remove(names(qs),'\\%'),
                       sep = '_')

    out <- c(out, qs)

  }

  if('se' %in% stat_names || 'sd' %in% stat_names){

    .sd <- sd(x, na.rm = TRUE)

    if('sd' %in% stat_names) out$sd <- .sd

    if('se' %in% stat_names) out$se <- .sd / sqrt(length(x)-1)

  }

  as.data.table(out)

}

smry_bnry <- function(x,
                      stat_names = c('n_event',
                                     'n_total',
                                     'prevalence',
                                     'odds')){

  if(is.factor(x)) return(
    smry_bnry(x = as.numeric(x)-1,
              stat_names = stat_names)
  )

  out <- list()

  n_event <- sum(x)
  n_total <- length(x)

  if('n_event' %in% stat_names) out$n_event <- n_event
  if('n_total' %in% stat_names) out$n_total <- n_total

  prevalence <- n_event / n_total

  if('prevalence' %in% stat_names) out$prevalence <- prevalence

  if('odds' %in% stat_names) out$odds <- prevalence / (1-prevalence)

  as.data.table(out)

}

smry_ttev <- function(status, time, horizon){

  if(sum(status) < 1){
    return(data.table(time = sort(unique(time)),
                      cuminc = 0,
                      se = 0))
  }

  out <- cuminc(ftime = time, fstatus = status) |>
    getElement(1) |>
    as.data.table()

  out[, var:= sqrt(var)]

  setnames(out, old = 'var', new = 'se')
  setnames(out, old = 'est', new = 'cuminc')

  out[time < horizon]

}
