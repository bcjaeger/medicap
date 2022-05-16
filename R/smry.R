

smry_ctns <- function(x,
                      stat_names = c('mean_est',
                                     'quantile',
                                     'se',
                                     'sd')){
  UseMethod('smry_ctns')

}

smry_ctns.numeric <- function(x,
                              stat_names = c('mean_est',
                                             'quantile',
                                             'se',
                                             'sd')){

  if(length(na.omit(x)) < 12) return(
    data.table(
      ctns_mean_est = NA_real_,
      ctns_quant_0 = NA_real_,
      ctns_quant_25 = NA_real_,
      ctns_quant_50 = NA_real_,
      ctns_quant_75 = NA_real_,
      ctns_quant_100 = NA_real_,
      ctns_sd = NA_real_,
      ctns_se = NA_real_
    )
  )

  out <- list()

  if('mean_est' %in% stat_names) out$mean_est <- mean(x, na.rm = TRUE)

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

  names(out) <- paste('ctns', names(out), sep = '_')

  as.data.table(out)

}

smry_ctns.character <- function(x,
                      stat_names = c('mean_est',
                                     'quantile',
                                     'se',
                                     'sd')){

  smry_ctns(x = as.factor(x), stat_names = stat_names)

}

smry_ctns.factor <- function(x,
                      stat_names = c('mean_est',
                                     'quantile',
                                     'se',
                                     'sd')){

  smry_ctns(x = as.numeric(x)-1, stat_names = stat_names)

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

  if(is.character(x)) return(
    smry_bnry(x = as.factor(x),
              stat_names = stat_names)
  )

  if(length(na.omit(x)) < 12){
    return(
      data.table(
        bnry_n_event = NA_real_,
        bnry_n_total = NA_real_,
        bnry_prevalence = NA_real_,
        bnry_odds = NA_real_
      )
    )
  }

  out <- list()

  n_event <- sum(x, na.rm = TRUE)
  n_total <- length(na.omit(x))

  if('n_event' %in% stat_names) out$n_event <- n_event
  if('n_total' %in% stat_names) out$n_total <- n_total

  prevalence <- n_event / n_total

  if('prevalence' %in% stat_names) out$prevalence <- prevalence

  if('odds' %in% stat_names) out$odds <- prevalence / (1-prevalence)

  names(out) <- paste('bnry', names(out), sep = '_')

  as.data.table(out)

}

smry_ttev <- function(status,
                      time,
                      horizon,
                      ABDHMO_days = NULL,
                      crude_inc_mult_by = 365.25 * 1000){

  if(sum(status, na.rm = TRUE) < 12){
    return(data.table(ttev_time = NA_real_,
                      ttev_inc_cumulative_est = NA_real_,
                      ttev_inc_cumulative_se = NA_real_,
                      ttev_inc_crude_est = NA_real_))
  }

  # ABDHMO_days is used to account for patients without a
  # follow-up time for the given outcome. Patients who did
  # not have an event will not have a recorded follow-up time
  # for the given outcome but will have a value for ABDHMO_days.
  if(!is.null(ABDHMO_days)){
    time[is.na(time)] <- ABDHMO_days[is.na(time)]
  }

  # truncate time values and status values to the given horizon
  time_past_horizon <- which(time > horizon)

  .time <- time
  .time[time_past_horizon] <- horizon

  .status <- status
  .status[time_past_horizon] <- 0

  if(sum(.status, na.rm = TRUE) == 0){
    return(data.table(ttev_time = NA_real_,
                      ttev_inc_cumulative_est = NA_real_,
                      ttev_inc_cumulative_se = NA_real_,
                      ttev_inc_crude_est = NA_real_))
  }

  out <- try(
    cuminc(ftime = .time, fstatus = .status) |>
      getElement(1) |>
      as.data.table()
  )

  if(inherits(out, 'try-error')) browser()


  out[, var:= sqrt(var)]

  setnames(out, old = 'var', new = 'inc_cumulative_se')
  setnames(out, old = 'est', new = 'inc_cumulative_est')

  index_keep <- out[time <= horizon, .I]

  out[['inc_crude_est']] <- NA_real_

  out[index_keep[length(index_keep)],
             inc_crude_est := crude_inc_mult_by *
               sum(.status, na.rm = TRUE) /
               sum(.time, na.rm = TRUE)]

  names(out) <- paste('ttev', names(out), sep = '_')

  out <- out[index_keep]

  diffs <- c(1, diff(out$ttev_time))

  out[diffs == 0, ttev_time := ttev_time + 0.01]

  # prepend the 0 row so that plots will look more uniform

  first_row <- data.table(ttev_time = 0,
                          ttev_inc_cumulative_est = 0,
                          ttev_inc_cumulative_se = 0,
                          ttev_inc_crude_est = 0)

  rbindlist(list(first_row, out))

}
