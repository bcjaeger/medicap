#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n_obs
load_dt_fake <- function(n_obs) {

  set.seed(329)

  guide <- read_rds('data/fake_data_guide.rds')

  guide$Year <- NULL

  dt_fake <- data.table(
    Year = sample(2007:2019, size = n_obs, replace = TRUE)
  )

  for(i in names(guide)){

    dt_fake[[i]] <- sample(guide[[i]],
                           size = n_obs,
                           replace = TRUE)

    if(is.factor(guide[[i]])){

      l <- levels(guide[[i]])
      dt_fake[, f := factor(f, levels = l), env = list(f = i)]

    }

  }

  dt_fake[['Post_index_statin']] <-
    rbinom(n_obs, size = 1, prob = 1/2)

  dt_fake[['Post_index_statin_days']]<-
    abs(round(rnorm(n_obs, mean = 180, sd = 50)))

  list(dt_stroke = copy(dt_fake),
       dt_ami = copy(dt_fake),
       dt_racs = copy(dt_fake)) |>
    map(
      select,
      Year,
      Age,
      Gender,
      Race,
      Pre_index_hypertension,
      year_ABDHMO,
      Post_index_statin,
      Post_index_statin_days
    )

}
