#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n_obs
load_dt_fake <- function(n_obs) {

  set.seed(329)

  dt_fake <- data.table(
    Year = sample(2007:2019, size = n_obs, replace = TRUE),
    Age = sample(1:5, size = n_obs, replace = TRUE),
    Gender = sample(1:2, size = n_obs, replace = TRUE),
    Race = sample(0:6, size = n_obs, replace = TRUE)
  )

  dt_fake[, Pre_index_statin_intensity := sample(x = c(0, 1, 2),
                                                 size = n_obs,
                                                 replace = TRUE)]

  dt_fake[, Pre_index_statin := rbinom(n_obs, size = 1, prob = Gender/3)]
  dt_fake[, Post_index_statin := rbinom(n_obs, size = 1, prob = Race/sum(0:6))]
  dt_fake[, Post_index_statin_days := abs(round(rnorm(n_obs, mean = 180, sd = 50)))]

  dt_fake[, Age := factor(Age,
                          levels = 1:5,
                          labels = c("65-69",
                                     "70-74",
                                     "75-79",
                                     "80-84",
                                     "85+"))]

  dt_fake[, Gender := factor(Gender,
                             levels = 1:2,
                             labels = c("Male", "Female"))]

  dt_fake[, Race := factor(Race,
                           levels = 0:6,
                           labels = c("Unknown",
                                      "White",
                                      "Black",
                                      "Other",
                                      "Asian",
                                      "Hispanic",
                                      "Native"))]

  list(dt_stroke = copy(dt_fake),
       dt_ami = copy(dt_fake),
       dt_racs = copy(dt_fake))

}
