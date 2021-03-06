#' append_date_suffix
#'
#' Generates a day of the month value with the approriate suffix (e.g. 9th or 2nd)
#' Obtained from https://stackoverflow.com/questions/40039903/r-add-th-rd-and-nd-to-dates
#'
#' @param  dates A vector of dates for which to generate the appropriate day of the month and assocaited suffix
#'
#' @export
#' @return A vector of day of the month values with the appropriate suffix.
#'
append_date_suffix <- function(dates){
  require(lubridate)
  require(dplyr)
  dayy <- day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

