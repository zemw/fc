
#' Convert to `ts` object
#'
#' @param x a vector containing time series data
#' @param date a vector containing date/time series
#' @param freq frequency of the series (e.g. quarterly=4, monthly=12)
#'
#' @return a `ts` object
#' @export
as_ts <- function(x, date, freq = 4) {
  
  require(lubridate)
  
  if (freq == 1) {
    conv <- function(.) lubridate::year(.)
  } else if (freq == 4) {
    conv <- function(.) c(lubridate::year(.), lubridate::quarter(.))
  } else if (freq == 12) {
    conv <- function(.) c(lubridate::year(.), lubridate::month(.))
  } else {
    stop("Frequency must be onen of 1,4,12.")
  }
  
  t_start <- date[[1]]
  t_end <- date[[length(date)]]
  
  tseries <- ts(x, start = conv(t_start), end = conv(t_end), frequency = freq)
  
  tseries
}

