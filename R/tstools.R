
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

#' Exponential moving average (EMA)
#' 
#' EMA = [x_1 + (1-k) x_2 + (1-k)^2 x_3 + ...]/[1 + (1-k) + (1-k)^2 + ...]
#' 
#' @param x input vector (most recent value comes the last)
#' @param k decaying factor
#' @param n backward window length (include current value)
#' @param complete only return complete window
#'
#' @return MA vector of the same length as @x
#' @remark The function will ignore leading NAs. However, missing values
#'         in the middle of the data will not be dealt with. So make sure
#'         there is no missing values in the middle of the data.
#' @export
ema <- function(x, k = NULL, n = NULL, complete = F) {
  
  if (is.null(n)) n = length(x)
  if (is.null(k)) k = 2/(n+1)
  
  # weights
  w <- map_dbl(1:n, ~(1-k)^(.x-1))
  # output vector
  y <- double(length(x))
  
  # ignore leading NAs
  s <- 1  
  while (s < length(x) && is.na(x[s])) {
    s <- s + 1
  }
  y[1:(s-1)] <- NA_real_
  # s is the starting index of non-NA values
  # e is the index of the end of the first window
  e <- s+n-1

  # incomplete window
  if (isTRUE(complete)) {
    y[s:(e-1)] <- NA_real_
  } else {
    for (i in s:(e-1)) {
      y[i] <- x[s:i] %*% rev(w[1:(i-s+1)]) / sum(w[1:(i-s+1)])
    }
  }
  
  # compute complete window 
  for (i in e:length(x)) {
    y[i] <- x[(i-n+1):i] %*% rev(w) / sum(w)
  }
  
  y
}
