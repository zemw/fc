library(tidyverse)
library(lubridate)
library(seasonal)
library(mFilter)
library(timetk)
library(rlang)


tbl_to_xts <- function(object, index_var, data_var) {
  
  stopifnot(!missing(object) && is_tibble(object))
  stopifnot(!missing(index_var), !missing(data_var))
  
  index_var <- enquo(index_var)
  data_var <- enquo(data_var)
  
  index <- object %>% pull(!!index_var)
  data <- object %>% pull(!!data_var)
  
  x <- xts(x = data, order.by = index)
  names(x) <- quo_name(data_var)
  
  x
}

x11_adjust <- function(x, index = NULL) {
  
  if (is.zoo(x)) index <- index(x)
  else stopifnot(!is.null(index))
  
  freq <- tk_get_frequency(index, message = FALSE) 
  
  if (freq == 4) 
    start <- c(year(index[1]), quarter(index[1]))
  else if (freq == 12) 
    start <- c(year(index[1]), month(index[1]))
  else stop(paste("Unsupported frequency:", freq))
  
  # convert to ts object. seas requires ts object
  x_ts <- ts(x, start = start, frequency = freq)
  x_sa <- seas(x_ts, x11 = "")
  
  # return a list of ts series
  list(
    final = final(x_sa),
    original = original(x_sa), 
    trend = trend(x_sa),
    irregular = irregular(x_sa)
  )
}

hp_filter <- function(x, .lambda=1600) {
  # returns an mFilter object
  result <- hpfilter(x, freq = .lambda, type = "lambda")
  # returns a list
  unclass(result) 
}

cf_filter <- function(x, .pl, .pu, .root=TRUE, .drift=TRUE) {
  result <- cffilter(x, 
                     pl=.pl, 
                     pu=.pu, 
                     root=.root, 
                     drift=.drift)
  unclass(result) 
}
