library(tidyverse)
library(lubridate)
library(magrittr)
library(seasonal)
library(mFilter)
library(timetk)
library(rlang)
library(slider)

# X11 seasonal adjustment
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

# Hodrick-Prescott filter
hp_filter <- function(x, .lambda=1600) {
  # returns an mFilter object
  result <- hpfilter(x, freq = .lambda, type = "lambda")
  # returns a list
  unclass(result) 
}

# Christiano-Fitzgerald filter
cf_filter <- function(x, .pl, .pu, .root=T, .drift=T) {
  # returns an mFilter object
  result <- cffilter(x, 
                     pl=.pl, 
                     pu=.pu, 
                     root=.root, 
                     drift=.drift)
  unclass(result) 
}

zoo_to_ts <- function(x) {
  stopifnot(is.zoo(x))
  ind <- zoo::index(x)
  freq <- tk_get_frequency(ind, message = F) 
  
  start <- case_when(
    freq == 4L ~ c(year(ind[1]), quarter(ind[1])), 
    freq == 12L ~ c(year(ind[1]), month(ind[1]))
  )
  
  if (all(is.na(start)))
    stop(paste("Unsupported frequency:", freq))
  
  # convert to ts object. seas requires ts object
  ts(x, start = start, frequency = freq)
}

tbl_to_zoo <- function(object, index_var, data_var) {
  
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

tbl_to_ts <- function(object, index_var, data_var) {
  stopifnot(!missing(object) && is_tibble(object))
  stopifnot(!missing(index_var), !missing(data_var))
  
  index_var <- enquo(index_var)
  data_var <- enquo(data_var)
  
  index <- object %>% pull(!!index_var)
  data <- object %>% pull(!!data_var)
  
  x <- xts(x = data, order.by = index)
  zoo_to_ts(x)
}

# series a time series into a string of Matlab code
# seriesInfo = 
#   list(
#     seriesId = 'CRDQCNAPABIS',
#     srCode = 'SRCRDQCNAPABIS',
#     country = 'China',
#     seriesName = 'CN: Total Credit to Private Non-Financial Sector',
#     nameLocal = '',
#     Unit = 'RMB bn',
#     frequency = 'Quarterly',
#     source = 'Bank for International Settlements',
#     firstObsDate = '10/01/1985 00:00:00',
#     LastObsDate = '',
#     multiplierCode = 'BN',
#     tLastUpdTime = '',
#     seriesStatus = '',
#     remarks  = '',
#     functionInformation = '""'
#   )
.ts_serialize <- function(object, index_var, data_var, 
                          name = "", timeFormat = "",
                          seriesInfo = list()) {
  
  index_var <- enquo(index_var)
  data_var <- enquo(data_var)
  var_name <- quo_name(data_var)
  
  index <- object %>% pull(!!index_var)
  data <- object %>% pull(!!data_var)
  
  str_data <- sprintf("%s =\ntimeseries([%s], {%s});", 
                      var_name,
                      paste(data, collapse = ";"), 
                      paste(
                        index %>% 
                          map_chr(~format.Date(., "%m/%d/%Y")) %>%
                          map_chr(~sprintf("'%s'", .)), 
                        collapse = ","))
  
  str_name <- str_c(sprintf("\n%s.Name = '%s';", var_name, name), 
                    sprintf("\n%s.TimeInfo.Format = '%s';", var_name, timeFormat))
  
  str_info <- sprintf("\n%s.UserData.seriesInfo = struct(%s);", 
                      var_name, 
                      map2_chr(seriesInfo, 
                               names(seriesInfo), 
                               ~ sprintf("'%s','%s'", .y, .x)) %>%
                        str_c(collapse = ","))
  
  str_c(str_data, str_name, str_info)
}


