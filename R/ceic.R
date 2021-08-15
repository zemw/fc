library(tidyverse)
library(magrittr)
library(furrr)
library(xts)
library(re2r)


ceic_load <- function(file, parallel = FALSE) {
  
  # set up parallel computing
  if (isTRUE(parallel))
    future::plan(multisession)
  
  .raw_text <- read_file(file) # load matlab .m file
  
  .series <- NULL   # store time series data
  .metadata <- NULL # store metadata of the series
  
  # regex matching that returns a data.frame with 
  # captured groups as the columns
  match_df <- function(txt, re, pl = parallel) {
    ret <- re2_match_all(txt, re, parallel = pl)
    ret_m <- unclass(ret[[1]])
    data.frame(ret_m[,-1])
  }
  
  # extract various data fields from raw text by regular expression
  # run these matching in parallel to speed up processing
  m_data <- match_df(.raw_text, "(?P<var>\\w*)\\s=\\stimeseries\\(\\[(?P<value>.*?)],\\s\\{(?P<index>.*?)\\}\\);") 
  m_name <- match_df(.raw_text, "(?P<var>\\w*)\\.Name\\s=\\s'(?P<name>.*?)';") 
  m_info <- match_df(.raw_text, "(?P<var>\\w*)\\.UserData\\.seriesInfo\\s=\\sstruct\\((?P<info>.*?)\\);") 
  m_format <- match_df(.raw_text, "(?P<var>\\w*)\\.TimeInfo\\.Format\\s=\\s'(?P<format>.*?)';") 
  
  # extract metadata (key-value pairs) of a series from seriesInfo
  extract_info <- function(var, info) {
    # extract key-value pairs from data field `info`
    match_df(info, "'(?P<key>\\w*)',(?P<value>'.*?'|\\[.*?\\])", FALSE) %>%
      mutate_if(is.character, ~ str_remove_all(., "\'")) %>%
      filter(key != "", key != "functionInformation") %>%
      pivot_wider(names_from = key) 
  }
  
  # run through every match and join the results into one table
  if (isTRUE(parallel)) {
    # future_pmap runs the map in parallel
    .metadata <- m_info %>% 
      future_pmap(extract_info, .options = furrr_options(seed = TRUE)) %>% 
      reduce( ~ bind_rows(.x, .y)) 
  } else {
    .metadata <- m_info %>% 
      pmap(extract_info) %>% 
      reduce( ~ bind_rows(.x, .y)) 
  }

  # extract times series dates and values from text
  extract_data <- function(var, value, index) {
    values <- str_split(value, ';')[[1]]
    dates <- str_split(index, ',')[[1]]
    xts(as.numeric(values), lubridate::mdy(dates))
  }
  
  # run through every match and return a list of xts objects
  if (isTRUE(parallel)) {
    .series <- m_data %>%
      future_pmap(extract_data, .options = furrr_options(seed = TRUE))
  } else {
    .series <- m_data %>% 
      pmap(extract_data)
  }
  names(.series) <- .metadata$seriesId
  
  
  ## return a list of functions
  list(
    # query metadata of series by keywords in names
    query = function(keyword = NULL) {
      if (!is.null(keyword)) {
        .metadata %>%
          filter(str_detect(seriesName, fixed(keyword, ignore_case = T)))
      } else {
        .metadata
      }
    }, 
    
    # return all series in the dataset
    fetch_all = function(output = 'tbl') {
      mts <- .series %>% reduce(~merge(.x, .y))
      lbs <- .metadata %>% pull(seriesName)
      tbl <- data.frame(.= zoo::index(mts), mts) %>% as_tibble
      names(tbl) <- make.unique(c("date", lbs))
      tbl
    }, 
    
    # fetch time series by id or name
    fetch = function(seriesId = NULL, 
                     keyword = NULL, 
                     output = c('tbl', 'xts'), 
                     rename_cols = NULL) {
      
      x <- NULL # series to be returned
      
      if (!is.null(seriesId)) {
        x <- .series[seriesId] %>% reduce(~ merge(.x, .y))
      } else if (!is.null(keyword)) {
        seriesId <- ceic_query(keyword) %>% pull(seriesId)
        x <- .series[seriesId] %>% reduce(~ merge(.x, .y))
      } else {
        stop('No series IDs or names specified!')
      }
      
      if (is.null(x)) return(x) # no series found
      
      if (!is.null(rename_cols)) {
        names(x) <- rename_cols
      } else {
        names(x) <- seriesId
      }
      
      output <- match.arg(output)
      if (output == 'tbl') {
        data.frame(date=index(x), x) %>% as_tibble
      } else {
        x
      }
    },
    
    # translate series id into names
    get_name = function(series_id, with_unit = FALSE) {
      name <- .metadata %>% 
        filter(seriesId == series_id) %>%
        pull(seriesName)
      if (with_unit) {
        unit <- .metadata %>% 
          filter(seriesId == series_id) %>%
          pull(Unit)
        sprintf("%s (%s)", name, unit)
      } else {
        name
      }
    }
    
  ) # end of list
}
