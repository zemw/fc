library(tidyverse)
library(magrittr)
library(xts)

source('regex.R')


ceic_load <- function(file) {
  
  .raw_text <- read_file(file) # load matlab .m file
  
  .series <- NULL   # store time series data
  .metadata <- NULL # store metadata of the series
  
  # extract various data fields from raw text by regular expression
  m_data <- re_matches(.raw_text, "(?<var>\\w*)\\s=\\stimeseries\\(\\[(?<value>.*?)],\\s\\{(?<index>.*?)\\}\\);") 
  m_name <- re_matches(.raw_text, "(?<var>\\w*)\\.Name\\s=\\s'(?<name>.*?)';") 
  m_info <- re_matches(.raw_text, "(?<var>\\w*)\\.UserData\\.seriesInfo\\s=\\sstruct\\((?<info>.*?)\\);") 
  m_format <- re_matches(.raw_text, "(?<var>\\w*)\\.TimeInfo\\.Format\\s=\\s'(?<format>.*?)';") 
  
  # extract metadata (key-value pairs) of a series from seriesInfo, and 
  # join the metadata of all series into one table.
  .metadata <-
    m_info %>% pmap(function(var, info) {
      # extract key-value pairs from data field `info`
      re_matches(info, "'(?<key>\\w*)',(?<value>'.*?'|\\[.*?\\])") %>%
        mutate_if(is.character, ~ str_remove_all(., "\'")) %>%
        filter(key != "") %>%
        pivot_wider(names_from = key) 
    }) %>% reduce( ~ bind_rows(.x, .y)) 
  
  # extract times series dates and values from text
  # return a list of xts objects
  .series <-
    m_data %>% pmap(function(var, value, index) {
      values <- str_split(value, ';')[[1]]
      dates <- str_split(index, ',')[[1]]
      xts(as.numeric(values), lubridate::mdy(dates))
    })
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
    
    # fetch time series by id or name
    fetch = function(seriesId = NULL, 
                     keyword = NULL, 
                     output = c('df', 'xts'), 
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
      if (output == 'df') {
        data.frame(index=index(x), x) %>% as_tibble
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

# Example of usage
cki_m <- ceic_load("CKI_M.m")
cki_q <- ceic_load("CKI_Q.m")


