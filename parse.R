library(tidyverse)
library(magrittr)
library(tsibble)
library(xts)

source('regcapturedmatches.R')

.series <- NULL   # store time series data
.metadata <- NULL # store metadata of the series


# return a datafrome whose rows are the matches 
# and columns the matched groups
re_matches <- function(text, pattern) {
  gregexpr(pattern, text, perl=T) %>% 
    regcapturedmatches(text, .) %>%
    data.frame()
}


ceic_init <- function(file) {
  
  raw_text <- read_file(file) # load matlab .m file
  
  re_data <- "(?<id>\\w*)\\s=\\stimeseries\\(\\[(?<value>.*?)],\\s\\{(?<index>.*?)\\}\\);"
  re_name <- "(?<id>\\w*)\\.Name\\s=\\s'(?<name>.*?)';"
  re_format <- "(?<id>\\w*)\\.TimeInfo\\.Format\\s=\\s'(?<format>.*?)';"
  re_info <- "(?<id>\\w*)\\.UserData\\.seriesInfo\\s=\\sstruct\\((?<info>.*?)\\);"
  re_key_value <- "'(?<key>\\w*)',(?<value>'.*?'|\\[.*?\\])"
  
  m_data <- re_matches(raw_text, re_data) 
  m_name <- re_matches(raw_text, re_name) 
  m_info <- re_matches(raw_text, re_info) 
  m_format <- re_matches(raw_text, re_format) 
  
  # extract metadata (key-value pairs) of a series from seriesInfo, and 
  # join the metadata of all series into one table.
  .metadata <<-
    m_info %>% pmap(function(id, info) {
      re_matches(info, re_key_value) %>%
        mutate_if(is.character, ~ str_remove_all(., "\'")) %>%
        pivot_wider(names_from = key) %>%
        mutate(id = id)
    }) %>% reduce( ~ bind_rows(.x, .y)) 
  
  # extract times series dates and values from text
  # return a list of xts objects
  .series <<-
    m_data %>% pmap(function(id, value, index) {
      values <- str_split(value, ';')[[1]]
      dates <- str_split(index, ',')[[1]]
      xts(as.numeric(values), lubridate::mdy(dates))
    })
  names(.series) <<- m_data$id
  
}

ceic_query <- function(name = NULL) {
  if (is.null(name)) {
    .metadata
  } else {
    .metadata %>%
      filter(str_detect(seriesName, fixed(name, ignore_case = T)))
  }
}


ceic_get_name <- function(idx) {
  idx <- as.character(idx) 
  .metadata %>%
    filter(str_detect(id, idx)) %>%
    pull(seriesName)
}


ceic_fetch <- function(id = NULL, 
                       name = NULL, 
                       output = c('df', 'xts'), 
                       rename_cols = NULL) {
  x <- NULL
  
  if (!is.null(id)) {
    x <- .series[id] %>% reduce(~ merge(.x, .y))
  } else if (!is.null(name)) {
    id <- ceic_query(name) %>% pull(id)
    x <- .series[id] %>% reduce(~ merge(.x, .y))
  } else {
    stop('No series IDs or names specified!')
  }
  
  if (!is.null(rename_cols)) {
    names(x) <- rename_cols
  } else {
    names(x) <- id
  }
  
  output <- match.arg(output)
  if (output == 'df') {
    data.frame(index=index(x), x) %>% as_tibble
  } else {
    x
  }
  
}



ceic_init("data.m")




