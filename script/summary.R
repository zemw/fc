#' Summarize the result of a regression model into a print-ready table.
#'
#' @param fit an lm model
#' @param coefs regressors whose coefficients are to keep in the output table
#' @param rename rename the regressors; a vector of strings, or a function that 
#'               takes a current names of regressors and returns the new name.
#' @param label the column name of the model
#' @param digits number of digits of the output numbers
#' @param vcov variance-covariance matrix from sandwich
#' @param stats summary statistics to report in the output table; 
#'              names are the same as broom::glance
#' @param add_stats add other statistics; list(name = function(.fit)) 
#' @param add_lines add other rows; list(name = value)
#' 
.smart_summary <- function(fit,
                           coefs,
                           rename = NULL,
                           label = "*",
                           digits = 3,
                           vcov = NULL,
                           stats = c("r.squared", "nobs"),
                           add_stats = list(),
                           add_lines = list()) {
  
  res <- tidy(coeftest(fit, vcov. = vcov)) %>%
    filter(grepl(str_c(coefs, collapse = "|"), term)) %>%
    mutate(value = paste(
      round(estimate, digits),
      case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      ),
      sprintf("(%s)", round(std.error, digits))
    ))
  
  # rename coefficients
  if (!is.null(rename)) {
    if (class(rename) == "character") {
      res %<>% mutate(name = rename)
    } else {
      res %<>% mutate(name = map_chr(term, as_mapper(rename)))
    }
  }
  
  res %<>% select(name, value)
  
  # append additional information
  if (is_list(add_lines) && length(add_lines) > 0) {
    res %<>% add_row(name = names(add_lines),
                     value = as.character(add_lines))
  }
  
  # append additional statistics
  if (is_list(add_stats) && length(add_stats) > 0) {
    res %<>% add_row(name = names(add_stats),
                     value = map_chr(add_stats, function(.fn) {
                       as_mapper(.fn) %>%
                         invoke(., list(fit)) %>%
                         round(., digits) %>%
                         format(.)
                     }))
  }
  
  # append summary statistics
  gln <- glance(fit)
  
  if ("r.squared" %in% stats) {
    res %<>% add_row(name = "$\\R^2$",
                     value = format(round(pull(gln, "r.squared"), digits)))
  }
  if ("adj.r.squared" %in% stats) {
    res %<>% add_row(name = "Adj. $\\R^2$",
                     value = format(round(pull(gln, "adj.r.squared"), digits)))
  }
  if ("nobs" %in% stats) {
    res %<>% add_row(name = "$N$",
                     value = format(round(pull(gln, "nobs"), digits)))
  }
  
  # finally, rename columns
  names(res) <- c(".", label)
  
  return(res)
}

#' Summarize the results of regression models into a print-ready table.
#'
#' @param ... models to summarize
#' 
smart_summary <- function(...,
                          coefs,
                          rename = NULL,
                          label = "(1)",
                          digits = 3,
                          vcov = NULL,
                          stats = c("r.squared", "nobs"),
                          add_stats = list(),
                          add_lines = list()) {
  
  fits <- list(...)
  sums <- fits %>% imap(function(.fit, .index) {
    .smart_summary(
      .fit,
      coefs,
      rename,
      sprintf("(%d)", .index),
      # label
      digits,
      vcov,
      stats,
      add_stats,
      add_lines
    )
  }) 
  res <- sums %>% reduce(~full_join(.x, .y, by = "."))
  # make sure the summary statistics to the bottom of the table
  S <- length(stats) + length(add_stats) + length(add_lines)
  L <- nrow(sums[[1]])
  N <- nrow(res)
  res %<>% slice(1:(L-S), (L+1):N, (L-S+1):L)
  return(res)
}

## Example
# smart_summary(fit_rec_base, fit_rec_credit, fit_rec_house, fit_rec_leverage,
#               coefs = c("GDP", "Credit", "House", "Leverage"), 
#               rename = ~gsub("_", ".", .), 
#               add_stats = list(AUC = ~auc(data_rec$rec, .x$fitted.values))) 
