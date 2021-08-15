#' ===========================================================================
#'
#' SCRIPT: Generate all results in the paper
#'
#' AUTHOR: Z. WANG
#'
#' DATE: 12 APRIL 2021
#'
#' NOTICE:  All information contained herein is, and remains the property 
#' of the author. No dissemination or reproduction of this information is 
#' permitted prior permission is obtained from the author.
#'
#' ===========================================================================

setwd("~/GitHub/fc")

library(librarian)
librarian::shelf(
  lubridate, magrittr, rlang, readxl, xts, forecast, 
  slider, patchwork, kableExtra, lmtest, sandwich, broom, 
  latex2exp, BCDating, bHP, pROC, vars, dynlm, stargazer, 
  tidyverse 
)

# ggplot global theme settings
ggplot2::theme_set(
  theme_minimal() + 
  theme(
    legend.title = element_blank(), 
    legend.position = "bottom"
  )
)

R_MAIN = TRUE # indicates this script is executed

# Loading data ---------------------------------------------------------------

source("R/ceic.R")

ceic_hdl <- ceic_load("data/raw.m")
data_raw <- ceic_hdl$fetch_all() 

names(data_raw) <- c("date",
                     "GDP",
                     "CPI",
                     "Credit",
                     "Credit_GDP",
                     "Leverage",
                     "RR",
                     "House")


# Cycle extraction -----------------------------------------------------------

source("R/tstools.R")

# HP filtered cycles
data_bhp <- 
  data_raw %>% 
  drop_na() %>%
  mutate(GDP = 100*BoostedHP(log(GDP)) %$% cycle) %>%
  mutate(CPI = BoostedHP(CPI) %$% cycle) %>%
  mutate(Credit = 100*BoostedHP(log(Credit)) %$% cycle) %>%
  mutate(Credit_GDP = BoostedHP(Credit_GDP) %$% cycle) %>%
  mutate(Leverage = BoostedHP(Leverage) %$% cycle) %>%
  mutate(House = 100*BoostedHP(log(House)) %$% cycle) 

# cyclical working data
data_cyl <- 
  data_bhp %>% 
  select(-Credit_GDP) %>% 
  filter(year(date) > 1997, year(date) < 2020) 

# standardized cycles
data_cyl_std <- 
  data_cyl %>%
  mutate_at(vars(-date), ~ ./sd(.))

# a wrapper over BCDating::BBQ to date business cycles
cycle_dating <- function(x, mincycle=5, minphase=2) {
  bcd <- BCDating::BBQ(x, mincycle, minphase) 
  list(
    peak = as.integer(1:length(x) %in% bcd@peaks),
    trough = as.integer(1:length(x) %in% bcd@troughs),
    state = bcd@states
  )
}

# generate peaks and troughs 
data_peak_trough <-
  data_cyl %>% 
  pivot_longer(!date) %>% 
  arrange(name, date) %>% 
  group_by(name) %>% 
  mutate(peak = cycle_dating(as_ts(value, date)) %$% peak) %>% 
  mutate(trough = cycle_dating(as_ts(value, date)) %$% trough) %>% 
  mutate(state = cycle_dating(as_ts(value, date)) %$% state)

# plotting the cycles together with the turning points
fig_cycles <-
  data_peak_trough %>%
  mutate(peak_a = if_else(peak == 1, value, NA_real_)) %>% 
  mutate(trough_a = if_else(trough == 1, value, NA_real_)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value)) +
  geom_point(aes(y = peak_a), shape = 24, fill = "blue") +
  geom_point(aes(y = trough_a), shape = 21, fill = "red") +
  facet_wrap( ~ name, scales = "free_y") +
  labs(x = NULL, y = NULL) 


# Cyclical feature statistics ------------------------------------------------

sink("/dev/null") # suppress output
bc <- list() # a list to store all business cycle statistics
bc$gdp <- summary(BBQ(as_ts(data_cyl$GDP, data_cyl$date)))
bc$cpi <- summary(BBQ(as_ts(data_cyl$CPI, data_cyl$date)))
bc$rr <- summary(BBQ(as_ts(data_cyl$RR, data_cyl$date)))
bc$cr <- summary(BBQ(as_ts(data_cyl$Credit, data_cyl$date)))
bc$hh <- summary(BBQ(as_ts(data_cyl$House, data_cyl$date)))
bc$lv <- summary(BBQ(as_ts(data_cyl$Leverage, data_cyl$date)))
sink() # restore output device

# table of amplitude and duration
tbl_cycles <-
  tribble(
    ~Indicator, ~Measure, ~Upturn, ~Downturn, ~`Full Cycle`,
    "GDP"     , "Amplitude", bc$gdp[1,1], bc$gdp[2,1], NA_real_, 
    "GDP"     , "Duration" , bc$gdp[1,2], bc$gdp[2,2], bc$gdp[1,2]+bc$gdp[2,2], 
    "CPI"     , "Amplitude", bc$cpi[1,1], bc$cpi[2,1], NA_real_, 
    "CPI"     , "Duration" , bc$cpi[1,2], bc$cpi[2,2], bc$cpi[1,2]+bc$cpi[2,2],
    "Credit"  , "Amplitude", bc$cr [1,1], bc$cr [2,1], NA_real_,
    "Credit"  , "Duration" , bc$cr [1,2], bc$cr [2,2], bc$cr [1,2]+bc$cr [2,2], 
    "Leverage", "Amplitude", bc$lv [1,1], bc$lv [2,1], NA_real_,
    "Leverage", "Duration" , bc$lv [1,2], bc$lv [2,2], bc$lv [1,2]+bc$lv [2,2], 
    "House Price"  , "Amplitude", bc$hh[1,1], bc$hh[2,1], NA_real_,
    "House Price"  , "Duration" , bc$hh[1,2], bc$hh[2,2], bc$hh[1,2]+bc$hh[2,2], 
    "Interest Rate", "Amplitude", bc$rr[1,1], bc$rr[2,1], NA_real_,
    "Interest Rate", "Duration" , bc$rr[1,2], bc$rr[2,2], bc$rr[1,2]+bc$rr[2,2], 
  )

# correlation between variables
fig_corr <- data_cyl %>% 
    GGally::ggpairs(columns = 2:ncol(data_cyl))

# store states of the series: 1 for expansion; 0 for contraction
data_states <-
  data_peak_trough %>%
  group_by(name) %>%
  mutate(upturn = if_else(state == 1, 1, 0)) %>%
  ungroup() %>% select(date, name, upturn) %>%
  pivot_wider(names_from = name, values_from = upturn) %>%
  select(date, GDP, CPI, Credit, House, Leverage, RR)
  
# concordance table
tbl_concord <- 
  data_states %>%
    select(-date) %>%
    map(function(each, others) {
      map_dbl(others, ~ sum(.x*.y + (1-.x)*(1-.y), na.rm = T)/length(.x), each)
    }, data_states %>% select(-date)) %>%
    as_tibble(rownames = NA) %>% 
  as.data.frame()
rownames(tbl_concord) <- names(tbl_concord)



# Event study ----------------------------------------------------------------

# data for event study in wide format
data_event_study <- 
  data_peak_trough %>% 
  select(date, name, value, peak, trough) %>% 
  pivot_wider(names_from = name, values_from = c(value,peak,trough)) %>% 
  rename_with(~gsub("value_", "", .) ,contains("value_"))

#' Plot impulse response from local projection coefficients
#'
#' @param lm_results a list of `lm` models
#' @param horizon time horizon to plot (e.g. 0:5)
#' @param ref plot reference curve (constant term)
#' @param title title of the plot
#'
#' @return a ggplot object
#' @remark Intercept and the parameter of interest must be the 
#'         first two coefficients in regression models
plot_irfs <- function(lm_results, horizon, ref=TRUE, title=NULL) {
  lm_results %>% 
    map_dfr(~ .$coefficients[1:2]) %>% 
    rename_with(~c("const", "beta")) %>% 
    mutate(actual = const + beta) %>% 
    ggplot(aes(x = horizon)) +
    geom_line(aes(y = actual, linetype = "Actual", color = "Actual")) +
    geom_line(aes(y = const, linetype = "Reference", color = "Reference")) +
    geom_vline(xintercept = 0, col = "grey") +
    geom_hline(yintercept = 0, col = "grey") +
    scale_x_continuous(breaks = horizon) +
    scale_y_continuous(limits = c(-2,2)) +
    labs(x = title, y = NULL) +
    guides(color = FALSE) +
    theme(aspect.ratio = 0.8) 
}

fig_es <- list() # store all event study sub-plots
horizon <- 0:6
fig_es$credit_peak <-
  map(horizon, ~lm(lead(GDP,.) ~ peak_Credit, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after credit cycle peaks")
fig_es$credit_trough <- 
  map(horizon, ~lm(lead(GDP,.) ~ trough_Credit, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after credit cycle troughs")
fig_es$house_peak <-
  map(horizon, ~lm(lead(GDP,.) ~ peak_House, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after house price peaks")
fig_es$house_trough <- 
  map(horizon, ~lm(lead(GDP,.) ~ trough_House, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after house price troughs")
fig_es$leverage_peak <-
  map(horizon, ~lm(lead(GDP,.) ~ peak_Leverage, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after leverage cycle peaks")
fig_es$leverage_trough <- 
  map(horizon, ~lm(lead(GDP,.) ~ trough_Leverage, data_event_study)) %>% 
  plot_irfs(horizon, title = "Quarters after leverage cycle troughs")

# patch all sub-plot into one 
fig_es_all <-
  fig_es$credit_peak + 
  fig_es$credit_trough +
  fig_es$house_peak + 
  fig_es$house_trough +
  fig_es$leverage_peak + 
  fig_es$leverage_trough + 
  plot_layout(ncol = 2, guides = "collect")



# Local projection with controls ---------------------------------------------

source("R/summary.R")

# data for local projection; in wide format
data_lp <- 
  data_peak_trough %>% 
  select(date, name, value, peak, trough) %>% 
  pivot_wider(names_from = name, values_from = c(value,peak,trough)) %>% 
  rename_with(~gsub("value_", "", .) ,contains("value_"))

# control variables in all regressions
controls <- ~. +lag(GDP,1)+lag(GDP,2)+CPI + lag(CPI, 1)+RR + lag(RR,1)+
  Credit + lag(Credit, 1) + House + lag(House,1) 
horizon <- 0:6

lp_credit_peak <- map(horizon, ~ lm(update(lead(GDP,.x) ~ peak_Credit, controls), data_lp)) 
lp_credit_trough <- map(horizon, ~ lm(update(lead(GDP,.x) ~ trough_Credit, controls), data_lp)) 
lp_house_peak <- map(horizon, ~ lm(update(lead(GDP,.x) ~ peak_House, controls), data_lp)) 
lp_house_trough <- map(horizon, ~ lm(update(lead(GDP,.x) ~ trough_House, controls), data_lp)) 
lp_leverage_peak <- map(horizon, ~ lm(update(lead(GDP,.x) ~ peak_Leverage, controls), data_lp)) 
lp_leverage_trough <- map(horizon, ~ lm(update(lead(GDP,.x) ~ trough_Leverage, controls), data_lp)) 

## LP: plotting -------------------------------------------------------------

fig_lp <- list() # store subplots
fig_lp$credit_peak <- lp_credit_peak %>% 
  plot_irfs(horizon, title = "Quarters after credit cycle peaks")
fig_lp$credit_trough <- lp_credit_trough %>% 
  plot_irfs(horizon, title = "Quarters after credit cycle troughs")
fig_lp$house_peak <- lp_house_peak %>% 
  plot_irfs(horizon, title = "Quarters after house price peaks")
fig_lp$house_trough <- lp_house_trough %>% 
  plot_irfs(horizon, title = "Quarters after house price troughs")
fig_lp$leverage_peak <- lp_leverage_peak %>% 
  plot_irfs(horizon, title = "Quarters after leverage cycle peaks")
fig_lp$leverage_trough <- lp_leverage_trough %>% 
  plot_irfs(horizon, title = "Quarters after leverage cycle troughs")

# patch all sub-plot into one 
fig_lp_all <-
  fig_lp$credit_peak + 
  fig_lp$credit_trough +
  fig_lp$house_peak + 
  fig_lp$house_trough +
  fig_lp$leverage_peak + 
  fig_lp$leverage_trough + 
  plot_layout(ncol = 2, guides = "collect")

## LP: Tabulation -----------------------------------------------------------

# produce summary tables for regression results
tbl_credit <- do.call(smart_summary, args = c(
  lp_credit_peak  [2:7],
  lp_credit_trough[2:7],
  list(
    coefs = c("(Intercept)", "peak_Credit", "trough_Credit"),
    rename = ~ case_when(
      . == '(Intercept)' ~ '$\\mu$',
      . == 'peak_Credit' ~ '$\\delta^{(P)}$',
      . == 'trough_Credit' ~ '$\\delta^{(T)}$'
    ),
    vcov = sandwich::NeweyWest,
    stats = c("r.squared")
  )
))

tbl_house <- do.call(smart_summary, args = c(
  lp_house_peak  [2:7],
  lp_house_trough[2:7],
  list(
    coefs = c("(Intercept)", "peak_House", "trough_House"),
    rename = ~ case_when(
      . == '(Intercept)' ~ '$\\mu$',
      . == 'peak_House' ~ '$\\delta^{(P)}$',
      . == 'trough_House' ~ '$\\delta^{(T)}$'
    ),
    vcov = sandwich::NeweyWest,
    stats = c("r.squared")
  )
))

tbl_leverage <- do.call(smart_summary, args = c(
  lp_leverage_peak  [2:7],
  lp_leverage_trough[2:7],
  list(
    coefs = c("(Intercept)", "peak_Leverage", "trough_Leverage"),
    rename = ~ case_when(
      . == '(Intercept)' ~ '$\\mu$',
      . == 'peak_Leverage' ~ '$\\delta^{(P)}$',
      . == 'trough_Leverage' ~ '$\\delta^{(T)}$'
    ),
    vcov = sandwich::NeweyWest,
    stats = c("r.squared")
  )
))


# Recession forecasting ------------------------------------------------------

# load recession dummies
data_rec <-
  read_csv("data/CHNREC.csv", col_types = "cn") %>%
  select(date = DATE, rec = CHNREC) %>%
  mutate(rec = as.integer(rec)) %>%
  mutate(date = ymd(date) + months(2)) %>%
  # moving average of last 2 years
  right_join(data_bhp %>%
               mutate_at(vars(!date), ~ slide_dbl(
                 .x, mean, .before = 7, .complete = T)) , 
             by = "date") %>%
  select(date, rec, GDP, CPI, Credit, House, Leverage, RR) %>% 
  filter(year(date) > 1998, year(date) < 2020)

# plot graph with shaded recession areas
fig_rec <-
  data_peak_trough %>%
  left_join(data_rec %>%
              select(date, rec), by = "date") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value)) +
  geom_rect(
    aes(
      xmin = date,
      xmax = date + months(3),
      ymin = -Inf,
      ymax = Inf,
      fill = factor(rec)
    ),
    color = NA,
    alpha = 0.3
  ) +
  scale_fill_brewer(palette = 2) +
  facet_wrap( ~ name, scales = "free_y") +
  labs(x = NULL, y = NULL)


## Plot ROC (Receiver Operating Characteristic) ------------------------------

# ROC for simple linear model with only one predictor
rec_roc <- list(
  house = pROC::roc(data_rec$rec, lm(rec ~ Credit, data_rec)$fitted.values, plot=F, quiet=T),
  credit = pROC::roc(data_rec$rec, lm(rec ~ House, data_rec)$fitted.values, plot=F, quiet=T),
  leverage = pROC::roc(data_rec$rec, lm(rec ~ Leverage, data_rec)$fitted.values, plot=F, quiet=T)
)

# plot ROC curves
fig_rec_roc <- 
  bind_cols(
    tibble(FPR_House = 1 - rec_roc$house$specificities, TPR_House = rec_roc$house$sensitivities), 
    tibble(FPR_Credit = 1 - rec_roc$credit$specificities, TPR_Credit = rec_roc$credit$sensitivities),
    tibble(FPR_Leverage = 1 - rec_roc$leverage$specificities, TPR_Leverage = rec_roc$leverage$sensitivities)
  ) %>% 
  ggplot() + 
  geom_line(aes(FPR_House, TPR_House, col = "House")) + 
  geom_line(aes(FPR_Credit, TPR_Credit, col = "Credit")) + 
  geom_line(aes(FPR_Leverage, TPR_Leverage, col = "Leverage")) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(x = "FPR", y = "TPR") +
  coord_fixed() 


## Regression models ----------------------------------------------------------

# linear models
rec_lm <- list(
  base = lm(rec ~ GDP + CPI + RR, data_rec), 
  house = lm(rec ~ House + GDP + CPI + RR, data_rec),
  credit = lm(rec ~ Credit + GDP + CPI + RR, data_rec),
  leverage = lm(rec ~ Leverage + GDP + CPI + RR, data_rec) 
)
# probit models
rec_probit <- list(
  base = glm(rec ~ GDP + CPI + RR, binomial(link = "probit"), data_rec), 
  house = glm(rec ~ House + GDP + CPI + RR, binomial(link = "probit"), data_rec), 
  credit = glm(rec ~ Credit + GDP + CPI + RR, binomial(link = "probit"), data_rec), 
  leverage = glm(rec ~ Leverage + GDP + CPI + RR, binomial(link = "probit"), data_rec) 
)

# generate report table
tbl_rec <- 
  smart_summary(
    rec_lm$base,
    rec_lm$credit,
    rec_lm$house,
    rec_lm$leverage,
    rec_probit$base,
    rec_probit$credit,
    rec_probit$house,
    rec_probit$leverage,
    excls = c("Intercept"),
    add_stats = list(AUROC = ~ pROC::auc(data_rec$rec, .x$fitted.values, quiet=T))
  ) %>% 
  rename("2-year moving average" = ".") %>% 
  slice(4:6, 1:3, 7:9)
