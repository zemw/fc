source("ceic.R")

cki_m <- ceic_load("../data/CKI_M.m") # monthly 
cki_q <- ceic_load("../data/CKI_Q.m") # quarterly

monthly_series <- list(
  CPI = "5716201",    # CPI MoM
  R007 = "7061001",   # Interbank 7-day repo rate
  House = "4006901",   # Commodity Bldg Selling Price: YTD Average
  AFRE = "365867277"  # Aggregate Financing to the Real Economy
)

cki_m$fetch(
  seriesId = as.character(monthly_series), 
  rename_cols = names(monthly_series)
  ) %>%
  # convert MoM difference to index
  mutate(CPI = CPI - 100) %>%
  mutate(CPI = cumsum(CPI) + 100) %>%
  # convert monthly data to quarterly
  condense_period(
    .date_var = date, 
    .period = "quarter", 
    .side = "end"
  ) -> data_m

quarterly_series <- list(
  GDP = "369703417", 
  Credit = "371938917"  # Credit to Non-financial Sector
)

cki_q$fetch(
  seriesId = as.character(quarterly_series), 
  rename_cols = names(quarterly_series)
  ) %>%
  mutate(`Credit/GDP` = Credit/GDP) -> data_q

# combine all data: un-adjusted time series 
data_raw <- 
  full_join(data_m, data_q, by = "date") %>%
  select(date, GDP, CPI, House, Credit, `Credit/GDP`, R007) %>% 
  filter_by_time(date, .start_date = "1999-03", .end_date = "2019-12") 


# test the correlation between BIS credit and AFRE
data_tmp <- full_join(data_m, data_q, by = "date") %>%
  select(date, Credit, AFRE) %>%
  filter_by_time(
    .date_var = date, 
    .start_date = "2014-03", 
    .end_date = "2019-12"
  ) 
cor(data_tmp$Credit, data_tmp$AFRE) # 0.994411


# extract cyclical components from the time series
data_cyl <- data_raw %>%
  # log before or after seasonal adjustment are almost identical
  mutate_at(vars(GDP, Credit, House), ~log(.)) %>%
  mutate_at(vars(-date), ~ x11_adjust(., index = date) %$% final) %>%
  mutate_at(vars(-date), ~ hp_filter(., .lambda = 1600) %$% cycle)

# standardize the cycles by standard deviation (mean = 0)
data_cyl_std <- data_cyl %>%
  mutate_at(vars(-date), ~ ./sd(.))
