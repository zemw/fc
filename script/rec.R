
source("script/summary.R")
source("script/ceic.R")

ceic_hdl <- ceic_load("data/raw.m")
data_raw <- ceic_hdl$fetch_all() 

names(data_raw) <- c("date",
                     "GDP",
                     "CPI",
                     "Credit",
                     "Credit/GDP",
                     "Leverage",
                     "RR",
                     "CBSP")

# first-order difference
data_diff <- data_raw %>% 
  mutate(GDP = diff_vec(GDP, log = T)*100) %>% 
  mutate(Credit = diff_vec(Credit, log = T)*100) %>% 
  mutate(House = diff_vec(CBSP, log = T)*100) %>% 
  mutate(Leverage = diff_vec(Leverage)) %>% 
  mutate(CPI = diff_vec(CPI)) %>% 
  #mutate(RR = diff_vec(RR)) %>% 
  select(-`Credit/GDP`, -CBSP) %>% 
  drop_na()

data_bhp <- data_raw %>% drop_na() %>%
  mutate(GDP = 100*BoostedHP(log(GDP)) %$% cycle) %>%
  mutate(CPI = BoostedHP(CPI) %$% cycle) %>%
  mutate(Credit = 100*BoostedHP(log(Credit)) %$% cycle) %>%
  mutate(`Credit/GDP` = BoostedHP(`Credit/GDP`) %$% cycle) %>%
  mutate(Leverage = BoostedHP(Leverage) %$% cycle) %>%
  mutate(House = 100*BoostedHP(log(CBSP)) %$% cycle) %>% 
  select(-`Credit/GDP`, -CBSP) 

# moving average of last 2 years
data_ma2 <- data_bhp %>% 
  mutate_at(vars(!date), ~slide_dbl(.x, mean, .before = 7, .complete = T)) 

# load recession dummies
data_rec <- 
  read_csv("data/CHNREC.csv") %>%
  select(date = DATE, rec = CHNREC) %>% 
  mutate(rec = as.integer(rec)) %>% 
  mutate(date = ymd(date) + months(2)) %>% 
  right_join(data_ma2, by ="date") %>% 
  filter(year(date) > 1998, year(date) < 2020)

# plot graph with shaded recession areas
plot_rec <-
  data_rec %>%
  pivot_longer(3:8) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap( ~ name, scales = "free_y") +
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
  scale_fill_brewer(palette = 2) 

# plot with peaks and troughs
plot_rec_pt <-
  data_peak_trough %>%
  left_join(data_rec %>% select(date, rec), by = "date") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value)) +
  geom_point(aes(y = peak), shape = 24, fill = "blue") +
  geom_point(aes(y = trough), shape = 21, fill = "red") +
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
  facet_wrap( ~ indicator, scales = "free_y") +
  labs(x = NULL, y = NULL)

# # generate data with lags
# data_rec %<>% 
#   pivot_longer(!date) %>% 
#   group_by(name) %>% 
#   arrange(name) %>% 
#   # generate lags L1, L2, L3, L4
#   mutate(map_dfc(1:4, ~ dplyr::lag(value, .))) %>% 
#   rename_with(~gsub("...", "L", .), contains("...")) %>% 
#   pivot_wider(values_from = 3:7) %>% 
#   rename_with(~gsub("value_", "", .), contains("value")) %>% 
#   filter(year(date) < 2020) %>% 
#   drop_na()

# ----------------------------------------------------------------------------
# Plot ROC (Receiver Operating Characteristic)
# ----------------------------------------------------------------------------

fit_rec_credit <- lm(rec ~ Credit, data_rec)
fit_rec_house <- lm(rec ~ House, data_rec)  
fit_rec_leverage <- lm(rec ~ Leverage, data_rec)

roc_house <- roc(data_rec$rec, fit_rec_house$fitted.values, plot = F)
roc_credit <- roc(data_rec$rec, fit_rec_credit$fitted.values, plot = F)
roc_leverage <- roc(data_rec$rec, fit_rec_leverage$fitted.values, plot = F)

plot_roc <- 
  bind_cols(
  tibble(FPR_House = 1 - roc_house$specificities, TPR_House = roc_house$sensitivities), 
  tibble(FPR_Credit = 1 - roc_credit$specificities, TPR_Credit = roc_credit$sensitivities),
  tibble(FPR_Leverage = 1 - roc_leverage$specificities, TPR_Leverage = roc_leverage$sensitivities)
  ) %>% 
  ggplot() + 
  geom_line(aes(FPR_House, TPR_House, col = "House")) + 
  geom_line(aes(FPR_Credit, TPR_Credit, col = "Credit")) + 
  geom_line(aes(FPR_Leverage, TPR_Leverage, col = "Leverage")) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(x = "FPR", y = "TPR") +
  coord_fixed() 


# ----------------------------------------------------------------------------
# Regression models
# ----------------------------------------------------------------------------

lm_rec_base <- lm(rec ~ GDP + CPI + RR, data_rec) 
lm_rec_house <- lm(rec ~ House + GDP + CPI + RR, data_rec) 
lm_rec_credit <- lm(rec ~ Credit + GDP + CPI + RR, data_rec) 
lm_rec_leverage <- lm(rec ~ Leverage + GDP + CPI + RR, data_rec) 

probit_rec_base <- glm(rec ~ GDP + CPI + RR, binomial(link = "probit"), data_rec) 
probit_rec_house <- glm(rec ~ House + GDP + CPI + RR, binomial(link = "probit"), data_rec) 
probit_rec_credit <- glm(rec ~ Credit + GDP + CPI + RR, binomial(link = "probit"), data_rec) 
probit_rec_leverage <- glm(rec ~ Leverage + GDP + CPI + RR, binomial(link = "probit"), data_rec) 

# generate report table
table_rec <- 
  smart_summary(
    lm_rec_base,
    lm_rec_credit,
    lm_rec_house,
    lm_rec_leverage,
    probit_rec_base,
    probit_rec_credit,
    probit_rec_house,
    probit_rec_leverage,
    excls = c("Intercept"),
    add_stats = list(AUROC = ~ auc(data_rec$rec, .x$fitted.values))
    ) %>% 
  rename("2-year moving average" = ".") %>% 
  slice(4:6, 1:3, 7:9)



