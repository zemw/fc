source("script/summary.R")
# rm(list = ls(all = TRUE))

# load recession dummies
data_rec <- 
  read_csv("data/CHNREC.csv") %>%
  select(date = DATE, rec = CHNREC) %>% 
  mutate(rec = as.integer(rec)) %>% 
  mutate(date = ymd(date) + months(2)) %>% 
  right_join(data_diff, by ="date") 

# plot graph with shaded recession areas
plot_rec_1 <-
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
plot_rec_2 <-
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

# data with lags
data_rec %<>% 
  pivot_longer(!date) %>% 
  group_by(name) %>% 
  arrange(name) %>% 
  # generate lags L1, L2, ...
  mutate(map_dfc(1:5, ~ dplyr::lag(value, .))) %>% 
  rename_with(~gsub("...", "L", .), contains("...")) %>% 
  pivot_wider(values_from = 3:8) %>% 
  rename_with(~gsub("value_", "", .), contains("value")) %>% 
  filter(year(date) > 1998, year(date) < 2020)

# ----------------------------------------------------------------------------
# Plot ROC (Receiver Operating Characteristic)
# ----------------------------------------------------------------------------

fit_rec_house <- lm(rec ~ L1_House + L2_House + L3_House + L4_House, data_rec)  
fit_rec_credit <- lm(rec ~ L1_Credit + L2_Credit + L3_Credit + L4_Credit, data_rec)
fit_rec_leverage <- lm(rec ~ L1_Leverage + L2_Leverage + L3_Leverage + L4_Leverage, data_rec)

roc_house <- roc(data_rec$rec, fit_rec_house$fitted.values, plot = T)
roc_credit <- roc(data_rec$rec, fit_rec_credit$fitted.values, plot = T)
roc_leverage <- roc(data_rec$rec, fit_rec_leverage$fitted.values, plot = T)

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

fit_rec_base <- lm(rec ~ L1_GDP + L2_GDP + L3_GDP + L4_GDP + L1_CPI + L2_CPI + L3_CPI + L4_CPI + L1_RR + L2_RR + L3_RR + L4_RR, data_rec) 
fit_rec_house <- lm(rec ~ L1_House + L2_House + L3_House + L4_House + L1_GDP + L2_GDP + L3_GDP + L4_GDP + L1_CPI + L2_CPI + L3_CPI + L4_CPI + L1_RR + L2_RR + L3_RR + L4_RR, data_rec) 
fit_rec_credit <- lm(rec ~ L1_Credit + L2_Credit + L3_Credit + L4_Credit + L1_GDP + L2_GDP + L3_GDP + L4_GDP + L1_CPI + L2_CPI + L3_CPI + L4_CPI + L1_RR + L2_RR + L3_RR + L4_RR, data_rec) 
fit_rec_leverage <- lm(rec ~ L1_Leverage + L2_Leverage + L3_Leverage + L4_Leverage + L1_GDP + L2_GDP + L3_GDP + L4_GDP + L1_CPI + L2_CPI + L3_CPI + L4_CPI + L1_RR + L2_RR + L3_RR + L4_RR, data_rec) 

table_rec <-
  smart_summary(
    fit_rec_base,
    fit_rec_credit,
    fit_rec_house,
    fit_rec_leverage,
    coefs = c("GDP", "Credit", "House", "Leverage"),
    rename = ~ gsub("_", ".", .),
    add_lines = list(Controls = "Yes"),
    add_stats = list(AUC = ~ auc(data_rec$rec, .x$fitted.values))
  ) 

# probit_rec_base <- glm(rec ~ L1_GDP + L2_GDP + L3_GDP + L4_GDP + L1_CPI + L2_CPI + L3_CPI + L4_CPI + L1_RR + L2_RR + L3_RR + L4_RR, binomial(link = "probit"), data_rec) 



