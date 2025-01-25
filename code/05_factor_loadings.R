source("code/00_dependencies.R")
source("code/01_functions.R")
source("code/02_data.R")


# AAPL FF3FM --------------------------------------------------------------

aapl_ff3fm <- aapl_returns_tbl |> 
  select(returns, return_on_the_s_p_500_index) |> 
  cbind(ff_mome_tbl) |> 
  select(-momentum_factor) |> 
  select(-excess_return_on_the_market) |> 
  rename(
    date = date_sas_last_trading_day_of_the_month,
    aapl_returns = returns,
    sp500_return = return_on_the_s_p_500_index,
    smb_ret = small_minus_big_return,
    hml_ret = high_minus_low_return,
    rf_rate = risk_free_return_rate_one_month_treasury_bill_rate
  ) |> 
  relocate(date) |> 
  mutate(aapl_excess_ret = aapl_returns - rf_rate,
         sp_excess_ret = sp500_return - rf_rate) |> 
  select(-aapl_returns) |>
  select(-sp500_return) |> 
  select(-rf_rate)

aapl_ff3fm_input <- xts(aapl_ff3fm[,2:5],order.by = as.Date(aapl_ff3fm[,1]))

# apply rolling regression
rolling_betas <- rollapply(
  aapl_ff3fm_input,
  width = 60,
  FUN = calculate_betas,
  by.column = FALSE,
  align = "right"
)

# organize results
rolling_betas <- as_tibble(rolling_betas)
colnames(rolling_betas) <- c("alpha", "beta_mkt_rf", "beta_smb", "beta_hml")
rolling_betas <- rolling_betas |> 
  filter(!is.na(alpha)) |>
  mutate(date = aapl_ff3fm$date[60:nrow(aapl_ff3fm)]) |> 
  relocate(date)

write_xlsx(rolling_betas, path = "output/aapl_ff3fm_coefficients.xlsx")

aapl_ff3fm_plot_data <- rolling_betas |> 
  pivot_longer(!date, names_to = "beta", values_to = "value")


# graph results
aapl_ff3fm_rolling_plot <- ggplot(aapl_ff3fm_plot_data, aes(x = date, y = value, 
                                                            color = beta)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "AAPL FF3FM Rolling 5-Year Coefficients",
    subtitle = "2014 to 2023",
    x = "",
    y = ""
  ) +
  scale_color_manual(
    name = "",
    labels = c("Alpha", "Beta - HML", "Beta - Mkt", "Beta - SMB"),
    values = c("red", "forestgreen", "blue", "purple"))

ggsave("output/aapl_ff3fm_rolling_plot.png")


# industry FF3FM ----------------------------------------------------------
sp_returns <- aapl_returns_tbl |> 
  select(return_on_the_s_p_500_index)

ind_ff3fm <- industry_ratios_tbl |> 
  select(value_weighted_industry_return) |> 
  cbind(sp_returns) |> 
  cbind(ff_mome_tbl) |> 
  select(-momentum_factor) |> 
  select(-excess_return_on_the_market) |> 
  rename(
    date = date_sas_last_trading_day_of_the_month,
    ind_return = value_weighted_industry_return,
    sp500_ret = return_on_the_s_p_500_index,
    smb_ret = small_minus_big_return,
    hml_ret = high_minus_low_return,
    rf_rate = risk_free_return_rate_one_month_treasury_bill_rate
  ) |> 
  relocate(date) |> 
  mutate(ind_excess_ret = ind_return - rf_rate,
         sp_excess_ret = sp500_ret - rf_rate) |> 
  select(-ind_return) |> 
  select(-sp500_ret) |> 
  select(-rf_rate)

ind_ff3fm_input <- xts(ind_ff3fm[,2:5],order.by = as.Date(ind_ff3fm[,1]))

# apply rolling regression
ind_rolling_betas <- rollapply(
  ind_ff3fm_input,
  width = 60,
  FUN = calculate_ind_betas,
  by.column = FALSE,
  align = "right"
)

# organize results
ind_rolling_betas <- as_tibble(ind_rolling_betas)
colnames(ind_rolling_betas) <- c("alpha", "beta_mkt_rf", "beta_smb", "beta_hml")
ind_rolling_betas <- ind_rolling_betas |> 
  filter(!is.na(alpha)) |>
  mutate(date = ind_ff3fm$date[60:nrow(ind_ff3fm)]) |> 
  relocate(date)

write_xlsx(ind_rolling_betas, path = "output/ind_ff3fm_coefficients.xlsx")

ind_ff3fm_plot_data <- ind_rolling_betas |> 
  pivot_longer(!date, names_to = "beta", values_to = "value")


# graph results
ind_ff3fm_rolling_plot <- ggplot(ind_ff3fm_plot_data, aes(x = date, y = value, 
                                                            color = beta)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "Industry FF3FM Rolling 5-Year Coefficients",
    subtitle = "2014 to 2023",
    x = "",
    y = ""
  ) +
  scale_color_manual(
    name = "",
    labels = c("Alpha", "Beta - HML", "Beta - Mkt", "Beta - SMB"),
    values = c("red", "forestgreen", "blue", "purple"))

ggsave("output/industry_ff3fm_rolling_plot.png")

# AAPL non-rolling FF3FM (for appendix) ----------------------------------------------
# define dates for 2x 5 year periods and 1x 4 year period 
end_period_01 <- '2015-01-01'
end_period_02 <- '2020-01-01'

# returns for period 1 - NEED TO MAKE THESE REPRODUCIBLE
ff3fm_input_01 <- aapl_ff3fm |> 
  filter(date < end_period_01)

lm_ff3fm_01 = lm(aapl_excess_ret ~ sp_excess_ret+smb_ret+hml_ret, data = ff3fm_input_01)

summary(lm_ff3fm_01)

# returns for period 2
ff3fm_input_02 <- aapl_ff3fm |> 
  filter(date > end_period_01 & date < end_period_02)

lm_ff3fm_02 = lm(aapl_excess_ret ~ sp_excess_ret+smb_ret+hml_ret, data = ff3fm_input_02)

summary(lm_ff3fm_02)

# returns for period 3
ff3fm_input_03 <- aapl_ff3fm |> 
  filter(date > end_period_02)

lm_ff3fm_03 = lm(aapl_excess_ret ~ sp_excess_ret+smb_ret+hml_ret, data = ff3fm_input_03)

summary(lm_ff3fm_03)

# AAPL FF3FM Regression, 2010-2023 ----------------------------------------

lm_aapl_ff3fm_agg <- lm(aapl_excess_ret ~ sp_excess_ret+smb_ret+hml_ret, data = aapl_ff3fm)
summary(lm_aapl_ff3fm_agg)


