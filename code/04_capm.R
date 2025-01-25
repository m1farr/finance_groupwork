source("code/00_dependencies.R")
source("code/01_functions.R")
source("code/02_data.R")

# AAPL CAPM --------------------------------------------------

company_returns <- aapl_returns_tbl |> 
  select(names_date, returns, return_on_the_s_p_500_index) |> 
  rename(
    date = names_date,
    company_return = returns,
    sp500_return = return_on_the_s_p_500_index
  )

rf_ret_tbl <- ff_mome_tbl |> 
  select(risk_free_return_rate_one_month_treasury_bill_rate, 
         date_sas_last_trading_day_of_the_month) |> 
  rename(
    date = date_sas_last_trading_day_of_the_month,
    rf_return_rate = risk_free_return_rate_one_month_treasury_bill_rate
  )

aapl_capm_agg <- left_join(company_returns, rf_ret_tbl) |> 
  mutate(
    ri_minus_rf = company_return - rf_return_rate,
    rm_minus_rf = sp500_return - rf_return_rate
  ) |> 
  as.data.frame()

aapl_capm_input <- xts(aapl_capm_agg[,2:6], order.by = as.Date(aapl_capm_agg$date))

# apply rolling regression
capm_rolling_betas <- rollapply(
  aapl_capm_input,
  width = 60,
  FUN = calculate_capm_beta,
  by.column = FALSE,
  align = "right"
)

# organize results
capm_rolling_betas <- as_tibble(capm_rolling_betas)
colnames(capm_rolling_betas) <- c("alpha", "beta")
capm_rolling_betas <- capm_rolling_betas |> 
  filter(!is.na(beta)) |>
  mutate(date = aapl_capm_agg$date[60:nrow(aapl_capm_agg)]) |> 
  relocate(date)

write_xlsx(capm_rolling_betas, path = "output/aapl_capm_rolling_betas.xlsx")

aapl_capm_plot_data <- capm_rolling_betas |> 
  pivot_longer(!date, names_to = "coefficient", values_to = "value")


# graph results
aapl_capm_rolling_plot <- ggplot(aapl_capm_plot_data, aes(x = date, y = value, 
                                                            color = coefficient)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "AAPL CAPM Rolling 5-Year Coefficients",
    subtitle = "2014 to 2023",
    x = "",
    y = ""
  )

ggsave("output/aapl_capm_rolling_plot.png")

aapl_capm_vol <- sqrt(var(capm_rolling_betas$beta))


# Industry CAPM -----------------------------------------------------------

sp_returns <- aapl_returns_tbl |> 
  select(names_date, return_on_the_s_p_500_index)

industry_capm_returns <- industry_ratios_tbl |> 
  cbind(sp_returns) |> 
  select(names_date, value_weighted_industry_return, 
         return_on_the_s_p_500_index) |> 
  rename(
    sp_500_return = return_on_the_s_p_500_index,
    date = names_date
  ) |> 
  left_join(rf_ret_tbl) |> 
  mutate(
    ri_minus_rf = value_weighted_industry_return - rf_return_rate,
    rm_minus_rf = sp_500_return - rf_return_rate
  )

ind_capm_input <- xts(industry_capm_returns[,2:6], order.by = as.Date(industry_capm_returns$date))

# apply rolling regression
ind_capm_betas <- rollapply(
  ind_capm_input,
  width = 60,
  FUN = calculate_capm_beta,
  by.column = FALSE,
  align = "right"
)

# organize results
ind_capm_betas <- as_tibble(ind_capm_betas)
colnames(ind_capm_betas) <- c("alpha", "beta")
ind_capm_betas <- ind_capm_betas |> 
  filter(!is.na(beta)) |>
  mutate(date = industry_capm_returns$date[60:nrow(industry_capm_returns)]) |> 
  relocate(date)

ind_capm_vol <- sqrt(var(ind_capm_betas$beta))

write_xlsx(ind_capm_betas, path = "output/ind_capm_betas.xlsx")

ind_capm_plot_data <- ind_capm_betas |> 
  pivot_longer(!date, names_to = "coefficient", values_to = "value")

ind_capm_rolling_plot <- ggplot(ind_capm_plot_data, aes(x = date, y = value, 
                                                            color = coefficient)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "Industry CAPM Rolling 5-Year Coefficients",
    subtitle = "2014 to 2023",
    x = "",
    y = ""
  ) +
  scale_color_manual(
    name = "",
    labels = c("Alpha", "Beta"),
    values = c("red", "blue"))

ggsave("output/ind_capm_rolling-plot.png")


# Combine CAPM plots ------------------------------------------------------

temp_aapl_capm <- aapl_capm_plot_data |> 
  mutate(coefficient = str_glue("aapl_{coefficient}"))

temp_ind_capm <- ind_capm_plot_data |> 
  mutate(coefficient = str_glue("ind_{coefficient}"))

temp <- rbind(temp_aapl_capm, temp_ind_capm)

final_capm_rolling_plot <- ggplot(temp, aes(x = date, y = value, 
                                                        color = coefficient)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "AAPL vs Industry CAPM Rolling 5-Year Coefficients",
    subtitle = "2014 to 2023",
    x = "",
    y = ""
  ) +
  scale_color_manual(
    name = "Coefficients",
    labels = c("Apple Alpha", "Apple Beta", 
               "Industry Alpha", "Industry  Beta"),
    values = c("orange", "red", "forestgreen", "blue"))

ggsave("output/final_capm_rolling_plot.png")


# AAPL non-rolling CAPM (for appendix) ----------------------------------------------
# define dates for 2x 5 year periods and 1x 4 year period 
end_period_01 <- '2015-01-01'
end_period_02 <- '2020-01-01'

# returns for period 1 - NEED TO MAKE THESE REPRODUCIBLE
capm_input_01 <- aapl_capm_agg |> 
  filter(date < end_period_01)

lm_capm_01 = lm(ri_minus_rf~rm_minus_rf, data = capm_input_01)

summary(lm_capm_01)

# returns for period 2
capm_input_02 <- aapl_capm_agg |> 
  filter(date > end_period_01 & date < end_period_02)

lm_capm_02 = lm(ri_minus_rf~rm_minus_rf, data = capm_input_02)

summary(lm_capm_02)

# returns for period 3
capm_input_03 <- aapl_capm_agg |> 
  filter(date > end_period_02)

lm_capm_03 = lm(ri_minus_rf~rm_minus_rf, data = capm_input_03)

summary(lm_capm_03)


# AAPL CAPM regression, 2010-2023 -----------------------------------------

lm_aapl_capm_agg <- lm(ri_minus_rf ~ rm_minus_rf, data = aapl_capm_agg)
summary(lm_aapl_capm_agg)




