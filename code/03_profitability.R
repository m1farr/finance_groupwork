# task 1: metrics (profitability: NPM) ---------------------------------------------------------

# get NPM for AAPL and industry (Information Technology)
industry_npm <- get_npm(industry_ratios_tbl, net_profit_margin_median)
aapl_npm <- get_npm(aapl_ratios_tbl, net_profit_margin)

# combine NPM data in 1 table to create graph
npm_data <- left_join(industry_npm, aapl_npm) |> 
   pivot_longer(!date, names_to = "metric", values_to = "npm")

# graph NPM
npm_plot <- ggplot(npm_data, aes(x = date, y = npm, color = metric)) +
  geom_line() +
  theme_bw() + 
  labs(
    title = "Apple vs Industry: Net Profit Margin",
    subtitle = "End of Month Reports from 2010 to 2023",
    x = "",
    y = "Net Profit Margin"
  ) + 
  scale_color_manual(
    name = "Key",
    labels = c("Apple", "Industry (Median)"), 
    values = c("red", "blue"))

# save NPM graph as .png
ggsave("output/npm_plot.png")

# get values for comparison
aapl_npm_median <- median(aapl_npm$net_profit_margin)
industry_npm_median <- median(industry_npm$net_profit_margin_median)

aapl_npm_vol <- sqrt(var(aapl_npm$net_profit_margin))
industry_npm_vol <- sqrt(var(industry_npm$net_profit_margin_median))

# Mann Whitney tests

wilcox.test(aapl_npm[[2]], industry_npm[[2]])

# Asset Turnover Ratio Calculations and Statistical Testing
aapl_asset_turnover_ratio <- aapl_ratios_tbl |> 
  select(asset_turnover) |> 
  filter(!is.na(asset_turnover))

ind_asset_turnover_ratio <- industry_ratios_tbl |> 
  select(asset_turnover_median)

wilcox.test(aapl_asset_turnover_ratio[[1]], ind_asset_turnover_ratio[[1]])

# Current Ratio Calculations and Statistical Testing
aapl_current_ratio <- aapl_ratios_tbl |> 
  select(current_ratio) |> 
  filter(!is.na(current_ratio))

ind_current_ratio <- industry_ratios_tbl |> 
  select(current_ratio_median)

wilcox.test(aapl_current_ratio[[1]], ind_current_ratio[[1]])

# P/E Calculations and Statistical Testing
aapl_pe_ratio <- aapl_ratios_tbl |> 
  select(p_e_diluted_incl_ei) |> 
  filter(!is.na(p_e_diluted_incl_ei))

ind_pe_ratio <- industry_ratios_tbl |> 
  select(p_e_diluted_incl_ei_median)

wilcox.test(aapl_pe_ratio[[1]], ind_pe_ratio[[1]])

# P/B Calculations and Statistical Testing
aapl_pb_ratio <- aapl_ratios_tbl |> 
  select(price_book) |> 
  filter(!is.na(price_book))

ind_pb_ratio <- industry_ratios_tbl |> 
  select(price_book_median)

wilcox.test(aapl_pb_ratio[[1]], ind_pb_ratio[[1]])

# P/S Ratios Calculations and Statistical Testing
aapl_ps_ratio <- aapl_ratios_tbl |> 
  select(price_sales) |> 
  filter(!is.na(price_sales))

ind_ps_ratio <- industry_ratios_tbl |> 
  select(price_sales_median)

wilcox.test(aapl_ps_ratio[[1]], ind_ps_ratio[[1]])


