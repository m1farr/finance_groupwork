source("code/00_dependencies.R")
source("code/01_functions.R")

# load worksheets from excel separately

industry_ratios_tbl <- load_data("INDUSTRY LEVEL FINANCIAL RATIOS") |> 
  filter(gic_description_reference == "Information Technology")
aapl_statements_tbl <- load_data("Apple Inc Financial Statements ")
aapl_ratios_tbl <- load_data("AAPL Financial Ratios") |> 
  filter(!is.na(date))
aapl_returns_tbl <- load_data("AAPL monthly returns")
ff_mome_tbl <- load_data("Fama-French 3 Factors Plus Mome")

# save filtered industry ratios table for group members who are struggling to 
# do so in Excel
write_xlsx(industry_ratios_tbl, path = "output/info_tech.xlsx")