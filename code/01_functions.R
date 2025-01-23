# read excel sheet and rename cols
load_data <- function(sheet_name){
  data_sheet <- read_xlsx("data/cw_file.xlsx", sheet = sheet_name) |> 
    clean_names()
}

# select net profit margin and date columns for given table
get_npm <- function(tbl, metric){
  tbl |> 
    select(date, {{metric}}) |> 
    mutate(date = as.Date(date)) |> 
    as_tibble()
}

# Calculate beta - CAPM
calculate_capm_beta <- function(window_data){
  model <- lm(ri_minus_rf ~ rm_minus_rf, data = window_data)
  return(coef(model))
}

# Calculate betas - FF3FM
calculate_betas <- function(window_data) {
  # Perform regression
  model <- lm(aapl_excess_ret ~ sp_excess_ret + smb_ret + hml_ret, data = window_data)
  # Extract coefficients
  return(coef(model))
}

calculate_ind_betas <- function(window_data) {
  model <- lm(ind_excess_ret ~ sp_excess_ret + smb_ret + hml_ret, data = window_data)
  return(coef(model))
}



