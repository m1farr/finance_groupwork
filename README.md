# finance_groupwork: Overview
This repo contains the work that I contributed towards the "AAPL Shareholder Value Creation" group project for the module "Topics in Financial Management I."
<br>
<br>
My work mainly concerned with two portions of the project:

1. The calculation and graphical illustration of the **Net Profit Margin** of Apple and the industry.
2. The calculation and graphical illustration of the **CAPM** and **Fama-French 3-Factor** models for Apple and the industry.

## Data
All data was provided in the coursework files and is sourced from Wharton Research Data Services (WRDS). 

The coursework files provided the following data on Apple and the industry from 2010-2023:
- Company profile *(for Apple only)*
- Financial statements *(for Apple only)*
- Financial ratios
- Financial statements
- Fama-French 3-Factors and the risk-free rate

## Code

`00_dependencies.R` loads all of the additional packages that I utilized in this project. 
<br>

`01_functions.R` defines various functions that I wrote to streamline the coding process in subsequent pages.
<br>

`02_data.R` loads and stores the dataset sheets from the given Excel workbook.
<br>

`03_profitability.R` contains all the work related to calculating and graphing **Net Profit Margin** for both Apple and the industry.
Additionally, the file contains statistical testing (specifically, the **Mann-Whitney U Test**) for all chosen metrics. 
<br>

`04_capm.R` contains all the work related to calculating and graphing the **CAPM** betas for Apple and the industry.
<br>

`05_factor_loadings.R` contains all the work related to calculating and graphing the **Fama-French 3-Factor Model** for Apple and the industry.
