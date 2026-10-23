# Does Easier Alcohol Access Affect Road Safety? Insights from Texas Municipal Elections

**Somdeepa Das and Tong Li**

This repository contains the data and R code for the paper. The paper uses local option elections in Texas cities (2003–2019) to estimate how legalizing alcohol sales affects alcohol outlets, alcohol sales, and traffic crashes. Legalization increases off-premise outlets and retail alcohol sales but has no detectable effect on total, DUI, or fatal crashes.

## Running the code

1. Open `alcohol-access-road-safety.Rproj` in RStudio (or start R in the repository root).
2. Install the package versions recorded in `renv.lock`: `renv::restore()`.
3. Run everything: `source("R/00_run_all.R")` (under a minute).

Tables are written to `output/tables/tex/` (one LaTeX file per table in the paper) and figures to `output/figures/`. Intermediate results are saved as CSV files in `output/tables/`.

| Script                        | Purpose                                                                                                                  |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `01_build_county_gdp.R`       | County real GDP per capita from the BEA                                                                                  |
| `02_build_license_panel.R`    | City-year counts of on-premise (including private clubs) and off-premise liquor licenses from license-level TABC records |
| `03_build_analysis_panel.R`   | Election records, on-premise sales, and the city-year panel around each city's first election                            |
| `04_build_election_history.R` | City-year election history, stacked by election, for the stacked RD                                                      |
| `05_rd.R`                     | RD estimates, covariate balance, density test, RD on changes, bandwidth sensitivity, RD plots                            |
| `06_stacked_rd.R`             | Stacked dynamic RD: average effects, event studies, heterogeneity by beverage and election type                          |
| `07_did.R`                    | Callaway–Sant'Anna and two-way fixed effects difference-in-differences                                                   |
| `08_offpremise_sales.R`       | County-level off-premise sales: RD, stacked RD, difference-in-differences (needs NielsenIQ data)                         |
| `09_spillovers.R`             | Spillovers from wet neighbors within 20 miles and in distance rings                                                      |
| `10_descriptives.R`           | Election types, summary statistics, estimation samples, license–crash associations, Figure 1, private clubs              |
| `11_write_tables.R`           | Writes every table in the paper as LaTeX                                                                                 |

## Software

R 4.6 with the package versions in `renv.lock`. Main packages: `tidyverse`, `haven`, `rdrobust`, `rddensity`, `fixest`, `did`, `sf`.

`rdrobust` is pinned at version 3.0.0, which reproduces the estimates in the paper (and Stata `rdrobust` 10.0.0). Version 4.0.0 changes the small-sample correction for clustered standard errors, which moves them by about 1%.

## Data

### Public data (included, `data/raw/`)

| File                                                                                                           | Content                                                                                    | Source                                                               |
| -------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | -------------------------------------------------------------------- |
| `elections-data-97-2020-cleaned_all.dta`, `Elections_all_simplified_03_19.dta`                                 | Local option election results                                                              | Texas Alcoholic Beverage Commission (TABC)                           |
| `tabc_licenses_2024-07-30.csv`                                                                                 | License-level records (licensee names and addresses removed)                               | TABC, pre-2012 records and License Information extract of 2024-07-30 |
| `all-licenses-1990-2019.csv`                                                                                   | License spells by city (private-club statistics)                                           | TABC                                                                 |
| `panel_license_crash_1990-2023.csv`                                                                            | City-year crash counts                                                                     | Texas Department of Transportation, Crash Records Information System |
| `onpremise_sales_03-24.dta`                                                                                    | Mixed beverage gross receipts by city                                                      | TABC                                                                 |
| `cpi_u_annual.csv`                                                                                             | CPI-U, annual average                                                                      | FRED series CPIAUCNS                                                 |
| `CAGDP9_TX_2001_2024.csv`, `CAINC1_TX_1969_2024.csv`                                                           | County real GDP and population                                                             | Bureau of Economic Analysis                                          |
| `citypop_2003-2023.dta`, `citypop_baseyear.dta`, `countypop_03-23.dta`, `countypop_2006.dta`                   | City and county population                                                                 | U.S. Census Bureau                                                   |
| `allcity_controls.dta`, `allcity_outcomes_covs.dta`                                                            | City and county controls (religion, demographics, unemployment, politics, roads, vehicles) | Public sources described in the paper                                |
| `all_cityclassification_inTexas_2003-2019.dta`, `allcity_names_withcounty.dta`, `city_name_crosswalk_long.csv` | City wet/dry classification, counties, and name crosswalk                                  | Authors, from TABC and Census records                                |
| `geography/Cities.*`                                                                                           | City boundaries                                                                            | Texas Department of Transportation                                   |

### Restricted data (not included)

Off-premise sales come from the NielsenIQ Retail Scanner Data, available through the Kilts Center for Marketing at the University of Chicago Booth School of Business under a license that does not allow redistribution. Researchers with access can place the county-year aggregate `store_movement_TX_2006_2020_byGroup.dta` in `data/restricted/`; without it, the off-premise sales results are skipped and the rest of the analysis runs.

NielsenIQ disclosure: Researchers' own analyses calculated (or derived) based in part on data from Nielsen Consumer LLC and marketing databases provided through the NielsenIQ Datasets at the Kilts Center for Marketing Data Center at The University of Chicago Booth School of Business. The conclusions drawn from the NielsenIQ data are those of the researchers and do not reflect the views of NielsenIQ. NielsenIQ is not responsible for, had no role in, and was not involved in analyzing and preparing the results reported herein.

## License

Code is released under the MIT License (see `LICENSE`). Data remain subject to the terms of their original providers.
