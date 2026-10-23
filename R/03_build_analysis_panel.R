# 03_build_analysis_panel.R
#
# City-year panel around each city's first passed and first failed local
# option election, 2003-2023, with licenses, on-premise sales, crashes,
# population, and controls. Used by the RD and difference-in-differences
# analyses.
#
# Inputs:  data/raw/elections-data-97-2020-cleaned_all.dta  election records
#          data/raw/onpremise_sales_03-24.dta  on-premise (mixed beverage) receipts
#          data/raw/allcity_names_withcounty.dta  census city names
#          data/raw/citypop_2003-2023.dta  city population
#          data/raw/cpi_u_annual.csv  CPI-U annual averages (FRED CPIAUCNS)
#          data/derived/panel_license_crash.rds  (02_build_license_panel.R)
#          data/derived/allcity_controls.rds     (01_build_county_gdp.R)
# Outputs: data/derived/elections.rds  city-level election records, 2003-2019
#          data/derived/onpremise_sales.rds  city-year on-premise sales (0 if no license)
#          data/derived/analysis_panel.rds

library(tidyverse)
library(haven)

# ---- Elections ----------------------------------------------------------------
prohibit_issues <- c(
  "prohibit the legal sale of all alcoholic beverages including mixed beverages",
  "prohibit the legal sale of all alcoholic beverages for off-premises consumption only"
)

elections <- read_dta("data/raw/elections-data-97-2020-cleaned_all.dta") %>%
  zap_labels() %>%
  filter(between(year, 2003, 2019)) %>%
  mutate(vote_margin = for_vote / (for_vote + against_vote) - 0.5,
         # current city names
         city = recode(city,
                       "Clark" = "DISH", "New Fairfield" = "Fairfield",
                       "Desoto" = "DeSoto", "Saint Jo" = "St. Jo",
                       "Deer park" = "Deer Park", "DeKalb" = "De Kalb",
                       "Village of Buffalo Springs" = "Buffalo Springs",
                       "Nederwald" = "Niederwald"))
write_rds(elections, "data/derived/elections.rds")

# Elections in previously dry cities that legalize sales (city level only).
# Propositions on the same ballot are combined: a city-year counts as passed
# if any proposition passed, with the average margin of the passing
# propositions; otherwise as failed, with the average margin of all of them.
city_elections <- elections %>%
  filter(status_before != "wet", !issueclean %in% prohibit_issues,
         election_level == 2) %>%
  group_by(city, year) %>%
  summarise(passed = any(result == "passed"),
            vote_margin = if (passed) mean(vote_margin[result == "passed"])
                          else mean(vote_margin),
            .groups = "drop") %>%
  mutate(result = if_else(passed, "passed", "failed")) %>%
  select(-passed)

# Each city's first passed and first failed election
first_elections <- city_elections %>%
  group_by(city, result) %>%
  slice_min(year, n = 1) %>%
  ungroup() %>%
  transmute(city, result, vote_margin, vote_share = vote_margin + 0.5,
            elec_year = year, city_elecyear = str_c(city, "_", year))
count(first_elections, result)

# One row per election and calendar year, 2003-2023
election_years <- first_elections %>%
  cross_join(tibble(year = 2003:2023))

# ---- On-premise sales -----------------------------------------------------------
# Receipts are deflated to 2023 dollars with the annual CPI-U
cpi <- read_csv("data/raw/cpi_u_annual.csv", show_col_types = FALSE)
cpi_2023 <- cpi$cpi[cpi$year == 2023]

census_names <- read_dta("data/raw/allcity_names_withcounty.dta") %>% distinct(city)

onpremise <- read_dta("data/raw/onpremise_sales_03-24.dta") %>%
  zap_labels() %>%
  filter(between(year, 2003, 2023)) %>%
  # Stata-style proper case: capitalize letters that follow a non-letter
  mutate(city = str_trim(str_to_lower(location_city)),
         city = gsub("(^|[^a-z])([a-z])", "\\1\\U\\2", city, perl = TRUE),
         city = recode(city, "Clark" = "DISH", "New Fairfield" = "Fairfield",
                       "Desoto" = "DeSoto", "De Soto" = "DeSoto"),
         beer_wine_receipts = wine_receipts + beer_receipts) %>%
  left_join(cpi, by = "year") %>%
  mutate(inflator = cpi_2023 / cpi) %>%
  group_by(city, year) %>%
  summarise(across(c(liquor_receipts, beer_wine_receipts, total_receipts),
                   ~ sum(.x * inflator, na.rm = TRUE), .names = "{.col}_adj"),
            .groups = "drop") %>%
  semi_join(census_names, by = "city")               # incorporated cities only

# City-year panel, 2003-2023. Receipts are reported only by permit holders, so
# a city-year with no on-premise license and no reported receipts had no
# on-premise sales.
onpremise <- read_rds("data/derived/panel_license_crash.rds") %>%
  filter(between(year, 2003, 2023)) %>%
  select(city, year, lic_on) %>%
  left_join(onpremise, by = c("city", "year")) %>%
  mutate(across(ends_with("_adj"), ~ if_else(is.na(.x) & lic_on == 0, 0, .x))) %>%
  select(-lic_on)
write_rds(onpremise, "data/derived/onpremise_sales.rds")

# ---- Outcomes, population, and controls ------------------------------------------
citypop <- read_dta("data/raw/citypop_2003-2023.dta") %>%
  zap_labels() %>%
  rename(citypop = population)

pop2003 <- citypop %>% filter(year == 2003) %>% select(city, pop2003 = citypop)

controls <- read_rds("data/derived/allcity_controls.rds")

panel <- read_rds("data/derived/panel_license_crash.rds") %>%
  filter(year >= 2003) %>%
  left_join(onpremise, by = c("city", "year")) %>%
  inner_join(election_years, by = c("city", "year")) %>%
  inner_join(citypop, by = c("city", "year")) %>%
  left_join(pop2003, by = "city") %>%
  mutate(
    event_time = year - elec_year,
    # Licenses per 1,000 population
    tot_lics_per     = lic_all / citypop * 1000,
    tot_lics_on_per  = lic_on  / citypop * 1000,
    tot_lics_off_per = lic_off / citypop * 1000,
    # On-premise sales per capita (2023 dollars)
    on_sale_all_per = total_receipts_adj     / citypop,
    on_sale_bw_per  = beer_wine_receipts_adj / citypop,
    on_sale_lq_per  = liquor_receipts_adj    / citypop,
    # Crashes per 1,000 population
    crashes_ALL_per             = totalcrashes_all         / citypop * 1000,
    Fatalcrashes_ALL_per        = fatalcrashes_all         / citypop * 1000,
    AnyInjuryCrashes_ALL_per    = anyinjurycrashes_all     / citypop * 1000,
    NonInjuryCrashes_ALL_per    = noninjurycrashes_all     / citypop * 1000,
    crashes_DUI_per             = totalcrashes_dui         / citypop * 1000,
    Fatalcrashes_DUI_per        = fatalcrashes_dui         / citypop * 1000,
    AnyInjuryCrashes_DUI_per    = anyinjurycrashes_dui     / citypop * 1000,
    NonInjuryCrashes_DUI_per    = noninjurycrashes_dui     / citypop * 1000,
    crashes_NonDUI_per          = totalcrashes_non_dui     / citypop * 1000,
    Fatalcrashes_NonDUI_per     = fatalcrashes_non_dui     / citypop * 1000,
    AnyInjuryCrashes_NonDUI_per = anyinjurycrashes_non_dui / citypop * 1000,
    NonInjuryCrashes_NonDUI_per = noninjurycrashes_non_dui / citypop * 1000
  ) %>%
  inner_join(controls, by = c("city", "year")) %>%
  select(year, elec_year, event_time, city_elecyear, city, result, vote_margin,
         vote_share, county, citypop, pop2003, lic_club, ends_with("_per"),
         lncitypop:totalDVMT_permillion, realGDPpc) %>%
  arrange(city_elecyear, year)

cat("Analysis panel:", nrow(panel), "city-election-years,",
    n_distinct(panel$city_elecyear), "elections,", n_distinct(panel$city), "cities\n")
write_rds(panel, "data/derived/analysis_panel.rds")
