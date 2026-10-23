# 01_build_county_gdp.R
#
# County real GDP per capita from the BEA, attached to the city controls.
#   realGDPpc = county real GDP (thousands of chained 2017 $) / county population
#
# Inputs:  data/raw/CAGDP9_TX_2001_2024.csv  BEA CAGDP9, real GDP by county
#                                            (line 1 = all industry total)
#          data/raw/CAINC1_TX_1969_2024.csv  BEA CAINC1 (line 2 = population)
#          data/raw/allcity_outcomes_covs.dta  city -> primary county
#          data/raw/allcity_controls.dta       city-year controls
# Output:  data/derived/allcity_controls.rds

library(tidyverse)
library(haven)

# BEA real GDP by county (thousands of chained 2017 dollars)
gdp <- read_csv("data/raw/CAGDP9_TX_2001_2024.csv",
                col_types = cols(.default = "c")) %>%
  filter(LineCode == "1") %>%
  mutate(fips = as.numeric(str_remove_all(GeoFIPS, "[^0-9]"))) %>%
  filter(!is.na(fips), fips != 48000) %>%            # drop the state row
  mutate(county = str_trim(str_remove(GeoName, ", TX"))) %>%
  pivot_longer(matches("^\\d{4}$"), names_to = "year", values_to = "gdp") %>%
  transmute(fips, county, year = as.integer(year),
            gdp = suppressWarnings(as.numeric(gdp)))  # "(D)", "(NA)" -> NA

# BEA county population
pop <- read_csv("data/raw/CAINC1_TX_1969_2024.csv",
                col_types = cols(.default = "c")) %>%
  filter(LineCode == "2") %>%
  mutate(fips = as.numeric(str_remove_all(GeoFIPS, "[^0-9]"))) %>%
  filter(!is.na(fips), fips != 48000) %>%
  pivot_longer(matches("^\\d{4}$"), names_to = "year", values_to = "pop") %>%
  transmute(fips, year = as.integer(year),
            pop = suppressWarnings(as.numeric(pop))) %>%
  filter(between(year, 2001, 2024))

gdppc <- inner_join(pop, gdp, by = c("fips", "year"))
stopifnot(nrow(gdppc) == nrow(gdp), nrow(gdppc) == nrow(pop))

gdppc <- gdppc %>%
  mutate(ckey = str_to_lower(str_remove_all(county, "[ .]")),
         realGDPpc = gdp / pop) %>%
  select(ckey, year, realGDPpc)

# Each city's primary county
xwalk <- read_dta("data/raw/allcity_outcomes_covs.dta") %>%
  distinct(city, county) %>%
  mutate(ckey = str_to_lower(str_remove_all(county, "[ .]"))) %>%
  select(city, ckey)
stopifnot(!anyDuplicated(xwalk$city))

controls <- read_dta("data/raw/allcity_controls.dta") %>% zap_labels()
n_controls <- nrow(controls)
controls <- inner_join(controls, xwalk, by = "city")
stopifnot(nrow(controls) == n_controls)              # every city has a county

# County-level controls must be constant within the assigned county
controls %>%
  group_by(ckey, year) %>%
  summarise(sd = sd(Protestant_pct), .groups = "drop") %>%
  filter(sd > 1e-6) %>%
  { stopifnot(nrow(.) == 0) }

# Replace the original (mis-scaled) GDP measure with the BEA county measure
controls <- controls %>%
  select(-realGDPpc) %>%
  inner_join(gdppc, by = c("ckey", "year")) %>%
  select(-ckey)
stopifnot(nrow(controls) == n_controls)              # every county-year has GDP

summary(controls$realGDPpc)
write_rds(controls, "data/derived/allcity_controls.rds")
