# 09_spillovers.R
#
# Spatial spillovers: does a city's exposure to wet neighbors affect its
# licenses, sales, and crashes?
#   y_ct = b1 Wet_ct + b2 WetShare_ct + b3 Wet_ct x WetShare_ct + city FE + year FE
# WetShare = share of cities within 20 miles (centroid distance) that are wet
# in year t; the appendix uses 0-10, 10-20, and 20-50 mile rings. Spillovers to
# dry cities = b2; to wet cities = b2 + b3. Sample: cities already wet before
# 2003 and cities that legalize during 2003-2019 (always-dry cities count as
# neighbors but are not in the regression sample).
#
# Inputs:  data/raw/geography/Cities.shp  TxDOT city boundaries
#          data/raw/city_name_crosswalk_long.csv  city names across sources
#          data/raw/all_cityclassification_inTexas_2003-2019.dta
#          data/raw/citypop_2003-2023.dta, data/derived/elections.rds,
#          data/derived/panel_license_crash.rds, data/derived/onpremise_sales.rds
#          data/restricted/store_movement_TX_2006_2020_byGroup.dta (optional)
# Outputs: output/tables/spillover_results.csv

library(tidyverse)
library(haven)
library(sf)
library(fixest)

# ---- Names: map every source to the crosswalk's reference city name ----------------------
crosswalk <- read_csv("data/raw/city_name_crosswalk_long.csv", show_col_types = FALSE) %>%
  mutate(across(c(city_orig, city_ref), ~ str_to_lower(str_squish(.x))))
maps_lookup <- crosswalk %>% filter(dataset == "maps") %>% distinct(city_orig, city_ref)
census_lookup <- crosswalk %>% filter(dataset == "population") %>% distinct(city_orig, city_ref)
to_ref <- function(city) census_lookup$city_ref[match(str_to_lower(str_squish(city)), census_lookup$city_orig)]

# ---- Distances between city centroids (miles) ----------------------------------------------
cities <- read_sf("data/raw/geography/Cities.shp") %>%
  mutate(city_orig = str_to_lower(str_squish(CITY_NM))) %>%
  inner_join(maps_lookup, by = "city_orig") %>%
  distinct(city_ref, .keep_all = TRUE) %>%
  st_transform(32614)
dist <- st_distance(st_centroid(cities)) %>% units::set_units("miles") %>% units::drop_units()
dimnames(dist) <- list(cities$city_ref, cities$city_ref)

# ---- Wet/dry status by city and year, 2003-2019 ---------------------------------------------
classification <- read_dta("data/raw/all_cityclassification_inTexas_2003-2019.dta") %>%
  zap_labels() %>%
  mutate(city_ref = to_ref(city)) %>%
  filter(!is.na(city_ref)) %>%
  distinct(city_ref, .keep_all = TRUE) %>%
  select(city_ref, group_detail)

first_pass <- read_rds("data/derived/elections.rds") %>%
  filter(result == "passed", status_before == "dry") %>%
  mutate(city_ref = to_ref(city)) %>%
  group_by(city_ref) %>%
  summarise(first_pass = min(year))

always_dry <- c("Dry and no elections in 2003-2019", "Failed all elections during 2003-2019")
status <- classification %>%
  cross_join(tibble(year = 2003:2019)) %>%
  left_join(first_pass, by = "city_ref") %>%
  mutate(wet = case_when(
    group_detail == "Already-wet before 2003" ~ 1,
    group_detail %in% always_dry ~ 0,
    !is.na(first_pass) ~ as.numeric(year >= first_pass)
  ))

# Share of wet neighbors within each distance ring (neighbors of unknown
# status count in the denominator)
rings <- tribble(~ring,   ~lo, ~hi,
                 "0_20",    0,  20,
                 "0_10",    0,  10,
                 "10_20",  10,  20,
                 "20_50",  20,  50)
exposure <- map_dfr(2003:2019, function(yr) {
  wet_now <- status %>% filter(year == yr) %>% select(city_ref, wet)
  is_wet <- coalesce(wet_now$wet[match(rownames(dist), wet_now$city_ref)], 0)
  map_dfr(seq_len(nrow(rings)), function(i) {
    inside <- (dist > rings$lo[i] & dist <= rings$hi[i]) * 1     # excludes the city itself
    n_nb <- rowSums(inside)
    tibble(city_ref = rownames(dist), year = yr, ring = rings$ring[i],
           share = if_else(n_nb > 0, drop(inside %*% is_wet) / n_nb, 0))
  })
}) %>%
  pivot_wider(names_from = ring, values_from = share, names_prefix = "exposure_")

# ---- Outcomes ---------------------------------------------------------------------------------
outcomes_city <- read_rds("data/derived/panel_license_crash.rds") %>%
  filter(between(year, 2003, 2019)) %>%
  left_join(read_rds("data/derived/onpremise_sales.rds"), by = c("city", "year")) %>%
  inner_join(read_dta("data/raw/citypop_2003-2023.dta") %>% zap_labels() %>%
               rename(population = population), by = c("city", "year")) %>%
  mutate(city_ref = to_ref(city)) %>%
  filter(!is.na(city_ref)) %>%
  group_by(city_ref, year) %>%
  summarise(population = max(population),
            across(c(lic_all, lic_off, lic_on, totalcrashes_all, totalcrashes_dui,
                     fatalcrashes_all), ~ sum(.x, na.rm = TRUE)),
            # on-premise sales are already 0 where a city has no license;
            # they stay missing where a licensed city reports no receipts
            total_receipts_adj = if (all(is.na(total_receipts_adj))) NA else sum(total_receipts_adj, na.rm = TRUE),
            .groups = "drop")

city_panel <- status %>%
  filter(!group_detail %in% always_dry) %>%
  left_join(exposure, by = c("city_ref", "year")) %>%
  inner_join(outcomes_city, by = c("city_ref", "year")) %>%
  mutate(all_licenses_rate     = lic_all / population * 1000,
         off_licenses_rate     = lic_off / population * 1000,
         on_licenses_rate      = lic_on  / population * 1000,
         onprem_sales_total_pc = total_receipts_adj / population,
         total_crashes_rate    = totalcrashes_all / population * 1000,
         dui_crashes_rate      = totalcrashes_dui / population * 1000,
         fatal_crashes_rate    = fatalcrashes_all / population * 1000)

city_outcomes <- c("all_licenses_rate", "off_licenses_rate", "on_licenses_rate",
                   "onprem_sales_total_pc", "total_crashes_rate", "dui_crashes_rate",
                   "fatal_crashes_rate")

# ---- Estimation ---------------------------------------------------------------------------------
# Dry-city spillover (b2) and wet-city spillover (b2 + b3), with delta-method SE
spillover_fit <- function(data, y, ring, fe, cl) {
  x <- paste0("exposure_", ring)
  m <- feols(as.formula(paste(y, "~ wet *", x, "|", fe)), data = data, cluster = cl)
  inter <- paste0("wet:", x)
  V <- vcov(m)
  wet_b <- coef(m)[x] + coef(m)[inter]
  wet_se <- sqrt(V[x, x] + V[inter, inter] + 2 * V[x, inter])
  df <- degrees_freedom(m, "t")
  tibble(outcome = y, ring = ring,
         effect = c("dry", "wet"),
         b  = c(coef(m)[x], wet_b),
         se = c(se(m)[x], wet_se),
         p  = c(pvalue(m)[x], 2 * pt(-abs(wet_b / wet_se), df)),
         mean_dv = mean(data[[y]], na.rm = TRUE), N = nobs(m))
}

results <- expand_grid(outcome = city_outcomes, ring = rings$ring) %>%
  pmap_dfr(~ spillover_fit(city_panel, .x, .y, "city_ref + year", ~ city_ref))

# Off-premise sales (NielsenIQ, county level): status and exposure are
# population-weighted county averages of the city measures; a county is wet
# if more than half of its city population lives in wet cities
sales_file <- "data/restricted/store_movement_TX_2006_2020_byGroup.dta"
if (file.exists(sales_file)) {
  city_county <- read_dta("data/raw/allcity_names_withcounty.dta") %>%
    transmute(city_ref = to_ref(city), county) %>% filter(!is.na(city_ref)) %>%
    distinct(city_ref, .keep_all = TRUE)
  county_panel <- status %>%
    left_join(exposure, by = c("city_ref", "year")) %>%
    inner_join(outcomes_city %>% select(city_ref, year, population), by = c("city_ref", "year")) %>%
    inner_join(city_county, by = "city_ref") %>%
    group_by(county, year) %>%
    summarise(wet = as.numeric(weighted.mean(wet, population, na.rm = TRUE) > 0.5),
              across(starts_with("exposure_"), ~ weighted.mean(.x, population, na.rm = TRUE)),
              .groups = "drop") %>%
    inner_join(read_dta(sales_file) %>% zap_labels() %>%
                 rename(county = fips_county_descr) %>%
                 mutate(county = recode(county, "Mclennan" = "McLennan", "De Witt" = "DeWitt",
                                        "Dewitt" = "DeWitt", "San Augustin" = "San Augustine",
                                        "Mcmullen" = "McMullen")) %>%
                 inner_join(read_dta("data/raw/countypop_03-23.dta") %>% zap_labels(),
                            by = c("year", "county")) %>%
                 transmute(county, year, offprem_sales_total_pc = sales_dollar_adj / countypop),
               by = c("county", "year")) %>%
    filter(between(year, 2006, 2019))
  results <- bind_rows(results,
                       map_dfr(rings$ring, ~ spillover_fit(county_panel, "offprem_sales_total_pc", .x,
                                                           "county + year", ~ county)))
}

write_csv(results, "output/tables/spillover_results.csv")
results %>% filter(ring == "0_20") %>%
  mutate(across(c(b, se, mean_dv), ~ round(.x, 3)), p = round(p, 3)) %>%
  print(n = 30)
