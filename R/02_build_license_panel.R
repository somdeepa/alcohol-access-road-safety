# 02_build_license_panel.R
#
# City-year liquor-license counts, 1990-2023, merged into the city-year crash
# panel.
#   - A business is a combined license number, active from the year of its
#     first issue to the year of its last expiration.
#   - lic_on   = on-premise licenses, INCLUDING private clubs
#     lic_off  = off-premise licenses
#     lic_club = private clubs (subset of lic_on; descriptive)
#     lic_all  = lic_on + lic_off
#   - City-years with no license of a type are recorded as 0.
#
# Inputs:  data/raw/tabc_licenses_2024-07-30.csv  license-level TABC records
#            (pre-2012 file + TABC License Information extract of 2024-07-30;
#             licensee names and addresses removed)
#          data/raw/panel_license_crash_1990-2023.csv  city-year crash counts
# Output:  data/derived/panel_license_crash.rds

library(tidyverse)

# City-name key used on both sides: the license data drop "City"/"Village"
# and spell some names differently
city_key <- function(city, county = NA_character_) {
  raw <- str_to_lower(str_trim(city))
  key <- raw %>%
    str_replace("^st\\. ", "saint ") %>%
    str_remove_all("'") %>%
    str_remove(" (city|village)$")
  case_when(
    key == "gun barrell"                          ~ "gun barrel",
    key %in% c("reno (parker)", "reno (tarrant)") ~ "reno (parker&tarrant)",
    # Clarksville City (Gregg) and Clarksville (Red River) share a name
    raw == "clarksville city"                     ~ "clarksville city",
    key == "clarksville" & county %in% "Gregg"    ~ "clarksville city",
    TRUE                                          ~ key
  )
}

# ---- License counts by city and year -----------------------------------------
licenses <- read_csv("data/raw/tabc_licenses_2024-07-30.csv",
                     col_types = cols(.default = "c")) %>%
  filter(category %in% c("On-Premise", "Off-Premise")) %>%   # drop Non-Retail
  mutate(sy = as.numeric(str_sub(original_issue_date, 1, 4)),
         ey = as.numeric(str_sub(expiration_date, 1, 4))) %>%
  filter(!is.na(sy), !is.na(ey))

# One spell per business
spells <- licenses %>%
  group_by(combined_number) %>%
  summarise(across(c(city, county, category, category_detail), ~ first(na.omit(.x))),
            sy = min(sy), ey = max(ey), .groups = "drop") %>%
  mutate(start = pmax(sy, 1990), stop = pmin(ey, 2023)) %>%
  filter(start <= stop) %>%
  mutate(key = city_key(city, county))

counts <- spells %>%
  mutate(year = map2(start, stop, seq)) %>%
  unnest(year) %>%
  group_by(key, year) %>%
  summarise(lic_on   = sum(category == "On-Premise"),
            lic_off  = sum(category == "Off-Premise"),
            lic_club = sum(category_detail %in% "Private Club"),
            .groups = "drop")

# ---- Merge into the crash panel ------------------------------------------------
panel <- read_csv("data/raw/panel_license_crash_1990-2023.csv",
                  show_col_types = FALSE) %>%
  rename_with(str_to_lower) %>%
  mutate(key = city_key(city))
stopifnot(!anyDuplicated(select(panel, key, year)))

panel <- panel %>%
  rename(old_on = total_businesses_on, old_off = total_businesses_off) %>%
  left_join(counts, by = c("key", "year")) %>%
  mutate(across(c(lic_on, lic_off, lic_club), ~ replace_na(.x, 0)))

# Validation: off-premise counts match the original panel in most city-years
# 2003-2019 (the original records city-years without licenses as 1)
check <- panel %>% filter(between(year, 2003, 2019), !is.na(old_off))
cat("Share of 2003-2019 city-years with off-premise counts equal to the original:",
    round(mean(check$lic_off == check$old_off), 3), "\n")
stopifnot(mean(check$lic_off == check$old_off) > 0.9)

panel <- panel %>%
  mutate(lic_all = lic_on + lic_off) %>%
  select(-old_on, -old_off, -total_businesses, -key)

write_rds(panel, "data/derived/panel_license_crash.rds")
