# 10_descriptives.R
#
# Descriptive tables and figures:
#   - What local option elections legalize (Table 1)
#   - Summary statistics by estimation sample (Table 2)
#   - Estimation samples by design (Appendix)
#   - License-crash associations (Appendix)
#   - Elections over time (Figure 1)
#   - Private clubs before and after the election (text)
#
# Inputs:  data/derived/*.rds, data/raw/allcity_outcomes_covs.dta,
#          data/raw/all-licenses-1990-2019.csv, data/raw/countypop_03-23.dta,
#          data/restricted/store_movement_TX_2006_2020_byGroup.dta (optional)
# Outputs: output/tables/desc_*.csv, output/figures/election_trends.pdf

library(tidyverse)
library(haven)
library(fixest)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

elections <- read_rds("data/derived/elections.rds")
history   <- read_rds("data/derived/election_history.rds")
rd_panel  <- read_rds("data/derived/rd_panel.rds")
stacked   <- read_rds("data/derived/stacked_panel.rds")
did_panel <- read_rds("data/derived/did_panel.rds")

# ---- Table 1: what local option elections legalize -------------------------------------
# Propositions on the same ballot are combined; the ballot type is the most
# permissive proposition on it
ballots <- elections %>%
  filter(election_level == 2, !str_detect(str_to_lower(issueclean), "prohibit")) %>%
  mutate(iss = str_to_lower(issueclean),
         bwoff   = str_detect(iss, "beer and wine for off|beer for off"),
         alloff  = iss == "legal sale of all alcoholic beverages for off-premises consumption only",
         mbrest  = str_detect(iss, "mixed beverages in restaurants"),
         fullwet = str_detect(iss, "including mixed"),
         dry = status_before == "dry")
ballot_type <- function(data) {
  data %>%
    summarise(across(c(bwoff, alloff, mbrest, fullwet), any),
              passed = any(result == "passed"), .groups = "drop") %>%
    mutate(type = case_when(fullwet ~ 5, alloff ~ 3, bwoff & mbrest ~ 2,
                            bwoff ~ 1, mbrest ~ 4, TRUE ~ 6))
}
all_city <- ballots %>% group_by(city, year) %>% ballot_type() %>%
  semi_join(history %>% distinct(city, year = cohort), by = c("city", "year"))
prev_dry <- ballots %>% filter(dry) %>% group_by(city, year) %>% ballot_type()
table1 <- bind_rows(all_city %>% mutate(group = "all"), prev_dry %>% mutate(group = "dry")) %>%
  group_by(group, type) %>%
  summarise(elections = n(), passed = sum(passed), .groups = "drop") %>%
  bind_rows(bind_rows(all_city %>% mutate(group = "all"), prev_dry %>% mutate(group = "dry")) %>%
              group_by(group) %>%
              summarise(type = 0, elections = n(), passed = sum(passed), cities = n_distinct(city)))
write_csv(table1, "output/tables/desc_election_types.csv")
print(table1 %>% filter(type == 0))

# ---- Table 2: summary statistics --------------------------------------------------------
roads <- read_dta("data/raw/allcity_outcomes_covs.dta") %>%
  zap_labels() %>%
  select(city, year, road1 = CenterlineMiles_per1000, road2 = totalDVMT_per1000,
         road3 = vehicles_per1000)

sales_file <- "data/restricted/store_movement_TX_2006_2020_byGroup.dta"
county_sales <- if (file.exists(sales_file)) {
  read_dta(sales_file) %>% zap_labels() %>%
    rename(county = fips_county_descr) %>%
    mutate(county = recode(county, "Mclennan" = "McLennan", "De Witt" = "DeWitt",
                           "Dewitt" = "DeWitt", "San Augustin" = "San Augustine",
                           "Mcmullen" = "McMullen")) %>%
    inner_join(read_dta("data/raw/countypop_03-23.dta") %>% zap_labels(), by = c("year", "county")) %>%
    transmute(county, year, sales_pc = sales_dollar_adj / countypop)
} else tibble(county = character(), year = numeric(), sales_pc = numeric())

sum_vars <- c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per", "on_sale_all_per",
              "sales_pc", "crashes_ALL_per", "crashes_DUI_per", "Fatalcrashes_ALL_per",
              "citypop", "Protestant_pct", "hispop_pct", "realGDPpc", "road1", "road2",
              "road3", "pol_ratio", "edu_hsonly", "aur")
summary_stats <- bind_rows(
  rd_panel %>% filter(between(event_time, -5, 5)) %>% mutate(sample = "RD"),
  stacked %>% mutate(sample = "Stacked RD"),
  did_panel %>% mutate(sample = "DiD")
) %>%
  left_join(roads, by = c("city", "year")) %>%
  left_join(county_sales, by = c("county", "year")) %>%
  group_by(sample) %>%
  summarise(cities = n_distinct(city), obs = n(),
            across(all_of(sum_vars), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))
write_csv(summary_stats, "output/tables/desc_summary_statistics.csv")

# ---- Estimation samples -------------------------------------------------------------------
rd_post <- rd_panel %>% filter(event_time == 1) %>% arrange(city, elec_year) %>% distinct(city, .keep_all = TRUE)
units_did <- did_panel %>% distinct(unit, city, treat_year, result)
samples <- tribble(
  ~design, ~elections, ~cities, ~treated, ~comparison, ~obs,
  "RD", nrow(rd_post), nrow(rd_post), sum(rd_post$result == "passed"), sum(rd_post$result == "failed"), nrow(rd_post),
  "Stacked RD", n_distinct(stacked$city_id), n_distinct(stacked$city),
    n_distinct(stacked$city_id[stacked$group == "treated"]), n_distinct(stacked$city_id[stacked$group == "control"]),
    nrow(stacked),
  "DiD", sum(units_did$treat_year > 0 | units_did$result %in% "failed"), n_distinct(did_panel$city),
    sum(units_did$treat_year > 0), sum(units_did$treat_year == 0), nrow(did_panel)
) %>%
  mutate(failed = sum(units_did$result %in% "failed" & units_did$treat_year == 0),
         no_election = sum(is.na(units_did$result)))
write_csv(samples, "output/tables/desc_estimation_samples.csv")
print(samples)

# ---- License-crash associations ----------------------------------------------------------------
assoc_data <- read_rds("data/derived/analysis_panel.rds") %>%
  filter(!city %in% c("DISH", "Jolly", "Coffee City", "Emhouse"))
license_crash <- map_dfr(c("crashes_ALL_per", "crashes_DUI_per", "crashes_NonDUI_per"), function(y) {
  m <- feols(as.formula(paste(y, "~ tot_lics_on_per + tot_lics_off_per | city_elecyear + year")),
             data = assoc_data, cluster = ~ city)
  tibble(outcome = y, term = c("on", "off"), b = coef(m), se = se(m), p = pvalue(m),
         N = nobs(m), r2 = r2(m, "r2"))
})
write_csv(license_crash, "output/tables/desc_license_crash.csv")

# ---- Figure 1: elections over time -------------------------------------------------------------
fig1 <- history %>%
  filter(M_year == 1) %>%
  distinct(city, year, D_year) %>%
  count(year, result = if_else(D_year == 1, "passed", "failed")) %>%
  mutate(result = factor(result, levels = c("failed", "passed")))     # failed stacked on top
g <- ggplot(fig1, aes(year, n, fill = result)) +
  geom_col(width = 0.9, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c(passed = "grey45", failed = "grey90")) +
  scale_x_continuous(breaks = seq(2005, 2020, 5)) +
  labs(x = NULL, y = "Number of elections") +
  theme_bw(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("output/figures/election_trends.pdf", g, width = 7, height = 4.5)

# ---- Private clubs before and after the election (RD sample) -------------------------------------
licenses <- read_csv("data/raw/all-licenses-1990-2019.csv", col_types = cols(.default = "c")) %>%
  filter(category_detail != "Non-Retail") %>%
  mutate(across(c(start_year, stop_year), as.numeric),
         city_base = str_trim(str_remove(city, " \\(.*\\)$")),
         dupcity = city != city_base,          # duplicate names carry a county suffix
         lic_county = str_to_lower(county))
rd_elections <- rd_post %>%
  select(city, year = elec_year, result) %>%
  left_join(elections %>% filter(election_level == 2) %>% distinct(city, county1),
            by = "city", multiple = "first") %>%
  mutate(city_base = str_to_lower(city), elec_county = str_to_lower(county1))
clubs <- map_dfr(c(m1 = -1, p2 = 2), function(k) {
  rd_elections %>%
    left_join(licenses %>% select(city_base, dupcity, lic_county, category, category_detail,
                                  start_year, stop_year),
              by = "city_base", relationship = "many-to-many") %>%
    filter(!(dupcity %in% TRUE & lic_county != elec_county)) %>%
    mutate(on = start_year <= year + k & stop_year >= year + k) %>%
    group_by(city, year, result) %>%
    summarise(on_prem = sum(on & category %in% "On-Premise"),
              club    = sum(on & category_detail %in% "Private Club"),
              mixed   = sum(on & category_detail %in% "Mixed Beverage"), .groups = "drop") %>%
    mutate(when = if_else(k < 0, "t-1", "t+2"))
})
club_stats <- clubs %>%
  group_by(when) %>%
  summarise(share_any_club = mean(club > 0), club_share_onprem = sum(club) / sum(on_prem),
            passed_club = mean(club[result == "passed"]), passed_mixed = mean(mixed[result == "passed"]),
            passed_onprem = mean(on_prem[result == "passed"]), elections = n())
write_csv(club_stats, "output/tables/desc_private_clubs.csv")
print(club_stats)
