# 07_did.R
#
# Staggered difference-in-differences. Treated: cities completely dry before
# the study period that legalize sales, treated from their first successful
# election. Comparison: cities whose elections all failed and dry cities that
# held no election (not-yet-treated cities also serve as controls).
#   - Callaway and Sant'Anna (2021), doubly robust, universal base period,
#     covariates, weighted by 2003 population; the reported estimate is the
#     average of the event-study effects for years 0-5
#   - Two-way fixed effects for comparison
#
# Inputs:  data/derived/analysis_panel.rds, data/derived/panel_license_crash.rds,
#          data/derived/onpremise_sales.rds, data/derived/allcity_controls.rds,
#          data/raw/all_cityclassification_inTexas_2003-2019.dta,
#          data/raw/citypop_2003-2023.dta, data/raw/citypop_baseyear.dta
# Outputs: data/derived/did_panel.rds
#          output/tables/did_*.csv
#          output/figures/did/*.png

library(tidyverse)
library(haven)
library(fixest)
library(did)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/did", recursive = TRUE, showWarnings = FALSE)

covariates <- c("lncitypop", "Protestant_pct", "realGDPpc", "hispop_pct",
                "vehicles_permillion", "pol_ratio", "edu_hsonly", "aur")
dropped_cities <- c("DISH", "Jolly", "Coffee City", "Emhouse")

# ---- Panel ---------------------------------------------------------------------------
# Cities with elections: a city that failed and later passed enters as treated
with_elections <- read_rds("data/derived/analysis_panel.rds") %>%
  filter(!city %in% dropped_cities) %>%
  group_by(city) %>%
  filter(!(n_distinct(city_elecyear) == 2 & result == "failed")) %>%
  ungroup() %>%
  mutate(treat_year = if_else(result == "passed", elec_year, 0))

# Dry cities that held no election, 2003-2023
no_election_cities <- read_dta("data/raw/all_cityclassification_inTexas_2003-2019.dta") %>%
  filter(group_detail == "Dry and no elections in 2003-2019") %>%
  distinct(city)

no_election <- read_rds("data/derived/panel_license_crash.rds") %>%
  filter(between(year, 2003, 2023)) %>%
  left_join(read_rds("data/derived/onpremise_sales.rds"), by = c("city", "year")) %>%
  semi_join(no_election_cities, by = "city") %>%
  inner_join(read_dta("data/raw/citypop_2003-2023.dta") %>% zap_labels() %>%
               rename(citypop = population), by = c("city", "year")) %>%
  inner_join(read_dta("data/raw/citypop_baseyear.dta") %>% zap_labels() %>%
               select(city, pop2003), by = "city") %>%
  inner_join(read_rds("data/derived/allcity_controls.rds"), by = c("city", "year")) %>%
  mutate(treat_year = 0,
         tot_lics_per         = lic_all / citypop * 1000,
         tot_lics_on_per      = lic_on  / citypop * 1000,
         tot_lics_off_per     = lic_off / citypop * 1000,
         on_sale_all_per      = total_receipts_adj / citypop,
         crashes_ALL_per      = totalcrashes_all / citypop * 1000,
         crashes_DUI_per      = totalcrashes_dui / citypop * 1000,
         crashes_NonDUI_per   = totalcrashes_non_dui / citypop * 1000,
         Fatalcrashes_ALL_per = fatalcrashes_all / citypop * 1000,
         Fatalcrashes_DUI_per = fatalcrashes_dui / citypop * 1000)

outcomes <- c(tot_lics_per = "All Licenses", tot_lics_on_per = "On-Premise Licenses",
              tot_lics_off_per = "Off-Premise Licenses", on_sale_all_per = "On-Premise Sales",
              crashes_ALL_per = "All Crashes", crashes_DUI_per = "DUI Crashes",
              crashes_NonDUI_per = "Non-DUI Crashes", Fatalcrashes_ALL_per = "Fatal Crashes",
              Fatalcrashes_DUI_per = "DUI Fatal Crashes")

did_panel <- bind_rows(with_elections, no_election) %>%
  mutate(unit = str_c(city, "_", treat_year),
         unit_id = match(unit, sort(unique(unit), method = "radix")),
         post = as.numeric(treat_year > 0 & year >= treat_year)) %>%
  select(unit, unit_id, city, county, year, treat_year, result, event_time, post, citypop,
         pop2003, all_of(covariates), all_of(names(outcomes))) %>%
  arrange(unit_id, year)
write_rds(did_panel, "data/derived/did_panel.rds")

did_panel %>% distinct(unit, treat_year, result) %>%
  count(group = case_when(treat_year > 0 ~ "treated", result %in% "failed" ~ "failed",
                          TRUE ~ "no election")) %>% print()

# ---- Estimation ------------------------------------------------------------------------
twfe <- list()
csdid <- list()
events <- list()
for (y in names(outcomes)) {
  # Grapevine reports large on-premise sales with no on-premise license
  d <- did_panel %>%
    filter(!(y == "on_sale_all_per" & city == "Grapevine")) %>%
    drop_na(all_of(c(y, covariates, "pop2003")))

  m <- feols(as.formula(paste(y, "~ post +", paste(covariates, collapse = " + "), "| unit_id + year")),
             data = d, weights = ~ pop2003, cluster = ~ unit_id)
  twfe[[y]] <- tibble(outcome = y, b = coef(m)["post"], se = se(m)["post"],
                      p = pvalue(m)["post"], N = nobs(m))

  # On-premise sales are missing in many city-years (unbalanced panel); see README
  if (FALSE) next
  att <- att_gt(yname = y, tname = "year", idname = "unit_id", gname = "treat_year",
                xformla = as.formula(paste("~", paste(covariates, collapse = " + "))),
                data = d, weightsname = "pop2003", control_group = "notyettreated",
                base_period = "universal", est_method = "dr", bstrap = FALSE)
  es <- aggte(att, type = "dynamic", min_e = -5, max_e = 5, bstrap = FALSE, na.rm = TRUE)
  csdid[[y]] <- tibble(outcome = y, b = es$overall.att, se = es$overall.se,
                       p = 2 * pnorm(-abs(es$overall.att / es$overall.se)), N = nrow(d))
  events[[y]] <- tibble(outcome = y, time = es$egt, beta = es$att.egt, se = es$se.egt)
}
did_results <- bind_rows(bind_rows(twfe) %>% mutate(method = "TWFE"),
                         bind_rows(csdid) %>% mutate(method = "CSDID"))
pre_means <- did_panel %>%
  filter(event_time == -1, !is.na(result)) %>%
  summarise(across(all_of(names(outcomes)), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "pre_mean")
did_results <- left_join(did_results, pre_means, by = "outcome")
write_csv(did_results, "output/tables/did_results.csv")
write_csv(bind_rows(events), "output/tables/did_event_study.csv")
print(did_results, n = 30)

# ---- Event-study figures -----------------------------------------------------------------
fig_names <- c(tot_lics_per = "All_Licenses", tot_lics_on_per = "OnPremise_Licenses",
               tot_lics_off_per = "OffPremise_Licenses", on_sale_all_per = "OnPremise_Sales",
               crashes_ALL_per = "All_Crashes", crashes_DUI_per = "DUI_Crashes",
               crashes_NonDUI_per = "Non-DUI_Crashes", Fatalcrashes_ALL_per = "Fatal_All_Crashes",
               Fatalcrashes_DUI_per = "Fatal_DUI_Crashes")
for (y in names(events)) {
  g <- bind_rows(events) %>%
    filter(outcome == y) %>%
    mutate(se = replace_na(se, 0)) %>%
    ggplot(aes(time, beta)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 1) +
    geom_vline(xintercept = -1, colour = "red", linetype = "dashed", linewidth = 1) +
    geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                  colour = "blue", width = 0, linewidth = 1) +
    geom_point(colour = "blue", size = 2.5) +
    scale_x_continuous(breaks = -5:5) +
    labs(x = "Years from treatment", y = "Estimate and 95% CI") +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  ggsave(sprintf("output/figures/did/CSDID_%s.png", fig_names[[y]]), g,
         width = 7, height = 5, dpi = 230)
}
