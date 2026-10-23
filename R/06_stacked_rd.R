# 06_stacked_rd.R
#
# Stacked dynamic RD (Cellini, Ferreira and Rothstein 2010; Biasi, Lafortune
# and Schonholzer 2025). Each city-level election is a unit observed from five
# years before to five years after the vote. Passed elections are treated;
# failed elections not followed by a passed election within five years are
# clean controls. Regressions control for the city's full election history
# (whether an election was held, passed, and its vote share k years earlier
# or later), with city-by-election and election-by-year fixed effects,
# weighted by population in the election year, clustered by city.
#
# Inputs:  data/derived/panel_license_crash.rds, data/derived/onpremise_sales.rds,
#          data/derived/election_history.rds, data/derived/allcity_controls.rds,
#          data/raw/citypop_2003-2023.dta
# Outputs: data/derived/stacked_panel.rds
#          output/tables/stacked_rd_*.csv
#          output/figures/stacked_rd/*.png

library(tidyverse)
library(haven)
library(fixest)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/stacked_rd", recursive = TRUE, showWarnings = FALSE)

covariates <- c("lncitypop", "Protestant_pct", "realGDPpc", "hispop_pct",
                "vehicles_permillion", "totalDVMT_permillion", "pol_ratio",
                "edu_hsonly", "aur", "election_turnout")

# ---- Stacked panel ------------------------------------------------------------------
citypop <- read_dta("data/raw/citypop_2003-2023.dta") %>% zap_labels() %>%
  rename(citypop = population)

stacked <- read_rds("data/derived/panel_license_crash.rds") %>%
  filter(between(year, 2003, 2023)) %>%
  left_join(read_rds("data/derived/onpremise_sales.rds"), by = c("city", "year")) %>%
  inner_join(read_rds("data/derived/election_history.rds"), by = c("city", "year")) %>%
  mutate(event_time = year - cohort) %>%
  filter(between(event_time, -5, 5)) %>%
  group_by(city_id) %>%
  mutate(
    # Treated: the election passed. Control: it failed and no election passed
    # in the following five years.
    treated = any(D_year == 1 & year == cohort),
    pass_next5 = sum(D_year[between(event_time, 1, 5)])
  ) %>%
  ungroup() %>%
  filter(treated | pass_next5 == 0) %>%
  mutate(group = if_else(treated, "treated", "control")) %>%
  inner_join(citypop, by = c("city", "year")) %>%
  group_by(city_id) %>%
  mutate(basepop = citypop[event_time == 0][1]) %>%     # population in the election year
  ungroup() %>%
  filter(!is.na(basepop)) %>%
  inner_join(read_rds("data/derived/allcity_controls.rds"), by = c("city", "year")) %>%
  mutate(election_turnout = (for_vote + against_vote) / citypop) %>%
  filter(!city %in% c("DISH", "Jolly", "Coffee City", "Emhouse")) %>%
  mutate(tot_lics_per         = lic_all / citypop * 1000,
         tot_lics_on_per      = lic_on  / citypop * 1000,
         tot_lics_off_per     = lic_off / citypop * 1000,
         on_sale_all_per      = total_receipts_adj / citypop,
         on_sale_bw_per       = beer_wine_receipts_adj / citypop,
         on_sale_lq_per       = liquor_receipts_adj / citypop,
         crashes_ALL_per      = totalcrashes_all / citypop * 1000,
         crashes_DUI_per      = totalcrashes_dui / citypop * 1000,
         crashes_NonDUI_per   = totalcrashes_non_dui / citypop * 1000,
         Fatalcrashes_ALL_per = fatalcrashes_all / citypop * 1000,
         Fatalcrashes_DUI_per = fatalcrashes_dui / citypop * 1000)

# Election history k years later ("lag": event time -k) and earlier ("lead":
# event time +k), within each unit; 0 if outside the observed window
for (k in 1:5) for (v in c("D", "M", "V")) {
  shifted <- stacked %>% select(city_id, year, value = !!sym(paste0(v, "_year")))
  stacked <- stacked %>%
    left_join(shifted %>% mutate(year = year - k) %>% rename(!!paste0(v, "_lag", k) := value),
              by = c("city_id", "year")) %>%
    left_join(shifted %>% mutate(year = year + k) %>% rename(!!paste0(v, "_lead", k) := value),
              by = c("city_id", "year"))
}
stacked <- stacked %>% mutate(across(matches("_(lag|lead)\\d$"), ~ replace_na(.x, 0)))
write_rds(stacked, "data/derived/stacked_panel.rds")

cat("Stacked panel:", nrow(stacked), "obs,", n_distinct(stacked$city_id), "elections,",
    n_distinct(stacked$city), "cities,", n_distinct(stacked$cohort), "cohorts\n")
stacked %>% distinct(city_id, group) %>% count(group) %>% print()

history_terms <- c(paste0(rep(c("D", "M", "V"), each = 10),
                          c("_lag5", "_lag4", "_lag3", "_lag2", "_year",
                            "_lead1", "_lead2", "_lead3", "_lead4", "_lead5")))
event_terms <- c(D_lag5 = -5, D_lag4 = -4, D_lag3 = -3, D_lag2 = -2, D_year = 0,
                 D_lead1 = 1, D_lead2 = 2, D_lead3 = 3, D_lead4 = 4, D_lead5 = 5)

stacked_rd <- function(data, y, covs = covariates) {
  feols(as.formula(paste(y, "~", paste(c(covs, history_terms), collapse = " + "),
                         "| city_id + cohort^year")),
        data = data, weights = ~ basepop, cluster = ~ city)
}

# Event-study coefficients (event time -1 is the reference)
event_study <- function(model) {
  tibble(term = names(event_terms), time = event_terms) %>%
    mutate(beta = coef(model)[term], se = se(model)[term]) %>%
    select(-term) %>%
    bind_rows(tibble(time = -1, beta = 0, se = 0)) %>%
    mutate(across(c(beta, se), ~ replace_na(.x, 0))) %>%
    arrange(time)
}

# ---- Main estimates -------------------------------------------------------------------
outcomes <- c(tot_lics_per = "Total Licenses", tot_lics_on_per = "On-Premise Licenses",
              tot_lics_off_per = "Off-Premise Licenses", on_sale_all_per = "On-Premise Sales",
              on_sale_bw_per = "On-Premise Beer&Wine Sales", on_sale_lq_per = "On-Premise Liquor Sales",
              crashes_ALL_per = "All Crash Rate", crashes_DUI_per = "DUI Crash Rate",
              crashes_NonDUI_per = "Non-DUI Crash Rate", Fatalcrashes_ALL_per = "All Fatal Crash Rate",
              Fatalcrashes_DUI_per = "DUI Fatal Crash Rate")

results <- list()
events  <- list()
for (y in names(outcomes)) {
  m  <- stacked_rd(stacked, y)
  V  <- vcov(m)
  df <- degrees_freedom(m, "t")
  # Average effect in years 0-2 and 3-5 after the election
  avg <- map_dfr(list(c("D_year", "D_lead1", "D_lead2"), c("D_lead3", "D_lead4", "D_lead5")), function(terms) {
    w <- setNames(rep(1 / 3, 3), terms)
    b <- sum(w * coef(m)[terms])
    s <- sqrt(drop(t(w) %*% V[terms, terms] %*% w))
    tibble(b = b, se = s, p = 2 * pt(-abs(b / s), df))
  })
  results[[y]] <- tibble(outcome = y,
                         b02 = avg$b[1], se02 = avg$se[1], p02 = avg$p[1],
                         b35 = avg$b[2], se35 = avg$se[2], p35 = avg$p[2],
                         premean = mean(stacked[[y]][stacked$event_time == -1], na.rm = TRUE),
                         r2a = r2(m, "ar2"), N = nobs(m))
  events[[y]] <- event_study(m) %>% mutate(outcome = y)
}
results <- bind_rows(results)
events  <- bind_rows(events)
write_csv(results, "output/tables/stacked_rd_results.csv")
write_csv(events,  "output/tables/stacked_rd_event_study.csv")
print(results, width = 200)

# ---- Event-study figures --------------------------------------------------------------
event_plot <- function(d, ytitle, ref_line = -0.5) {
  ggplot(d, aes(time, beta)) +
    geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = ref_line, colour = "red", linetype = "dashed", linewidth = 0.8) +
    geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se, linetype = "95% CI"),
                  colour = "blue", width = 0.25, linewidth = 1) +
    geom_line(colour = "blue", linewidth = 1) +
    geom_point(aes(shape = "Estimate"), colour = "blue", size = 2.5) +
    scale_x_continuous(breaks = -5:5) +
    labs(x = "Years since Election Passage", y = ytitle, shape = NULL, linetype = NULL) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank(), legend.position = c(0.13, 0.9),
          legend.background = element_blank(), legend.spacing.y = unit(0, "pt"))
}
for (y in names(outcomes)) {
  ggsave(sprintf("output/figures/stacked_rd/StackedDRD_%s.png", y),
         event_plot(filter(events, outcome == y), paste("Effect on", outcomes[[y]])),
         width = 7, height = 5, dpi = 230)
}

# ---- On-premise sales by beverage (no covariates) ---------------------------------------
by_beverage <- bind_rows(
  event_study(stacked_rd(stacked, "on_sale_bw_per", covs = NULL)) %>% mutate(label = "Beer & Wine Sales"),
  event_study(stacked_rd(stacked, "on_sale_lq_per", covs = NULL)) %>% mutate(label = "Liquor Sales")
)
write_csv(by_beverage, "output/tables/stacked_rd_onpremise_by_beverage.csv")
g <- by_beverage %>%
  mutate(time = if_else(label == "Beer & Wine Sales", time + 0.15, time)) %>%
  ggplot(aes(time, beta, colour = label, shape = label, linetype = label)) +
  geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2,
                linewidth = 1, linetype = "solid") +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_colour_manual(values = c("Beer & Wine Sales" = "#E69F00", "Liquor Sales" = "blue")) +
  scale_linetype_manual(values = c("Beer & Wine Sales" = "dashed", "Liquor Sales" = "solid")) +
  scale_x_continuous(breaks = -5:5) +
  labs(x = "Years since Election Passage", y = "Effect on On-Premise Sales Per Capita",
       colour = NULL, shape = NULL, linetype = NULL) +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")
ggsave("output/figures/stacked_rd/StackedDRD_onpremise_sales_byCategory.png", g,
       width = 7, height = 5, dpi = 230)

# ---- Licenses and crashes by election type ------------------------------------------------
# Treated units: elections whose ballot legalized this type; all clean controls
by_type <- list()
for (tp in c("beerwine", "hardalcohol")) {
  keep <- stacked$city_id[stacked$year == stacked$cohort &
                            (stacked$group == "control" | stacked[[tp]] == 1)]
  for (y in c("tot_lics_per", "crashes_ALL_per")) {
    es <- event_study(stacked_rd(filter(stacked, city_id %in% keep), y))
    by_type[[paste(tp, y)]] <- mutate(es, type = tp, outcome = y)
    stub <- if (y == "tot_lics_per") "licenses" else "Crashes"
    ggsave(sprintf("output/figures/stacked_rd/StackedDRD_All_%s_%s_election.png", stub, tp),
           event_plot(es, if (y == "tot_lics_per") "Effect on Licenses" else "Effect on Crashes",
                      ref_line = -1),
           width = 7, height = 5, dpi = 230)
  }
}
write_csv(bind_rows(by_type), "output/tables/stacked_rd_by_election_type.csv")
