# 08_offpremise_sales.R
#
# Off-premise alcohol sales (NielsenIQ Retail Scanner Data, county level,
# 2006-2020). NielsenIQ files report only 3-digit ZIP codes, so sales are
# measured by county and the elections are county-level aggregates of all
# local option elections (county, city, and precinct) in the county.
#   - RD: each county's first election in the sample, one year after the vote
#   - Stacked dynamic RD (all elections), overall and by beverage
#   - Difference-in-differences (TWFE and Callaway-Sant'Anna)
#
# RESTRICTED DATA: requires data/restricted/store_movement_TX_2006_2020_byGroup.dta
# (NielsenIQ via the Kilts Center). The script stops quietly if it is absent.
#
# Inputs:  data/derived/elections.rds, data/derived/allcity_controls.rds,
#          data/raw/allcity_names_withcounty.dta, data/raw/countypop_03-23.dta,
#          data/raw/countypop_2006.dta
# Outputs: output/tables/offprem_*.csv, output/figures/offprem/*.png

library(tidyverse)
library(haven)
library(rdrobust)
library(fixest)
library(did)

sales_file <- "data/restricted/store_movement_TX_2006_2020_byGroup.dta"
if (!file.exists(sales_file)) {
  message("NielsenIQ data not found; skipping off-premise sales.")
} else {

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/offprem", recursive = TRUE, showWarnings = FALSE)

# ---- County-level election history ---------------------------------------------------
prohibit_issues <- c(
  "prohibit the legal sale of all alcoholic beverages including mixed beverages",
  "prohibit the legal sale of all alcoholic beverages for off-premises consumption only"
)
city_county <- read_dta("data/raw/allcity_names_withcounty.dta") %>% select(city, county)

county_props <- read_rds("data/derived/elections.rds") %>%       # all levels
  filter(!(status_before == "wet" & status_current == "dry"),
         !issueclean %in% prohibit_issues) %>%
  left_join(city_county, by = "city") %>%
  mutate(county = coalesce(county, county1),             # city's county, else the record's
         issue = str_remove_all(str_to_lower(issueclean), "[,.]"),
         onpremise  = str_detect(issue, "on the premises|in restaurants|on-premises"),
         offpremise = str_detect(issue, "off-premises"),
         neither    = !onpremise & !offpremise,          # unrestricted ballots: both
         onpremise  = onpremise | neither,
         offpremise = offpremise | neither,
         beerwine   = str_detect(issue, "beer|wine|all alcoholic beverages"),
         hardalcohol = str_detect(issue, "spirits") |
           (str_detect(issue, "mixed beverage|all alcoholic beverages") &
              !str_detect(issue, "except mixed beverages")))

# One record per county-year: average votes by result; a year with any
# passing election counts as passed
county_history <- county_props %>%
  group_by(county, year, result) %>%
  summarise(for_vote = floor(mean(for_vote) + 0.5),          # round half up
            against_vote = floor(mean(against_vote) + 0.5),
            across(c(onpremise, offpremise, beerwine, hardalcohol), ~ as.numeric(any(.x))),
            .groups = "drop") %>%
  group_by(county, year) %>%
  filter(result == "passed" | !any(result == "passed")) %>%
  ungroup() %>%
  transmute(county, year, M_year = 1, D_year = as.numeric(result == "passed"),
            V_year = for_vote / (for_vote + against_vote),
            onpremise, offpremise, beerwine, hardalcohol)

# One unit per county-level election, observed every year
county_units <- county_history %>%
  distinct(county, cohort = year) %>%
  cross_join(tibble(year = 2003:2023)) %>%
  left_join(county_history, by = c("county", "year")) %>%
  mutate(across(M_year:hardalcohol, ~ replace_na(.x, 0)),
         unit = str_c(county, "_", cohort))

# ---- Sales panel ---------------------------------------------------------------------------
sales <- read_dta(sales_file) %>%
  zap_labels() %>%
  rename(county = fips_county_descr) %>%
  mutate(county = recode(county, "Mclennan" = "McLennan", "De Witt" = "DeWitt",
                         "Dewitt" = "DeWitt", "San Augustin" = "San Augustine",
                         "Mcmullen" = "McMullen")) %>%
  inner_join(read_dta("data/raw/countypop_03-23.dta") %>% zap_labels(), by = c("year", "county")) %>%
  # Per capita (total population), as for on-premise sales
  mutate(sales_pc          = sales_dollar_adj / countypop,
         sales_pc_beerwine = (sales_dollar5001_adj + sales_dollar5003_adj) / countypop,
         sales_pc_liquor   = sales_dollar5002_adj / countypop) %>%
  inner_join(read_dta("data/raw/countypop_2006.dta") %>% zap_labels() %>% select(county, countypop2006),
             by = "county") %>%
  select(county, year, countypop, sales_pc, sales_pc_beerwine, sales_pc_liquor, countypop2006)

# Stacked panel: elections 2006 or later, five years before/after, passed
# elections and failed elections not followed by a passed one within five years
# (treatment and later passes come from the election history, whether or not
# sales are observed in those years)
stacked <- county_units %>%
  filter(cohort >= 2006) %>%
  mutate(event_time = year - cohort) %>%
  group_by(unit) %>%
  mutate(treated = any(D_year == 1 & event_time == 0),
         pass_next5 = sum(D_year[between(event_time, 1, 5)])) %>%
  ungroup() %>%
  filter(treated | pass_next5 == 0, between(event_time, -5, 5)) %>%
  inner_join(sales, by = c("county", "year")) %>%
  filter(between(year, 2006, 2020))

for (k in 1:5) for (v in c("D", "M", "V")) {
  shifted <- stacked %>% select(unit, year, value = !!sym(paste0(v, "_year")))
  stacked <- stacked %>%
    left_join(shifted %>% mutate(year = year - k) %>% rename(!!paste0(v, "_lag", k) := value),
              by = c("unit", "year")) %>%
    left_join(shifted %>% mutate(year = year + k) %>% rename(!!paste0(v, "_lead", k) := value),
              by = c("unit", "year"))
}
stacked <- stacked %>% mutate(across(matches("_(lag|lead)\\d$"), ~ replace_na(.x, 0)))
write_rds(stacked, "data/restricted/county_stacked_panel.rds")       # contains NielsenIQ data
cat("County stacked panel:", nrow(stacked), "obs,", n_distinct(stacked$unit), "elections,",
    n_distinct(stacked$county), "counties\n")

# RD sample: each county's first election from 2006 on (whatever happens
# afterwards, as in the city RD), five years before and after, with its vote margin
first_unit <- county_units %>%
  filter(cohort >= 2006) %>%
  group_by(county) %>% filter(cohort == min(cohort)) %>% ungroup() %>%
  mutate(event_time = year - cohort) %>%
  filter(between(event_time, -5, 5)) %>%
  inner_join(sales, by = c("county", "year")) %>%
  filter(between(year, 2006, 2020)) %>%
  left_join(county_history %>% transmute(county, cohort = year, vote_margin = V_year - 0.5),
            by = c("county", "cohort"))

# ---- RD ------------------------------------------------------------------------------------
# County controls: city controls averaged within county, weighted by city population
county_controls <- read_rds("data/derived/allcity_controls.rds") %>%
  left_join(city_county, by = "city") %>%
  filter(between(year, 2006, 2020)) %>%
  mutate(citypop = round(exp(lncitypop))) %>%
  group_by(county, year) %>%
  summarise(across(c(aur, Protestant_pct, realGDPpc, edu_hsonly, pol_ratio, hispop_pct,
                     vehicles_permillion), ~ weighted.mean(.x, citypop, na.rm = TRUE)),
            .groups = "drop")
covariates <- c("lncountypop", "Protestant_pct", "realGDPpc", "hispop_pct",
                "vehicles_permillion", "pol_ratio", "edu_hsonly", "aur")

rd <- first_unit %>%
  group_by(county) %>% mutate(basepop = countypop[event_time == 0][1]) %>% ungroup() %>%
  inner_join(county_controls, by = c("county", "year")) %>%
  mutate(lncountypop = log(countypop),
         cluster_id = as.integer(factor(county)))

specs <- tribble(~spec, ~p, ~h,  ~weighted,
                 1,     1,  0.5, FALSE,
                 2,     2,  0.5, FALSE,
                 3,     1,  0.5, TRUE,
                 4,     2,  0.5, TRUE,
                 5,     1,  0.2, FALSE,     # too few counties for a data-driven
                 6,     2,  0.2, FALSE,     # bandwidth: fixed at 0.2
                 7,     1,  0.2, TRUE,
                 8,     2,  0.2, TRUE)
rd_fit <- function(data, p, h, weighted) {
  d <- data %>% drop_na(all_of(c("sales_pc", "vote_margin", covariates, if (weighted) "basepop")))
  fit <- rdrobust(d$sales_pc, d$vote_margin, c = 0, p = p, h = h,
                  covs = as.matrix(d[covariates]),
                  weights = if (weighted) d$basepop else NULL,
                  cluster = d$cluster_id, vce = "hc0")
  tibble(b = fit$coef[3], se = fit$se[3], pv = fit$pv[3], N = sum(fit$N_h))
}
post <- rd %>% filter(event_time == 1)
rd_sales <- specs %>%
  mutate(est = pmap(list(p, h, weighted), ~ rd_fit(post, ..1, ..2, ..3))) %>%
  unnest(est) %>%
  mutate(outcome = "sales_pc",
         pre_mean = mean(rd$sales_pc[rd$event_time == -1], na.rm = TRUE))
# Changes from one year before to one year after (covariates at t-1)
changes <- rd %>%
  filter(event_time %in% c(-1, 1)) %>%
  group_by(county) %>% filter(n() == 2) %>% arrange(event_time, .by_group = TRUE) %>%
  summarise(across(c(vote_margin, basepop, cluster_id, all_of(covariates)), first),
            sales_pc = last(sales_pc) - first(sales_pc), .groups = "drop")
rd_sales_changes <- specs %>%
  mutate(est = pmap(list(p, h, weighted), ~ rd_fit(changes, ..1, ..2, ..3))) %>%
  unnest(est) %>%
  mutate(outcome = "sales_pc")
write_csv(rd_sales_changes, "output/tables/offprem_rd_changes.csv")
write_csv(rd_sales, "output/tables/offprem_rd_results.csv")
print(rd_sales)

# RD plots (same construction as 05_rd.R)
for (t in c(-1, 1)) for (deg in 1:2) {
  d <- rd %>% filter(event_time == t) %>% drop_na(sales_pc, basepop, vote_margin) %>%
    group_by(vote_margin) %>% summarise(y = weighted.mean(sales_pc, basepop), .groups = "drop") %>%
    mutate(pass = vote_margin > 0,
           bin = floor(round(vote_margin * 100, 8)) / 100,
           bin = if_else(vote_margin == 0, -0.01, bin) + 0.005,
           bin = case_when(bin == min(bin) ~ bin + 0.01, bin == max(bin) ~ bin - 0.01, TRUE ~ bin)) %>%
    group_by(bin) %>% mutate(y_bin = mean(y)) %>% ungroup()
  g <- ggplot(d, aes(vote_margin, y)) +
    geom_smooth(aes(group = pass), method = "lm", formula = y ~ poly(x, deg, raw = TRUE),
                colour = "blue", fill = "blue", alpha = 0.2, linewidth = 1.3) +
    geom_point(aes(bin, y_bin), colour = "grey50") +
    geom_vline(xintercept = 0, colour = "red", linetype = "dashed", linewidth = 1) +
    labs(x = "Vote Margin", y = "Off-premise Sales (per capita)") +
    theme_bw(base_size = 16) + theme(panel.grid.minor = element_blank())
  ggsave(sprintf("output/figures/offprem/RegularRD_OffPremise_sales_%s1_%s.png",
                 if (t == -1) "-" else "+", if (deg == 1) "linear" else "quadratic"),
         g, width = 7, height = 5, dpi = 230)
}

# ---- Stacked dynamic RD --------------------------------------------------------------------
history_terms <- c(paste0(rep(c("D", "M", "V"), each = 10),
                          c("_lag5", "_lag4", "_lag3", "_lag2", "_year",
                            "_lead1", "_lead2", "_lead3", "_lead4", "_lead5")))
event_terms <- c(D_lag5 = -5, D_lag4 = -4, D_lag3 = -3, D_lag2 = -2, D_year = 0,
                 D_lead1 = 1, D_lead2 = 2, D_lead3 = 3, D_lead4 = 4, D_lead5 = 5)
stacked_events <- list()
stacked_results <- list()
for (y in c("sales_pc", "sales_pc_beerwine", "sales_pc_liquor")) {
  m <- feols(as.formula(paste(y, "~", paste(history_terms, collapse = " + "), "| unit + cohort^year")),
             data = stacked, weights = ~ countypop2006, cluster = ~ county)
  V <- vcov(m); df <- degrees_freedom(m, "t")
  avg <- map_dfr(list(c("D_year", "D_lead1", "D_lead2"), c("D_lead3", "D_lead4", "D_lead5")), function(terms) {
    w <- rep(1 / 3, 3); b <- sum(w * coef(m)[terms])
    s <- sqrt(drop(t(w) %*% V[terms, terms] %*% w))
    tibble(b = b, se = s, p = 2 * pt(-abs(b / s), df))
  })
  stacked_results[[y]] <- tibble(outcome = y, b02 = avg$b[1], se02 = avg$se[1], p02 = avg$p[1],
                                 b35 = avg$b[2], se35 = avg$se[2], p35 = avg$p[2],
                                 premean = mean(stacked[[y]][stacked$event_time == -1], na.rm = TRUE),
                                 r2a = r2(m, "ar2"), N = nobs(m))
  stacked_events[[y]] <- tibble(outcome = y, time = event_terms,
                                beta = coef(m)[names(event_terms)], se = se(m)[names(event_terms)]) %>%
    bind_rows(tibble(outcome = y, time = -1, beta = 0, se = 0)) %>% arrange(time)
}
write_csv(bind_rows(stacked_results), "output/tables/offprem_stacked_rd_results.csv")
write_csv(bind_rows(stacked_events), "output/tables/offprem_stacked_rd_event_study.csv")
print(bind_rows(stacked_results))

g <- stacked_events$sales_pc %>%
  ggplot(aes(time, beta)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = -0.5, colour = "red", linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), colour = "blue",
                width = 0.25, linewidth = 1) +
  geom_line(colour = "blue", linewidth = 1) + geom_point(colour = "blue", size = 2.5) +
  scale_x_continuous(breaks = -5:5) +
  labs(x = "Years since Election Passage", y = "Effect on Off-Premise Sales") +
  theme_bw(base_size = 16) + theme(panel.grid.minor = element_blank())
ggsave("output/figures/offprem/StackedDRD_offpremise_sales.png", g, width = 7, height = 5, dpi = 230)

g <- bind_rows(stacked_events$sales_pc_beerwine %>% mutate(label = "Beer & Wine Sales", time = time + 0.15),
               stacked_events$sales_pc_liquor %>% mutate(label = "Liquor Sales")) %>%
  ggplot(aes(time, beta, colour = label, shape = label, linetype = label)) +
  geom_hline(yintercept = 0, colour = "grey70", linetype = "dashed") +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2,
                linewidth = 1, linetype = "solid") +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_colour_manual(values = c("Beer & Wine Sales" = "#E69F00", "Liquor Sales" = "blue")) +
  scale_linetype_manual(values = c("Beer & Wine Sales" = "dashed", "Liquor Sales" = "solid")) +
  scale_x_continuous(breaks = -5:5) +
  labs(x = "Years since Election Passage", y = "Effect on Off-Premise Sales Per Capita",
       colour = NULL, shape = NULL, linetype = NULL) +
  theme_bw(base_size = 16) + theme(panel.grid.minor = element_blank(), legend.position = "top")
ggsave("output/figures/offprem/StackedDRD_offpremise_sales_byCategory.png", g, width = 7, height = 5, dpi = 230)

# ---- Difference-in-differences --------------------------------------------------------------
# Counties with an election in 2006-2019, treated from their first passing
# election (as in the city DiD); counties whose elections all failed are
# never treated; all years 2006-2020
first_pass <- county_history %>%
  filter(between(year, 2006, 2019)) %>%
  group_by(county) %>%
  summarise(treat_year = if (any(D_year == 1)) min(year[D_year == 1]) else 0)
did_panel <- first_pass %>%
  semi_join(first_unit, by = "county") %>%
  inner_join(sales, by = "county") %>%
  filter(between(year, 2006, 2020)) %>%
  mutate(post = as.numeric(treat_year > 0 & year >= treat_year),
         unit_id = as.integer(factor(county))) %>%
  drop_na(sales_pc)
twfe <- feols(sales_pc ~ post | unit_id + year, data = did_panel,
              weights = ~ countypop2006, cluster = ~ unit_id)
att <- att_gt(yname = "sales_pc", tname = "year", idname = "unit_id", gname = "treat_year",
              data = did_panel, weightsname = "countypop2006", control_group = "notyettreated",
              base_period = "universal", est_method = "dr", bstrap = FALSE,
              allow_unbalanced_panel = TRUE, faster_mode = FALSE)
es <- aggte(att, type = "dynamic", min_e = -5, max_e = 5, bstrap = FALSE, na.rm = TRUE)
did_sales <- tibble(
  outcome = "sales_pc", method = c("TWFE", "CSDID"),
  b  = c(coef(twfe)["post"], es$overall.att),
  se = c(se(twfe)["post"], es$overall.se),
  p  = c(pvalue(twfe)["post"], 2 * pnorm(-abs(es$overall.att / es$overall.se))),
  N  = nrow(did_panel),
  pre_mean = mean(rd$sales_pc[rd$event_time == -1], na.rm = TRUE))
write_csv(did_sales, "output/tables/offprem_did_results.csv")
print(did_sales)
cat("County DiD:", n_distinct(did_panel$county), "counties,",
    n_distinct(did_panel$county[did_panel$treat_year > 0]), "treated\n")

g <- tibble(time = es$egt, beta = es$att.egt, se = replace_na(es$se.egt, 0)) %>%
  ggplot(aes(time, beta)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 1) +
  geom_vline(xintercept = -1, colour = "red", linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), colour = "blue", width = 0, linewidth = 1) +
  geom_point(colour = "blue", size = 2.5) +
  scale_x_continuous(breaks = -5:5) +
  labs(x = "Years from treatment", y = "Estimate and 95% CI") +
  theme_bw(base_size = 16) + theme(panel.grid.minor = element_blank())
ggsave("output/figures/offprem/CSDID_OffPremise_Sales.png", g, width = 7, height = 5, dpi = 230)

}
