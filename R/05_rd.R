# 05_rd.R
#
# Regression discontinuity: each city's first election in the study period,
# outcomes one year after the vote, running variable = vote margin.
#   - Covariate balance (one year before the vote) and density test
#   - Main RD estimates, 8 specifications
#   - RD on changes in outcomes (t-1 to t+1)
#   - Estimates across bandwidths
#   - RD plots (linear and quadratic fits, one year before and after)
#
# Estimates are bias-corrected with robust standard errors clustered by
# election (Calonico, Cattaneo and Titiunik 2014), from rdrobust 3.0.0.
#
# Inputs:  data/derived/analysis_panel.rds, data/derived/elections.rds
#          data/raw/Elections_all_simplified_03_19.dta  (votes, for turnout)
# Outputs: data/derived/rd_panel.rds
#          output/tables/rd_*.csv
#          output/figures/rd/*.png

library(tidyverse)
library(haven)
library(rdrobust)
library(rddensity)
library(lpdensity)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/rd", recursive = TRUE, showWarnings = FALSE)

# ---- RD panel -----------------------------------------------------------------
# Turnout = votes cast (largest for and against counts across the city's
# propositions that year) / population in the election year
votes <- read_dta("data/raw/Elections_all_simplified_03_19.dta") %>%
  zap_labels() %>%
  group_by(city, elec_year) %>%
  summarise(vote_for = max(for_vote), vote_against = max(against_vote),
            .groups = "drop")

rd_panel <- read_rds("data/derived/analysis_panel.rds") %>%
  inner_join(votes, by = c("city", "elec_year")) %>%
  group_by(city_elecyear) %>%
  mutate(election_turnout = (vote_for + vote_against) / citypop[year == elec_year][1],
         basepop = citypop[event_time == 0][1]) %>%     # population in the election year
  ungroup() %>%
  select(-vote_for, -vote_against) %>%
  # Cities with incorporation changes or irregular reporting
  filter(!city %in% c("DISH", "Jolly", "Coffee City", "Emhouse")) %>%
  mutate(NonFatalcrashes_ALL_per = AnyInjuryCrashes_ALL_per + NonInjuryCrashes_ALL_per,
         NonFatalcrashes_DUI_per = AnyInjuryCrashes_DUI_per + NonInjuryCrashes_DUI_per,
         cluster_id = as.integer(factor(city_elecyear)))
write_rds(rd_panel, "data/derived/rd_panel.rds")

covariates <- c("lncitypop", "Protestant_pct", "realGDPpc", "hispop_pct",
                "vehicles_permillion", "totalDVMT_permillion", "pol_ratio",
                "edu_hsonly", "aur", "election_turnout")

outcomes <- c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per", "on_sale_all_per",
              "crashes_ALL_per", "crashes_DUI_per", "crashes_NonDUI_per",
              "Fatalcrashes_ALL_per", "NonFatalcrashes_ALL_per",
              "Fatalcrashes_DUI_per", "NonFatalcrashes_DUI_per")

# Each city's first election, one year before / after the vote
pre  <- rd_panel %>% filter(event_time == -1) %>% arrange(city, elec_year) %>% distinct(city, .keep_all = TRUE)
post <- rd_panel %>% filter(event_time ==  1) %>% arrange(city, elec_year) %>% distinct(city, .keep_all = TRUE)
count(post, result)

# Columns of the RD table: (1)-(4) full bandwidth, (5)-(8) MSE-optimal bandwidth
specs <- tribble(
  ~spec, ~p, ~h,  ~weighted,
  1,     1,  0.5, FALSE,
  2,     2,  0.5, FALSE,
  3,     1,  0.5, TRUE,
  4,     2,  0.5, TRUE,
  5,     1,  NA,  FALSE,
  6,     2,  NA,  FALSE,
  7,     1,  NA,  TRUE,
  8,     2,  NA,  TRUE
)

# Bias-corrected estimate, robust SE and p-value from one rdrobust call
rd_est <- function(data, y, p, h = NA, weighted = FALSE, covs = covariates) {
  d <- data %>% drop_na(all_of(c(y, "vote_margin", covs, if (weighted) "basepop")))
  fit <- rdrobust(d[[y]], d$vote_margin, c = 0, p = p,
                  h = if (is.na(h)) NULL else h,
                  covs = if (length(covs) > 0) as.matrix(d[covs]) else NULL,
                  weights = if (weighted) d$basepop else NULL,
                  cluster = d$cluster_id, vce = "hc0")
  tibble(b = fit$coef[3], se = fit$se[3], pv = fit$pv[3],
         N = sum(fit$N_h), h_opt = fit$bws[1, 1])
}

# ---- Covariate balance, one year before the vote --------------------------------
# Full and MSE-optimal bandwidth, linear and quadratic, no covariates or weights
balance_specs <- tribble(~spec, ~p, ~h,
                         1,     1,  0.5,
                         2,     2,  0.5,
                         3,     1,  NA,
                         4,     2,  NA)
balance <- expand_grid(covariate = covariates, balance_specs) %>%
  mutate(est = pmap(list(covariate, p, h), ~ rd_est(pre, ..1, ..2, ..3, covs = NULL))) %>%
  unnest(est)
write_csv(balance, "output/tables/rd_balance.csv")

# ---- Main RD estimates, one year after the vote ----------------------------------
rd_results <- expand_grid(outcome = outcomes, specs) %>%
  mutate(est = pmap(list(outcome, p, h, weighted), ~ rd_est(post, ..1, ..2, ..3, ..4))) %>%
  unnest(est)
rd_means <- pre %>%
  summarise(across(all_of(outcomes), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "pre_mean")
rd_results <- left_join(rd_results, rd_means, by = "outcome")
write_csv(rd_results, "output/tables/rd_results.csv")

# ---- RD on changes, t-1 to t+1 (covariates measured at t-1) -----------------------
changes <- rd_panel %>%
  filter(event_time %in% c(-1, 1)) %>%
  group_by(city) %>% filter(elec_year == min(elec_year)) %>% ungroup() %>%
  group_by(cluster_id) %>% filter(n() == 2) %>% arrange(event_time, .by_group = TRUE) %>%
  summarise(across(c(vote_margin, basepop, all_of(covariates)), first),
            across(all_of(outcomes), ~ last(.x) - first(.x)), .groups = "drop")
rd_changes <- expand_grid(outcome = outcomes, specs) %>%
  mutate(est = pmap(list(outcome, p, h, weighted), ~ rd_est(changes, ..1, ..2, ..3, ..4))) %>%
  unnest(est)
write_csv(rd_changes, "output/tables/rd_changes.csv")
cat("RD on changes:", nrow(changes), "cities\n")

# ---- Estimates across bandwidths (preferred specification) ------------------------
bw_data <- post %>%
  left_join(changes %>% select(cluster_id, d_crashes_ALL_per = crashes_ALL_per), by = "cluster_id")
bw_outcomes <- c("tot_lics_off_per", "crashes_ALL_per", "crashes_DUI_per", "d_crashes_ALL_per")
rd_bandwidth <- expand_grid(outcome = bw_outcomes, h = seq(0.04, 0.44, by = 0.02)) %>%
  mutate(est = map2(outcome, h, ~ rd_est(bw_data, .x, 1, .y))) %>%
  unnest(est) %>%
  group_by(outcome) %>%
  mutate(h_opt = rd_est(bw_data, first(outcome), 1)$h_opt) %>%
  ungroup()
write_csv(rd_bandwidth, "output/tables/rd_bandwidth.csv")

bw_labels <- c(tot_lics_off_per = "Off-premise licenses", crashes_ALL_per = "All crashes",
               crashes_DUI_per = "DUI crashes",
               d_crashes_ALL_per = "Change in all crashes (t-1 to t+1)")
rd_bandwidth %>%
  mutate(outcome = factor(bw_labels[outcome], levels = bw_labels)) %>%
  ggplot(aes(h, b)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_vline(aes(xintercept = h_opt), colour = "red", linetype = "dashed") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), colour = "blue", width = 0.008) +
  geom_point(colour = "blue") +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 0.4, 0.1), limits = c(0, 0.45)) +
  labs(x = "Bandwidth (vote margin)", y = "Estimate and 95% CI") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank())
ggsave("output/figures/rd/RD_bandwidth_sensitivity.png", width = 10, height = 8, dpi = 160)

# ---- Density test (McCrary-type, Cattaneo, Jansson and Ma 2018) --------------------
# All election records in previously dry jurisdictions, 2003-2019
margins <- read_rds("data/derived/elections.rds") %>%
  filter(status_before != "wet", !str_detect(issueclean, "^prohibit")) %>%
  pull(vote_margin)
density_test <- rddensity(margins, c = 0, p = 2, q = 2)
cat("Density test p-value:", round(density_test$test$p_jk, 4), "\n")
write_csv(tibble(p_value = density_test$test$p_jk, n = length(margins)),
          "output/tables/rd_density_test.csv")
pdf(NULL)                                   # rdplotdensity prints its plot
density_plot <- rdplotdensity(density_test, margins, plotRange = range(margins),
                              title = "", xlabel = "Vote Margin", ylabel = "Density")
invisible(dev.off())
ggsave("output/figures/rd/density_test.png", density_plot$Estplot + theme(legend.position = "none"),
       width = 7, height = 5, dpi = 230)

# ---- Covariate balance figure: binned means one year before the vote --------------
cov_labels <- c(lncitypop = "Log(city population)", aur = "Unemployment Rate",
                realGDPpc = "GDP per capita (in 1000 dollars)", edu_hsonly = "High-school-only Share",
                election_turnout = "Election Turnout", Protestant_pct = "Protestant Share",
                hispop_pct = "Hispanic Share", pol_ratio = "Democrat Share",
                vehicles_permillion = "Registered Vehicles (per million)",
                totalDVMT_permillion = "Total Daily Vehicle Miles Traveled (per million)")
balance_bins <- map_dfr(names(cov_labels), function(v) {
  d <- pre %>% drop_na(all_of(v))
  bins <- rdplot(d[[v]], d$vote_margin, c = 0, p = 1, h = 0.5, hide = TRUE)$vars_bins
  tibble(covariate = cov_labels[[v]], x = bins$rdplot_mean_x, y = bins$rdplot_mean_y)
})
balance_bins %>%
  mutate(covariate = factor(covariate, levels = cov_labels)) %>%
  ggplot(aes(x, y)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(colour = "grey45") +
  facet_wrap(~ covariate, scales = "free_y", ncol = 3) +
  labs(x = "Vote Margin", y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank())
ggsave("output/figures/rd/RD_covariates_balance.png", width = 12, height = 12, dpi = 200)

# ---- RD plots ---------------------------------------------------------------------
# Means by vote margin (weighted by population), 0.01-wide bins, and separate
# linear or quadratic fits with 95% confidence bands on each side of the cutoff
plot_outcomes <- c(
  tot_lics_per = "All Active Licenses (per 1000)",
  tot_lics_on_per = "On-premise Licenses (per 1000)",
  tot_lics_off_per = "Off-premise Licenses (per 1000)",
  on_sale_all_per = "On-premise Sales (per 1000)",
  crashes_ALL_per = "All Crash Rate (per 1000)",
  crashes_DUI_per = "DUI Crash Rate (per 1000)",
  Fatalcrashes_ALL_per = "Fatal Crash Rate (per 1000)"
)
file_names <- c(tot_lics_per = "All_License", tot_lics_on_per = "OnPremise_License",
                tot_lics_off_per = "OffPremise_License", on_sale_all_per = "OnPremise_Sales",
                crashes_ALL_per = "All_Crashes", crashes_DUI_per = "DUI_Crashes",
                Fatalcrashes_ALL_per = "All_Fatal_Crashes")

for (v in names(plot_outcomes)) for (t in c(-1, 1)) for (deg in 1:2) {
  d <- (if (t == -1) pre else post) %>%
    drop_na(all_of(v), basepop) %>%
    group_by(vote_margin) %>%
    summarise(y = weighted.mean(.data[[v]], basepop), .groups = "drop") %>%
    mutate(pass = vote_margin > 0,
           bin = floor(round(vote_margin * 100, 8)) / 100,
           bin = if_else(vote_margin == 0, -0.01, bin) + 0.005,
           bin = case_when(bin == min(bin) ~ bin + 0.01,     # pull the outermost
                           bin == max(bin) ~ bin - 0.01,     # bins inward
                           TRUE ~ bin)) %>%
    group_by(bin) %>% mutate(y_bin = mean(y)) %>% ungroup()
  g <- ggplot(d, aes(vote_margin, y)) +
    geom_smooth(aes(group = pass), method = "lm", formula = y ~ poly(x, deg, raw = TRUE),
                colour = "blue", fill = "blue", alpha = 0.2, linewidth = 1.3) +
    geom_point(aes(bin, y_bin), colour = "grey50") +
    geom_vline(xintercept = 0, colour = "red", linetype = "dashed", linewidth = 1) +
    labs(x = "Vote Margin", y = plot_outcomes[[v]]) +
    theme_bw(base_size = 16) +
    theme(panel.grid.minor = element_blank())
  ggsave(sprintf("output/figures/rd/RegularRD_%s_%s%d_%s.png", file_names[[v]],
                 if (t == -1) "-" else "+", 1, if (deg == 1) "linear" else "quadratic"),
         g, width = 7, height = 5, dpi = 230)
}
