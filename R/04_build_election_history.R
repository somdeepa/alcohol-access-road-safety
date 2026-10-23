# 04_build_election_history.R
#
# City-year history of local option elections, stacked by election, for the
# stacked dynamic RD. Every city-level election 2003-2019 (dry or partly wet
# before the vote) defines a city-by-election unit ("cohort"); each unit
# carries the city's full election history over 2003-2023, so that earlier and
# later votes can be controlled for (Cellini, Ferreira and Rothstein 2010).
#
# Propositions on the same ballot are combined into one city-year election:
#   M_year = 1 if the city held an election, D_year = 1 if any proposition passed
#   V_year = vote share (mean over passing propositions if any passed,
#            otherwise over all propositions)
#   for_vote, against_vote = votes on the proposition with the most votes cast
#   onpremise, offpremise, beerwine, hardalcohol = ballot legalizes this type
#   D_year_<type> = a proposition of this type passed
#
# Input:   data/derived/elections.rds  (03_build_analysis_panel.R)
# Output:  data/derived/election_history.rds

library(tidyverse)

props <- read_rds("data/derived/elections.rds") %>%
  filter(election_level == 2, !str_detect(issueclean, "prohibit")) %>%
  mutate(
    V = for_vote / (for_vote + against_vote),
    passed = result == "passed",
    # What the proposition legalizes
    t_onpremise   = !str_detect(issueclean, "off-premise"),
    t_offpremise  = !str_detect(issueclean, "in restaurants"),
    t_beerwine    = !str_detect(issueclean, "^legal sale of mixed beverages"),
    t_hardalcohol = str_detect(issueclean, "including mixed|all alcoholic beverages for off|^legal sale of mixed beverages")
  )

history <- props %>%
  group_by(city, year) %>%
  summarise(
    M_year = 1,
    D_year = as.numeric(any(passed)),
    V_year = if (any(passed)) mean(V[passed]) else mean(V),
    for_vote     = for_vote[which.max(for_vote + against_vote)],
    against_vote = against_vote[which.max(for_vote + against_vote)],
    across(starts_with("t_"), ~ as.numeric(any(.x)), .names = "{str_remove(.col, 't_')}"),
    across(starts_with("t_"), ~ as.numeric(any(.x & passed)), .names = "D_year_{str_remove(.col, 't_')}"),
    .groups = "drop"
  ) %>%
  mutate(vote_margin_year = V_year - 0.5)

# Stack: one unit per city-level election, observed every year 2003-2023
units <- history %>% distinct(city, cohort = year)

election_history <- units %>%
  cross_join(tibble(year = 2003:2023)) %>%
  left_join(history, by = c("city", "year")) %>%
  mutate(across(M_year:vote_margin_year, ~ replace_na(.x, 0)),
         city_ordername = str_c(city, "_", cohort)) %>%
  arrange(city_ordername, year) %>%
  mutate(city_id = match(city_ordername, sort(unique(city_ordername), method = "radix"))) %>%
  select(year, city_id, city_ordername, city, cohort, onpremise, offpremise,
         beerwine, hardalcohol, vote_margin_year, for_vote, against_vote,
         V_year, M_year, D_year, starts_with("D_year_"))

cat("Election history:", n_distinct(election_history$city_ordername), "units in",
    n_distinct(election_history$city), "cities\n")
write_rds(election_history, "data/derived/election_history.rds")
