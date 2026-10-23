# 11_write_tables.R
#
# Writes every table in the paper as a LaTeX file (output/tables/tex/), in the
# layout used in the manuscript, from the CSV results of scripts 05-10.
#
# Conventions: RD cells are bias-corrected estimates with robust standard
# errors; stars * p<0.10, ** p<0.05, *** p<0.01; outcomes are shown with four
# decimals if their mean is below 10 and two otherwise (dollar amounts: two).

library(tidyverse)

out_dir <- "output/tables/tex"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
read <- function(f) read_csv(file.path("output/tables", f), show_col_types = FALSE)
has_sales <- file.exists("output/tables/offprem_rd_results.csv")   # NielsenIQ results

stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
num <- function(x, d) sprintf(paste0("%.", d, "f"), x)
comma <- function(x) formatC(x, format = "d", big.mark = ",")
decimals <- function(mean) if_else(abs(mean) >= 10, 2, 4)

rd_labels <- c(tot_lics_per = "All licenses", tot_lics_on_per = "On-premise licenses",
               tot_lics_off_per = "Off-premise licenses", on_sale_all_per = "On-premise sales",
               sales_pc = "Off-premise sales", crashes_ALL_per = "All crashes",
               crashes_DUI_per = "DUI crashes", crashes_NonDUI_per = "Non-DUI crashes",
               Fatalcrashes_ALL_per = "Fatal crashes", NonFatalcrashes_ALL_per = "Non-fatal crashes",
               Fatalcrashes_DUI_per = "DUI fatal crashes", NonFatalcrashes_DUI_per = "DUI non-fatal crashes")
first_stage <- c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per", "on_sale_all_per", "sales_pc")
second_stage <- c("crashes_ALL_per", "crashes_DUI_per", "crashes_NonDUI_per", "Fatalcrashes_ALL_per",
                  "NonFatalcrashes_ALL_per", "Fatalcrashes_DUI_per", "NonFatalcrashes_DUI_per")
dollars <- c("on_sale_all_per", "sales_pc")

# ---- Table 1: what local option elections legalize -------------------------------------------
t1 <- read("desc_election_types.csv")
cell <- function(g, k, v) t1[[v]][t1$group == g & t1$type == k]
types <- tibble(
  k = 1:6,
  lab = c(r"(Beer \& wine, off-premise only)", r"(Beer \& wine off-premise + mixed beverages in restaurants)",
          "All alcohol, off-premise", "Mixed beverages in restaurants only",
          "All alcohol including mixed beverages", "Other"),
  prac = c("Grocery and convenience stores sell beer and wine", "Above, plus restaurants serve liquor",
           "Package (liquor) stores", "Restaurants serve liquor; no retail sales",
           "Fully wet: stores, package stores, bars, restaurants", "E.g., beer and wine on- and off-premise"))
writeLines(c(
  r"(\begin{table}[!htbp])", r"(  \centering)", r"(  \caption{What Local Option Elections Legalize})",
  r"(  \label{tab:summary_elections})", r"(  \footnotesize)", r"(  \begin{threeparttable})",
  r"(    \begin{tabularx}{\textwidth}{@{}>{\raggedright\arraybackslash}X>{\raggedright\arraybackslash}Xcccc@{}})",
  r"(      \toprule)",
  r"(      & & \multicolumn{2}{c}{All city-level elections} & \multicolumn{2}{c}{Previously dry cities} \\)",
  r"(      \cmidrule(lr){3-4} \cmidrule(lr){5-6})",
  r"(      Ballot legalizes & In practice & Elections & Passed & Elections & Passed \\)", r"(      \midrule)",
  sprintf(r"(      %s & %s & %d & %d & %d & %d \\)", types$lab, types$prac,
          map_dbl(types$k, ~ cell("all", .x, "elections")), map_dbl(types$k, ~ cell("all", .x, "passed")),
          map_dbl(types$k, ~ cell("dry", .x, "elections")), map_dbl(types$k, ~ cell("dry", .x, "passed"))),
  r"(      \midrule)",
  sprintf(r"(      Total elections & & %d & %d & %d & %d \\)", cell("all", 0, "elections"), cell("all", 0, "passed"),
          cell("dry", 0, "elections"), cell("dry", 0, "passed")),
  sprintf(r"(      Cities & & \multicolumn{2}{c}{%d} & \multicolumn{2}{c}{%d} \\)", cell("all", 0, "cities"), cell("dry", 0, "cities")),
  r"(      \bottomrule)", r"(    \end{tabularx})", r"(    \begin{tablenotes}[flushleft])", r"(      \footnotesize)",
  paste0(r"(      \item \textit{Notes:} City-level local option elections, 2003--2019. Propositions a city places on the same ballot are combined into one election, coded as passed if any proposition passed. )",
         r"(\textit{Previously dry cities} are elections in cities that were completely dry before the vote. )",
         r"(Appendix Table~\ref{tab:estimation_samples} reports the elections each research design uses. )",
         r"(Rows are mutually exclusive and are assigned in the order: all alcohol including mixed beverages; )",
         r"(all alcohol off-premise; beer and wine off-premise with mixed beverages in restaurants; )",
         r"(beer and wine off-premise only; mixed beverages in restaurants only.)"),
  r"(    \end{tablenotes})", r"(  \end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "sum_of_elections.tex"))

# ---- Table 2: summary statistics -------------------------------------------------------------
ss <- read("desc_summary_statistics.csv") %>% arrange(match(sample, c("RD", "Stacked RD", "DiD")))
ss_labels <- c(
  tot_lics_per = "All licenses (per 1,000, annual)", tot_lics_on_per = "On-premise licenses (per 1,000, annual)",
  tot_lics_off_per = "Off-premise licenses (per 1,000, annual)",
  on_sale_all_per = "On-premise alcohol consumption (per capita, annual)",
  sales_pc = "Off-premise alcohol consumption (per capita, annual, county)",
  crashes_ALL_per = "All crashes (per 1,000, annual)", crashes_DUI_per = "DUI crashes (per 1,000, annual)",
  Fatalcrashes_ALL_per = "Fatal crashes (per 1,000, annual)", citypop = "City population",
  Protestant_pct = r"(Protestant (\% in county))", hispop_pct = r"(Hispanic (\% in county))",
  realGDPpc = "Real GDP per capita (thousands of chained 2017 USD, county)",
  road1 = "Centerline road mileage (per 1,000)", road2 = "Total daily vehicle miles traveled (per 1,000)",
  road3 = "Vehicles registered (per 1,000)",
  pol_ratio = r"(D/R vote ratio in presidential and gubernatorial elections (\% in county))",
  edu_hsonly = r"(Population with high school diploma only (\% in county))", aur = r"(Unemployment rate (\% in county))")
ss_row <- function(v) {
  m <- ss[[paste0(v, "_mean")]]; s <- ss[[paste0(v, "_sd")]]
  f <- if (v %in% c("citypop", "road2", "road3")) function(x) formatC(round(x), format = "d", big.mark = ",") else function(x) num(x, 2)
  cells <- if_else(is.na(m) | is.nan(m), "", paste0(f(m), " (", f(s), ")"))
  paste0(ss_labels[[v]], " & ", paste(cells, collapse = " & "), r"( \\)")
}
writeLines(c(
  r"(% --- Table --- (generated by 11_write_tables.R))", r"(\begin{table}[hbtp])", r"(\centering)",
  r"(\caption{Summary Statistics})", r"(\label{tab:summary_statistics})", r"(\footnotesize)", r"(\begin{threeparttable})",
  r"(\begin{tabular}{@{}>{\raggedright\arraybackslash}p{0.4\textwidth}ccc@{}})", r"(\toprule)",
  r"( & RD & Stacked RD & DiD \\)", r"( & (1) & (2) & (3) \\)", r"(\midrule)",
  paste0(r"(\textbf{Number of cities} & )", paste(comma(ss$cities), collapse = " & "), r"( \\)"),
  paste0(r"(\textbf{Observations} & )", paste(comma(ss$obs), collapse = " & "), r"( \\[0.3em])"), "",
  r"(\textbf{\textit{First Stage Outcomes}} \\[0.15em])",
  map_chr(c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per", "on_sale_all_per", "sales_pc"), ss_row),
  r"([0.3em])", "", r"(\textbf{\textit{Second Stage Outcomes}} \\[0.15em])",
  map_chr(c("crashes_ALL_per", "crashes_DUI_per", "Fatalcrashes_ALL_per"), ss_row),
  r"([0.3em])", "", r"(\textbf{\textit{City Characteristics}} \\[0.15em])",
  map_chr(c("citypop", "Protestant_pct", "hispop_pct", "realGDPpc", "road1", "road2", "road3",
            "pol_ratio", "edu_hsonly", "aur"), ss_row),
  r"(\bottomrule)", r"(\end{tabular})", "", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
  paste0(r"(\item \textit{Notes:} This table reports means with standard deviations in parentheses. )",
         r"(Column (1) covers city-years within five years of an election in the regression discontinuity sample; )",
         r"(column (2) covers city-year observations in the stacked regression discontinuity panel, where a city appears once for each election cohort it belongs to; )",
         r"(column (3) covers all city-years, 2003--2023, in the difference-in-differences sample, which adds cities that remained dry and held no election. )",
         r"(Appendix Table~\ref{tab:estimation_samples} describes how each sample is constructed. )",
         r"(Variables labeled (per 1,000) are measured per 1,000 population, and those labeled (\% in county) are county-level. )",
         r"(On-premise alcohol consumption is zero in city-years with no on-premise license. )",
         r"(Real GDP per capita is county real GDP from the Bureau of Economic Analysis divided by county population. )",
         r"(Off-premise alcohol consumption comes from the NielsenIQ Retail Scanner Data (2006--2020), which identify store locations only by three-digit ZIP code, )",
         r"(so sales are measured at the county level and each city is assigned its county's sales per capita. )",
         r"(Because NielsenIQ covers a sample of participating retailers, this measure understates total off-premise spending and is not comparable in level )",
         r"(to on-premise consumption, which covers all licensed bars and restaurants.)"),
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "summary_statistics.tex"))

# ---- RD balance ----------------------------------------------------------------------------
bal <- read("rd_balance.csv")
bal_labels <- c(lncitypop = "Log (Population)", aur = "Unemployment rate",
                realGDPpc = "GDP per capita (thousands of dollars)", edu_hsonly = "High-school only share",
                election_turnout = "Election turnout", Protestant_pct = "Protestant share",
                hispop_pct = "Hispanic share", pol_ratio = "Democrat share",
                vehicles_permillion = "Registered Vehicles per million",
                totalDVMT_permillion = "Daily Vehicle Miles Traveled per million")
bal_groups <- list("Socioeconomic Characteristics" = c("lncitypop", "aur", "realGDPpc", "edu_hsonly"),
                   "Electoral Characteristics" = c("election_turnout", "Protestant_pct", "hispop_pct", "pol_ratio"),
                   "Traffic Conditions" = c("vehicles_permillion", "totalDVMT_permillion"))
bal_lines <- imap(bal_groups, function(vars, g) {
  c(sprintf(r"(            \multicolumn{5}{l}{\textit{\textbf{%s}}} \\[3pt])", g),
    unlist(map(vars, function(v) {
      d <- bal %>% filter(covariate == v) %>% arrange(spec)
      c(sprintf(r"(            %s & %s \\)", bal_labels[[v]], paste0(num(d$b, 2), stars(d$pv), collapse = " & ")),
        sprintf(r"(             & %s \\)", paste0("(", num(d$se, 2), ")", collapse = " & ")))
    })))
})
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table})", r"(    \centering)", r"(    \begin{threeparttable})",
  r"(        \caption{Pre-treatment Covariate Balance Tests})", r"(        \begin{tabular}{lcccc})", r"(            \toprule)",
  r"(            & \multicolumn{2}{c}{Full Bandwidth} & \multicolumn{2}{c}{Optimal Bandwidth} \\)",
  r"(            \cmidrule(lr){2-3} \cmidrule(lr){4-5})", r"(            & Linear & Quadratic & Linear & Quadratic \\)",
  r"(            \midrule)",
  bal_lines[[1]], "", bal_lines[[2]], "", bal_lines[[3]],
  r"(            \bottomrule)",
  r"(            \multicolumn{5}{p{17cm}}{\footnotesize Notes: * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Standard errors are in parentheses. The table reports estimates for covariate balance tests using equation \ref{eq:rd}. Coefficients are bias-corrected estimates with robust standard errors \citep{calonico_robust_2014a}. We use both data-driven (optimal) and fixed (full) bandwidths with triangular kernels.} \\)",
  r"(        \end{tabular})", r"(        \label{tab:rdbalancetest})", r"(    \end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "RDbalance.tex"))

# ---- RD tables (levels and changes) -----------------------------------------------------------------
rd <- read("rd_results.csv")
rd_changes <- read("rd_changes.csv")
if (has_sales) {
  rd <- bind_rows(rd, read("offprem_rd_results.csv"))
  rd_changes <- bind_rows(rd_changes, read("offprem_rd_changes.csv"))
}
rd_means <- rd %>% distinct(outcome, pre_mean) %>%
  mutate(d = if_else(outcome %in% dollars, 2, decimals(pre_mean)))
rd_rows <- function(res, y, with_mean, last) {
  r <- res %>% filter(outcome == y) %>% arrange(spec)
  if (nrow(r) == 0) return(NULL)
  m <- rd_means %>% filter(outcome == y)
  b <- paste0(num(r$b, m$d), stars(r$pv)); s <- paste0("(", num(r$se, m$d), ")")
  c(paste0(rd_labels[[y]], if (with_mean) paste0(" & ", num(m$pre_mean, m$d)), " & ", paste(b, collapse = " & "), r"( \\)"),
    paste0(if (with_mean) "& " else "", "& ", paste(s, collapse = " & "), r"( \\)", if (last) "[1em]" else "[0.5em]"))
}
rd_body <- function(res, with_mean) {
  fs <- intersect(first_stage, res$outcome); ss2 <- second_stage
  c(r"(\textbf{\textit{First-stage outcomes}} \\[0.5em])",
    unlist(map(fs, ~ rd_rows(res, .x, with_mean, .x == last(fs)))), "",
    r"(\textbf{\textit{Second-stage outcomes}} \\[0.5em])",
    unlist(map(ss2, ~ rd_rows(res, .x, with_mean, .x == last(ss2)))), "")
}
spec_rows <- function(with_mean) {
  lead <- if (with_mean) " & " else " "
  c(paste0("Bandwidth &", lead, "Global & Global & Global & Global & Optimal & Optimal & Optimal & Optimal ", r"(\\)"),
    paste0("Polynomial Order &", lead, "Linear & Quadratic & Linear & Quadratic & Linear & Quadratic & Linear & Quadratic ", r"(\\)"),
    paste0("Covariates &", lead, paste(rep(r"(\checkmark)", 8), collapse = " & "), r"( \\)"),
    paste0("Population weights &", lead, r"(X & X & \checkmark & \checkmark & X & X & \checkmark & \checkmark \\)"))
}
sales_note <- r"(Off-premise sales are measured at the county level using county-level elections; for this outcome, columns (5)--(8) use a fixed bandwidth of 0.2. Off-premise sales are from a sample of stores aggregated to the county and understate changes in retail sales (Section~\ref{sec:data}).)"
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table}[!htbp])", r"(\centering)", r"(\caption{Regression Discontinuity Results})",
  r"(\label{tab:mainresultrd})", r"(\resizebox{\textwidth}{!}{)", r"(\begin{tabular}{@{}lccccccccc@{}})", r"(\toprule)",
  r"(& Mean & (1) & (2) & (3) & (4) & (5) &  (6) &  (7) &  (8) \\)", r"(\midrule)",
  rd_body(rd, TRUE), spec_rows(TRUE),
  r"(\bottomrule)", r"(\end{tabular})", "}", r"(\par\vspace{0.5em})", r"(\begin{minipage}{\textwidth})",
  paste0(r"({\footnotesize Notes: * p$<$0.10, ** p$<$0.05, *** p$<$0.01. The coefficients are obtained from separate regressions of equation \ref{eq:rd}, each using a different outcome measured per 1,000 population one year after the election, using each city's first election. )",
         r"(Coefficients are bias-corrected local polynomial estimates; standard errors (in parentheses) and significance are from the corresponding robust bias-corrected inference \citep{calonico_robust_2014a}, clustered at the city level. )",
         r"(Global bandwidths use the full range of the vote margin; optimal bandwidths are data-driven. )", sales_note, " ",
         r"(Licensing and on-premise sales data are obtained from the Texas Alcoholic Beverage Commission, off-premise sales data are from NielsenIQ, and crash data are from the Texas Department of Transportation. )",
         r"(The covariates are the city characteristics in Table \ref{tab:summary_statistics}. The Mean column reports the pre-period mean of the dependent variable, computed as the average of each outcome measured one year before the election.} )"),
  r"(\end{minipage})", r"(\end{table})"),
  file.path(out_dir, "main_result_RD.tex"))

n_change_cities <- read_rds("data/derived/rd_panel.rds") %>%
  filter(event_time %in% c(-1, 1)) %>% group_by(city) %>% filter(elec_year == min(elec_year)) %>%
  group_by(city_elecyear) %>% filter(n() == 2) %>% ungroup() %>% distinct(city) %>% nrow()
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table}[!htbp])", r"(\centering)",
  r"(\caption{Regression Discontinuity Estimates on Changes in Outcomes})", r"(\label{tab:rd_differenced})",
  r"(\resizebox{\textwidth}{!}{)", r"(\begin{tabular}{@{}lcccccccc@{}})", r"(\toprule)",
  r"(& (1) & (2) & (3) & (4) & (5) &  (6) &  (7) &  (8) \\)", r"(\midrule)",
  rd_body(rd_changes, FALSE), spec_rows(FALSE),
  r"(\bottomrule)", r"(\end{tabular})", "}", r"(\par\vspace{0.5em})", r"(\begin{minipage}{\textwidth})",
  paste0(r"({\footnotesize Notes: * p$<$0.10, ** p$<$0.05, *** p$<$0.01. The specifications are those of Table \ref{tab:mainresultrd}, but the dependent variable is the change in each outcome from one year before to one year after each city's first election in the study period; the sample is restricted to cities observed in both years ()",
         n_change_cities, r"( cities). )",
         r"(Covariates are measured one year before the election. Coefficients are bias-corrected local polynomial estimates; standard errors (in parentheses) and significance are from the corresponding robust bias-corrected inference \citep{calonico_robust_2014a}, clustered at the city level. )",
         sales_note, "}"),
  r"(\end{minipage})", r"(\end{table})"),
  file.path(out_dir, "rd_differenced.tex"))

# ---- Difference-in-differences tables -----------------------------------------------------------------
did <- read("did_results.csv")
if (has_sales) did <- bind_rows(did, read("offprem_did_results.csv"))
did <- did %>% mutate(d = if_else(outcome %in% dollars, 2, decimals(pre_mean)),
                      b_txt = paste0(num(b, d), stars(p)), se_txt = paste0("(", num(se, d), ")"),
                      mean_txt = num(pre_mean, d), N_txt = comma(N))
did_labels <- c(tot_lics_per = "All Licenses", tot_lics_on_per = "On-Premise Licenses",
                tot_lics_off_per = "Off-Premise Licenses", on_sale_all_per = "On-Premise Sales",
                sales_pc = "Off-Premise Sales", crashes_ALL_per = "All Crashes", crashes_DUI_per = "DUI Crashes",
                crashes_NonDUI_per = "Non DUI Crashes")
did_get <- function(y, m, v) did[[v]][did$outcome == y & did$method == m]
did_first <- intersect(c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per", "on_sale_all_per", "sales_pc"), did$outcome)
did_second <- c("crashes_ALL_per", "crashes_DUI_per", "crashes_NonDUI_per")
did_note <- r"(The specifications for city-level outcomes include the covariates in Table \ref{tab:summary_statistics} and are weighted by 2003 city population; the off-premise sales specification is estimated on the county panel, weighted by 2006 county population, without covariates. Off-premise sales are from a sample of stores aggregated to the county and understate changes in retail sales (Section~\ref{sec:data}).)"
main_did_row <- function(y, sp) c(
  sprintf(r"(\textbf{%s} & %s & %s & %s \\[2pt])", did_labels[[y]], did_get(y, "CSDID", "b_txt"),
          did_get(y, "CSDID", "mean_txt"), did_get(y, "CSDID", "N_txt")),
  sprintf(r"(& %s & & \\%s)", did_get(y, "CSDID", "se_txt"), sp), "")
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table})", r"(\centering)", r"(\captionsetup{skip=3pt})",
  r"(\caption{Difference-in-Differences Estimates})", r"(\label{tab:didmainresult})", r"(\begin{threeparttable})",
  r"(\begin{tabularx}{0.85\textwidth}{Xccc})", r"(\toprule)", r"(& Estimate & Pre-period mean & N \\)", r"(\midrule)", "",
  r"(\multicolumn{4}{l}{\textit{\textbf{First-stage outcomes}}} \\[4pt])",
  unlist(map(did_first, ~ main_did_row(.x, "[6pt]"))),
  r"(\multicolumn{4}{l}{\textit{\textbf{Second-stage outcomes}}} \\[4pt])",
  unlist(map(did_second, ~ main_did_row(.x, if (.x == "crashes_NonDUI_per") "[4pt]" else "[6pt]"))),
  r"(\bottomrule)", r"(\end{tabularx})", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
  r"(\item \textit{Notes:} * p$<$0.10, ** p$<$0.05, *** p$<$0.01.)",
  r"(The coefficients are the average of the event-study estimates for years 0--5 after treatment from the)",
  r"(\citet{callaway2021difference} (CSDID) estimator, each from a separate regression)",
  r"(using a different outcome measured per 1,000 population.)",
  paste(did_note, r"(Two-way fixed effects (TWFE) estimates for comparison)"),
  r"(are reported in Appendix Table~\ref{tab:did_twfe_csdid}.)",
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "main_result_DID.tex"))

both_row <- function(y) c(
  sprintf(r"(\textbf{%s} & %s & %s \\[2pt])", did_labels[[y]], did_get(y, "TWFE", "b_txt"), did_get(y, "CSDID", "b_txt")),
  sprintf(r"(& %s & %s \\[2pt])", did_get(y, "TWFE", "se_txt"), did_get(y, "CSDID", "se_txt")),
  sprintf(r"(\multicolumn{3}{l}{\quad\small Pre-period mean: %s \qquad N: %s} \\[6pt])",
          did_get(y, "CSDID", "mean_txt"), did_get(y, "CSDID", "N_txt")), "")
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table})", r"(\centering)", r"(\captionsetup{skip=3pt})",
  r"(\caption{Difference-in-Differences Estimates: TWFE and CSDID})", r"(\label{tab:did_twfe_csdid})",
  r"(\begin{threeparttable})", r"(\begin{tabularx}{0.8\textwidth}{Xcc})", r"(\toprule)",
  r"(& \multicolumn{2}{c}{Estimation Approaches} \\)", r"(\cmidrule(lr){2-3})", r"(& TWFE & CSDID \\)", r"(\midrule)", "",
  r"(\multicolumn{3}{l}{\textit{\textbf{First-stage outcomes}}} \\[4pt])", unlist(map(did_first, both_row)),
  r"(\midrule)", r"(\multicolumn{3}{l}{\textit{\textbf{Second-stage outcomes}}} \\[4pt])", unlist(map(did_second, both_row)),
  r"(\bottomrule)", r"(\end{tabularx})", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
  paste0(r"(\item \textit{Notes:} * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Standard errors in parentheses, clustered at the city (county) level. )",
         r"(TWFE estimates are from equation \ref{eq:twfe}; CSDID estimates are the average of the event-study estimates for years 0--5 after treatment from the \citet{callaway2021difference} estimator. )",
         did_note, r"( Licensing and on-premise sales data are from the Texas Alcoholic Beverage Commission, off-premise sales data are from NielsenIQ, and crash data are from the Texas Department of Transportation.)"),
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "DID_twfe_csdid.tex"))

# ---- Stacked RD tables -----------------------------------------------------------------------------------
drd <- read("stacked_rd_results.csv")
if (has_sales) drd <- bind_rows(drd, read("offprem_stacked_rd_results.csv") %>% filter(outcome == "sales_pc"))
drd <- drd %>% mutate(d = if_else(outcome %in% dollars, 2, decimals(premean)))
drd_common <- r"(* p$<$0.10, ** p$<$0.05, *** p$<$0.01. This table reports stacked RD estimates and standard errors of linear combinations of $\beta_k$ from equation \ref{eq:stacked_rd}: the average effect in years 0--2 and 3--5 after the election. Observations are weighted by the city population at the start of the analysis period. All specifications include city--cohort and year--cohort fixed effects)"
drd_table <- function(file, caption, label, vars, heads, note) {
  r <- drd %>% filter(outcome %in% vars) %>% arrange(match(outcome, vars))
  vars <- r$outcome; k <- length(vars)
  row <- function(lbl, b, s, p) c(paste0(r"(\quad )", lbl, " & ", paste0(num(b, r$d), stars(p), collapse = " & "), r"( \\)"),
                                  paste0(r"(\quad & )", paste0("(", num(s, r$d), ")", collapse = " & "), r"( \\)"))
  writeLines(c(
    r"(% (generated by 11_write_tables.R))", r"(\begin{table}[!htbp])", r"(\centering)", sprintf(r"(\caption{%s})", caption),
    sprintf(r"(\label{%s})", label), r"(\begin{threeparttable})", sprintf(r"(\begin{tabular}{l%s})", strrep("c", k)), r"(\toprule)",
    paste0(" & ", paste(heads[seq_len(k)], collapse = " & "), r"( \\)"), paste0(" & ", paste0("(", seq_len(k), ")", collapse = " & "), r"( \\)"),
    r"(\midrule)", r"(\textbf{Average Effect (years after election):} \\)",
    row("0--2 years", r$b02, r$se02, r$p02), row("3--5 years", r$b35, r$se35, r$p35),
    r"(\\[-0.8em])",
    paste0("City--cohort fixed effects", strrep(" & X", k), r"( \\)"),
    paste0("Year--cohort fixed effects", strrep(" & X", k), r"( \\)"),
    paste0("Pre-period mean & ", paste(num(r$premean, r$d), collapse = " & "), r"( \\)"),
    paste0(r"(Adj.\ \(R^2\) & )", paste(num(r$r2a, 4), collapse = " & "), r"( \\)"),
    paste0("N & ", paste(comma(r$N), collapse = " & "), r"( \\)"),
    r"(\bottomrule)", r"(\end{tabular})", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
    paste0(r"(\item \textit{Notes:} )", note), r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
    file.path(out_dir, file))
}
drd_table("DRD_Establishments.tex", "Stacked RD Estimates of Election Passage on Alcohol Establishments",
          "tab:stackeddrd_licenses", c("tot_lics_per", "tot_lics_on_per", "tot_lics_off_per"),
          c("All", "On-Premise", "Off-Premise"),
          paste0(drd_common, r"( and the covariates in Table \ref{tab:summary_statistics}. The dependent variables are: (1) all alcohol establishments, (2) on-premise establishments, and (3) off-premise establishments, each per 1,000 population. Standard errors, shown in parentheses, are clustered at the city level. Licensing data are from the Texas Alcoholic Beverage Commission.)"))
drd_table("DRD_Traffic_Accidents.tex", "Stacked RD Estimates of Election Passage on Traffic Accidents",
          "tab:stackeddrd_accidents", c("crashes_ALL_per", "crashes_DUI_per", "Fatalcrashes_ALL_per"),
          c("All~Crashes", "DUI~Crashes", "Fatal~Crashes"),
          paste0(drd_common, r"( and the covariates in Table \ref{tab:summary_statistics}. The dependent variables are: (1) total traffic accidents, (2) DUI-related crashes, and (3) fatal crashes, each per 1,000 population. Standard errors, shown in parentheses, are clustered at the city level. Crash data are from the Texas Department of Transportation.)"))
drd_table("DRD_Consumption.tex", "Stacked RD Estimates of Election Passage on Alcohol Sales",
          "tab:stackeddrd_consumption", c("on_sale_all_per", "sales_pc"), c("On-Premise", "Off-Premise"),
          paste0(drd_common, r"(. The on-premise specification (city level) also includes the covariates in Table \ref{tab:summary_statistics}; the off-premise specification is estimated on the county panel using county-level elections, weighted by 2006 county population, without covariates. The dependent variables are (1) on-premise sales per capita and (2) off-premise sales per capita, in 2023 dollars. Standard errors, shown in parentheses, are clustered at the city (county) level. On-premise sales data are from the Texas Alcoholic Beverage Commission; off-premise sales data are from NielsenIQ. Off-premise sales are from a sample of stores aggregated to the county and understate changes in retail sales (Section~\ref{sec:data}).)"))

# ---- License-crash associations (appendix) -------------------------------------------------------------------
lc <- read("desc_license_crash.csv")
lc_cell <- function(y, tm, v) lc[[v]][lc$outcome == y & lc$term == tm]
lc_ys <- c("crashes_ALL_per", "crashes_DUI_per", "crashes_NonDUI_per")
lc_row <- function(tm, lbl) c(
  sprintf(r"(        %-20s & %s \\)", lbl, paste0(map_chr(lc_ys, ~ num(lc_cell(.x, tm, "b"), 4)),
                                                  map_chr(lc_ys, ~ stars(lc_cell(.x, tm, "p"))), collapse = " & ")),
  sprintf(r"(                             & %s \\[3pt])", paste0("(", map_chr(lc_ys, ~ num(lc_cell(.x, tm, "se"), 4)), ")", collapse = " & ")))
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table}[!htbp])", r"(    \centering)", r"(    \caption{Alcohol Licenses and Crash Rates})",
  r"(    \label{tab:twfe_mechanism})", r"(    \begin{threeparttable})", r"(    \begin{tabular}{lccc})", r"(        \toprule)",
  r"(        & \multicolumn{3}{c}{Crash Rate} \\)", r"(        \cmidrule(lr){2-4})", r"(        & All & DUI & Non-DUI \\)", r"(        \midrule)",
  lc_row("on", "On-premise licenses"), lc_row("off", "Off-premise licenses"),
  r"(        \midrule)", r"(        City FE                         & X & X & X \\)", r"(        Year FE                         & X & X & X \\)",
  paste0(r"(        Observations                    & )", paste(map_chr(lc_ys, ~ comma(lc_cell(.x, "on", "N"))), collapse = " & "), r"( \\)"),
  paste0(r"(        $R^2$                           & )", paste(map_chr(lc_ys, ~ num(lc_cell(.x, "on", "r2"), 4)), collapse = " & "), r"( \\)"),
  r"(        \bottomrule)", r"(    \end{tabular})", r"(    \begin{tablenotes}[flushleft])", r"(        \footnotesize)",
  r"(        \item \textit{Notes:} * p$<$0.10, ** p$<$0.05, *** p$<$0.01. Each column reports estimates from equation \ref{eq:twfe_mechanism}, estimated on the city-year panel of the regression discontinuity sample. Standard errors, clustered at the city level, are in parentheses.)",
  r"(    \end{tablenotes})", r"(    \end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "mechanism.tex"))

# ---- Estimation samples (appendix) -----------------------------------------------------------------------------
es <- read("desc_estimation_samples.csv")
es_row <- function(dsg, text, end) {
  r <- es %>% filter(design == dsg)
  sprintf(r"(%s & %s & %s & %s & %s & %s & %s \\%s)", dsg, comma(r$elections), comma(r$cities), comma(r$treated),
          comma(r$comparison), comma(r$obs), text, end)
}
writeLines(c(
  r"(% (generated by 11_write_tables.R))", r"(\begin{table}[!htbp])", r"(\centering)", r"(\caption{Estimation Samples by Research Design})",
  r"(\label{tab:estimation_samples})", r"(\footnotesize)", r"(\begin{threeparttable})",
  r"(\begin{tabularx}{\textwidth}{@{}lccccc>{\raggedright\arraybackslash}X@{}})", r"(\toprule)",
  r"(Design & Elections & Cities & Treated & Comparison & Observations & Elections included \\)", r"(\midrule)",
  es_row("RD", "First election in the study period in each city that was completely dry before the vote", "[3pt]"),
  es_row("Stacked RD", "All city-level elections, including repeat elections and elections in partially wet cities", "[3pt]"),
  es_row("DiD", "Cities completely dry before the study period, treated at their first successful election, plus dry cities with no election", ""),
  r"(\bottomrule)", r"(\end{tabularx})", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
  paste0(r"(\item \textit{Notes:} Elections are city-level local option elections, 2003--2019, with propositions on the same ballot combined (Table~\ref{tab:summary_elections}). )",
         r"(RD: one observation per city, measured one year after its first election in the study period; treated elections passed and comparison elections failed. )",
         r"(Stacked RD: a unit is a city in one election cohort, and observations are city-year-cohort cells within five years of the election; comparison units failed in the cohort year and did not pass an election in the following five years, and the election-history terms in equation~\ref{eq:stacked_rd} absorb the effects of earlier and later votes. )",
         sprintf(r"(DiD: observations are city-years, 2003--2023; treated cities switch from dry to wet, and comparison cities are the %d cities whose elections all failed and the %d cities that remained dry and held no election. )",
                 es$failed[1], es$no_election[1]),
         r"(All samples exclude four cities with irregular license records (DISH, Jolly, Coffee City, and Emhouse).)"),
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "estimation_samples.tex"))

# ---- Spillover tables -------------------------------------------------------------------------------------------
sp <- read("spillover_results.csv") %>%
  mutate(txt_b = paste0(num(b, 3), stars(p)), txt_se = paste0("(", num(se, 3), ")"))
sp_cell <- function(y, rg, ef, v) sp[[v]][sp$outcome == y & sp$ring == rg & sp$effect == ef]
main_ys <- c("all_licenses_rate", "off_licenses_rate", "on_licenses_rate",
             "total_crashes_rate", "dui_crashes_rate", "fatal_crashes_rate")
sp_main_row <- function(ef, lbl) c(
  paste0(lbl, " & ", paste(map_chr(main_ys, ~ sp_cell(.x, "0_20", ef, "txt_b")), collapse = " & "), r"( \\)"),
  paste0(" & ", paste(map_chr(main_ys, ~ sp_cell(.x, "0_20", ef, "txt_se")), collapse = " & "), r"( \\)"))
writeLines(c(
  r"(\begin{table}[!htbp])", r"(\centering)", r"(\caption{Spillover Estimates with 20-Mile Exposure})", r"(\label{tab:spillover_main})",
  r"(\begin{threeparttable})", r"(\begin{tabular}{lcccccc})", r"(\toprule)",
  r"( & \multicolumn{3}{c}{Licenses} & \multicolumn{3}{c}{Crashes} \\)", r"(\cmidrule(lr){2-4}\cmidrule(lr){5-7})",
  r"( & All & Off & On & All & DUI & Fatal \\)", r"(\midrule)",
  sp_main_row("dry", "Spillovers to dry cities"), sp_main_row("wet", "Spillovers to wet cities"),
  r"(\\[-0.8em])",
  paste0("Mean of DV & ", paste(map_chr(main_ys, ~ num(sp_cell(.x, "0_20", "dry", "mean_dv"), 3)), collapse = " & "), r"( \\)"),
  paste0("N & ", paste(map_chr(main_ys, ~ comma(sp_cell(.x, "0_20", "dry", "N"))), collapse = " & "), r"( \\)"),
  r"(\bottomrule)", r"(\end{tabular})", r"(\begin{tablenotes}[flushleft])", r"(\footnotesize)",
  r"(\item \textit{Notes:} This table reports the coefficients from the spillover regression (Equation \ref{eq:spillover}) with own wet status, exposure to  wet neighbors, and their interaction. The first coefficient row reports the spillover effect of having wet neighbors for dry cities only, and the second row reports the total spillover effect on wet cities as a linear combination of the exposure and interaction coefficients. Exposure is the share of neighboring cities within 20 miles that are wet in a given year. All outcomes are measured per 1,000 population. The sample includes all cities that are already wet before 2003, and cities that change status during the study period. All columns include city and year fixed effects, and standard errors clustered at the city level are shown in parentheses.)",
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "spillover_main.tex"))

app_groups <- list(Licenses = c(all_licenses_rate = "All licenses", off_licenses_rate = "Off-premise licenses",
                                on_licenses_rate = "On-premise licenses"),
                   Sales = c(onprem_sales_total_pc = "On-premise sales", offprem_sales_total_pc = "Off-premise sales (county)"),
                   Crashes = c(total_crashes_rate = "All crashes", dui_crashes_rate = "DUI crashes",
                               fatal_crashes_rate = "Fatal crashes"))
rings_app <- c("0_10", "10_20", "20_50")
app_panel <- function(ef) unlist(imap(app_groups, function(ys, g) {
  ys <- ys[names(ys) %in% sp$outcome]
  c(sprintf(r"(\textbf{%s} &  &  &  \\)", g),
    unlist(imap(ys, function(lbl, y) c(
      paste0(lbl, " & ", paste(map_chr(rings_app, ~ sp_cell(y, .x, ef, "txt_b")), collapse = " & "), r"( \\)"),
      paste0(" & ", paste(map_chr(rings_app, ~ sp_cell(y, .x, ef, "txt_se")), collapse = " & "), r"( \\)",
             if (y == last(names(ys)) && g != "Crashes") "[0.1em]" else "")))))
}))
writeLines(c(
  r"(\begin{table}[htbp])", r"(\centering)", r"(\footnotesize)", r"(\caption{Spillover Estimates by Distance Bin})",
  r"(\label{tab:spillover_appendix})", r"(\begin{threeparttable})", r"(\setlength{\extrarowheight}{0pt})",
  r"(\begin{tabular}{lccc})", r"(\toprule)", r"( & 0--10 miles & 10--20 miles & 20--50 miles \\)", r"(\midrule)",
  r"(\multicolumn{4}{l}{\textit{Panel A. Spillovers to dry cities}} \\[0.1em])", app_panel("dry"),
  r"([0.3em])", r"(\multicolumn{4}{l}{\textit{Panel B. Spillovers to wet cities}} \\[0.1em])", app_panel("wet"),
  r"(\bottomrule)", r"(\end{tabular})", r"(\begin{tablenotes}[flushleft])", r"(\scriptsize)",
  r"(\item \textit{Notes:} Each cell comes from a separate regression (Equation \ref{eq:spillover}) including own wet status, one bin-specific spillover exposure, and their interaction. Panel A reports the spillover coefficient for dry cities; Panel B reports the linear combination of exposure and interaction coefficients for wet cities. Exposure is the share of neighboring cities in the indicated distance bin that are wet. License and crash outcomes are per 1,000 population; sales are per-capita dollars. Off-premise sales are county-level, all other outcomes are city-level. Sample excludes always-dry cities with no elections. All regressions include city/county and year fixed effects; standard errors clustered at city or county level.)",
  r"(\end{tablenotes})", r"(\end{threeparttable})", r"(\end{table})"),
  file.path(out_dir, "spillover_appendix.tex"))

message("Wrote ", length(list.files(out_dir, pattern = "\\.tex$")), " tables to ", out_dir)
