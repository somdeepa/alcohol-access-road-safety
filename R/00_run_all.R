# 00_run_all.R
#
# Runs the full replication. Open alcohol-access-road-safety.Rproj (or set the
# working directory to the repository root), restore the package versions with
# renv::restore(), and source this file.
#
# NielsenIQ data (off-premise sales) are restricted and not included. Scripts
# that need them check for data/restricted/ and skip those parts if absent.

for (d in c("data/derived", "data/restricted", "output/tables", "output/figures")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

scripts <- c(
  "R/01_build_county_gdp.R",
  "R/02_build_license_panel.R",
  "R/03_build_analysis_panel.R",
  "R/04_build_election_history.R",
  "R/05_rd.R",
  "R/06_stacked_rd.R",
  "R/07_did.R",
  "R/08_offpremise_sales.R",   # requires NielsenIQ data in data/restricted/
  "R/09_spillovers.R",
  "R/10_descriptives.R",
  "R/11_write_tables.R"
)

for (s in scripts) {
  message("Running ", s)
  source(s, local = new.env())               # each script in its own environment
}
