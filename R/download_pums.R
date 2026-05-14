# Download ACS PUMS 5-year (2018–2022) via tidycensus.
#
# WARNING: this call takes 1–3 hours via the Census API because
# tidycensus paginates per state and per row-limit chunk. Run interactively,
# not as part of any render. The cached output is saved to
#   data/scientist_demographics/census_pus/pums_raw.rds
# and read directly by data-prep/scientist_demographics.qmd.
#
# Prerequisite: a free Census API key.
#   Get one at https://api.census.gov/data/key_signup.html
#   Install once with: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#
# Alternative (if the API is too slow): download the per-state CSVs from
# https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/
# into data/scientist_demographics/census_pus/ and read/bind them manually.

library(here)
library(tidycensus)

if (nchar(Sys.getenv("CENSUS_API_KEY")) == 0) {
  stop("No Census API key found. Run: tidycensus::census_api_key('YOUR_KEY', install = TRUE)")
}

out_dir  <- here("data", "scientist_demographics", "census_pus")
out_file <- file.path(out_dir, "pums_raw.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pums_raw <- get_pums(
  variables = c("WAGP", "SEX", "RAC1P", "HISP", "SCHL", "AGEP", "OCCP", "PWGTP", "ESR"),
  state     = "all",
  year      = 2022,
  survey    = "acs5",
  show_call = FALSE
)

saveRDS(pums_raw, out_file)

cat("Saved:", out_file, "(",
    format(nrow(pums_raw), big.mark = ","), "rows,",
    round(file.size(out_file) / 1e6, 1), "MB )\n")
