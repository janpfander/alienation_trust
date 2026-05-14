# Download IPEDS Completions C2022_A from NCES.
#
# Run once. Saves the unzipped CSV to
#   data/scientist_demographics/ipeds/C2022_A.csv
# so that data-prep/scientist_demographics.qmd can read it without re-downloading
# on every render.
#
# Source: https://nces.ed.gov/ipeds/datacenter/data/C2022_A.zip
# Size:   ~10 MB zipped; runtime ~30 seconds on a fast connection.

library(here)
library(readr)

out_dir  <- here("data", "scientist_demographics", "ipeds")
out_file <- file.path(out_dir, "C2022_A.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

zip_path <- tempfile(fileext = ".zip")
unzip_to <- tempfile()

download.file(
  url      = "https://nces.ed.gov/ipeds/datacenter/data/C2022_A.zip",
  destfile = zip_path,
  quiet    = FALSE,
  mode     = "wb"
)

unzip(zip_path, exdir = unzip_to)

csv_in <- list.files(unzip_to, pattern = "(?i)^c2022_a\\.csv$", full.names = TRUE)
if (length(csv_in) == 0) {
  csv_in <- list.files(unzip_to, pattern = "(?i)^c2022_a", full.names = TRUE)[1]
}

file.copy(csv_in[1], out_file, overwrite = TRUE)
unlink(c(zip_path, unzip_to), recursive = TRUE)

cat("Saved:", out_file, "(", round(file.size(out_file) / 1e6, 1), "MB )\n")
