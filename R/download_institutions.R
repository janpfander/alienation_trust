# Download IPEDS HD2022 (Header/Directory) from NCES.
#
# Run once. Saves the institution directory as
#   data/institutions/institutions_raw.rds
# so that data-prep/institutions.qmd can read it without re-downloading
# on every render. Contains coordinates and Carnegie classification for
# all ~6,000 US postsecondary institutions; we later filter to R1/R2 only.
#
# Source: https://nces.ed.gov/ipeds/datacenter/data/HD2022.zip
# Size:   ~3 MB zipped; runtime ~10 seconds on a fast connection.

library(here)
library(readr)

out_dir  <- here("data", "institutions")
out_file <- file.path(out_dir, "institutions_raw.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

zip_path <- tempfile(fileext = ".zip")
unzip_to <- tempfile()

download.file(
  url      = "https://nces.ed.gov/ipeds/datacenter/data/HD2022.zip",
  destfile = zip_path,
  quiet    = FALSE,
  mode     = "wb"
)

unzip(zip_path, exdir = unzip_to)

csv_in <- list.files(unzip_to, pattern = "(?i)^hd2022\\.csv$", full.names = TRUE)
if (length(csv_in) == 0) {
  csv_in <- list.files(unzip_to, pattern = "(?i)^hd2022", full.names = TRUE)[1]
}

institutions_raw <- read_csv(csv_in[1], show_col_types = FALSE)
saveRDS(institutions_raw, out_file)
unlink(c(zip_path, unzip_to), recursive = TRUE)

cat("Saved:", out_file, "(", nrow(institutions_raw), "rows )\n")
