# =============================================================================
# build_opioid_test_data.R
#
# Fetches REAL public data to empirically test the opioid-crisis DAGs built
# from the 4 real papers (see opioid_papers/ and CAUDA_Opioid_Worked_Example_REAL.docx).
# This is the data half of Cox's requested workflow step 5: "where appropriate
# data are available, test [testable] implications and compare how well the
# competing theories survive empirical scrutiny."
#
# Sources (all real, public, no scraping/ToS violation):
#   1. CDC VSRR Provisional Drug Overdose Death Counts (data.cdc.gov, Socrata,
#      resource xkb8-kh2a) - state x year counts, 2015-present, broken out by
#      drug category. No API key needed.
#   2. DEA ARCOS opioid distribution data via the Washington Post's public API
#      (arcos-api.ext.nile.works, key="WaPo", literally documented as public) -
#      state/county-level oxycodone+hydrocodone pill shipments, 2006-2014.
#
# Output: inst/extdata/opioid_real_data.csv - a clean state-year panel, bundled
# with the package so the Shiny app can load it via system.file().
#
# Run this interactively/manually when refreshing the data; it is NOT run as
# part of the package build (network calls don't belong in R CMD check).
# =============================================================================

library(jsonlite)
library(dplyr)
library(tidyr)

cat("=== Step 1: CDC VSRR overdose death counts (2015-2024, state x indicator) ===\n")

indicators <- c(
  "Number of Drug Overdose Deaths",
  "Heroin (T40.1)",
  "Natural & semi-synthetic opioids (T40.2)",
  "Synthetic opioids, excl. methadone (T40.4)"
)

fetch_indicator <- function(ind) {
  url <- paste0(
    "https://data.cdc.gov/resource/xkb8-kh2a.json",
    "?month=December&period=12%20month-ending",
    "&indicator=", URLencode(ind, reserved = TRUE),
    "&$select=state,state_name,year,data_value",
    "&$limit=1000"
  )
  df <- tryCatch(fromJSON(url), error = function(e) { message("Failed: ", ind, " - ", e$message); NULL })
  if (!is.null(df) && nrow(df) > 0) df$indicator <- ind
  df
}

vsrr_list <- lapply(indicators, fetch_indicator)
vsrr_raw  <- bind_rows(vsrr_list)
cat("  Fetched", nrow(vsrr_raw), "rows across", length(indicators), "indicators\n")

# Reshape wide: one row per state-year, one column per indicator
vsrr_raw$data_value <- suppressWarnings(as.numeric(vsrr_raw$data_value))
vsrr_raw$year        <- as.integer(vsrr_raw$year)

vsrr_wide <- vsrr_raw %>%
  filter(!is.na(data_value), !state %in% c("US", "YC")) %>%   # drop national total + NYC-as-separate-row
  select(state, state_name, year, indicator, data_value) %>%
  pivot_wider(names_from = indicator, values_from = data_value) %>%
  rename(
    overdose_deaths_total   = `Number of Drug Overdose Deaths`,
    heroin_deaths           = `Heroin (T40.1)`,
    natural_semisynth_deaths = `Natural & semi-synthetic opioids (T40.2)`,
    synthetic_opioid_deaths = `Synthetic opioids, excl. methadone (T40.4)`
  )

cat("  Reshaped to", nrow(vsrr_wide), "state-year rows\n\n")

cat("=== Step 2: ARCOS opioid pill distribution (2006-2014 total, by state) ===\n")
cat("  NOTE: the live arcos-api.ext.nile.works plumber server is currently\n")
cat("  unreachable (verified down 2026-08-01), so we hit the underlying static\n")
cat("  S3 summary files it wraps directly instead - same real WaPo/DEA ARCOS\n")
cat("  data, just via its storage layer rather than the (defunct) query API.\n")
cat("  These are per-pharmacy totals summed across all of 2006-2014 (not\n")
cat("  broken out by year), so this becomes a single cross-sectional exposure\n")
cat("  figure per state rather than a year-by-year series.\n\n")

state_abbrs <- unique(vsrr_wide$state)
state_abbrs <- setdiff(state_abbrs, c("PR"))  # not in ARCOS state list

fetch_arcos_state_s3 <- function(st) {
  url <- paste0("https://wp-stat.s3.amazonaws.com/dea-pain-pill-database/summary/arcos-",
                tolower(st), "-county-pharmacy.tsv")
  df <- tryCatch(read.delim(url, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.data.frame(df) && nrow(df) > 0) {
    df$state <- st
    return(df)
  }
  NULL
}

cat("  Fetching per-state S3 summary files...\n")
arcos_list <- lapply(state_abbrs, function(st) {
  d <- fetch_arcos_state_s3(st)
  cat("   ", st, if (is.null(d)) "failed" else paste0(nrow(d), " pharmacies"), "\n")
  d
})
arcos_raw <- bind_rows(arcos_list)
cat("  Total ARCOS rows:", nrow(arcos_raw), " | Columns:", paste(names(arcos_raw), collapse=", "), "\n\n")

saveRDS(arcos_raw, "data-raw/arcos_raw_cache.rds")

dosage_col <- intersect(c("total_dosage_unit", "TOTAL_DOSAGE_UNIT"), names(arcos_raw))[1]

if (is.na(dosage_col) || nrow(arcos_raw) == 0) {
  cat("  !! Could not auto-detect dosage column - inspect data-raw/arcos_raw_cache.rds manually.\n")
  final <- vsrr_wide
} else {
  arcos_state_totals <- arcos_raw %>%
    group_by(state) %>%
    summarise(arcos_total_pills_2006_2014 = sum(as.numeric(.data[[dosage_col]]), na.rm = TRUE), .groups = "drop")

  cat("  Aggregated to", nrow(arcos_state_totals), "state totals\n\n")

  cat("=== Step 3: Merge + save final panel ===\n")
  final <- vsrr_wide %>%
    left_join(arcos_state_totals, by = "state") %>%
    arrange(state, year)
}

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
write.csv(final, "inst/extdata/opioid_real_data.csv", row.names = FALSE)
cat("  Saved inst/extdata/opioid_real_data.csv:", nrow(final), "rows,", ncol(final), "columns\n")
print(head(final, 10))
