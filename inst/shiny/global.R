# global.R — runs once when the app starts (both locally and on shinyapps.io)
# Ensures cauda package is available; installs from GitHub if missing.

# NOTE: the authoritative options(shiny.maxRequestSize=...) call now lives
# at the top of app.R, not here -- a live diagnostic proved this option set
# in global.R was never actually taking effect on shinyapps.io for this
# single-file app.R app (see the comment in app.R for the full story, fixed
# 2026-09-06). Left unset here to avoid confusion about which value wins.

if (!requireNamespace("cauda", quietly = TRUE)) {
  message("cauda not found — installing from GitHub...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("Cauda-R/cauda", quiet = TRUE)
}

# Load required packages
library(cauda)
library(pdftools)
