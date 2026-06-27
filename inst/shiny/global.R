# global.R — runs once when the app starts (both locally and on shinyapps.io)
# Ensures cauda package is available; installs from GitHub if missing.

if (!requireNamespace("cauda", quietly = TRUE)) {
  message("cauda not found — installing from GitHub...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("Cauda-R/cauda", quiet = TRUE)
}

# Load required packages
library(cauda)
library(pdftools)
