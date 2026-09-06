# global.R — runs once when the app starts (both locally and on shinyapps.io)
# Ensures cauda package is available; installs from GitHub if missing.

# Shiny defaults to a 5MB upload cap, which multi-PDF or larger CSV uploads
# routinely exceed ("Maximum upload size exceeded"). Raised to 30MB, which is
# roughly the ceiling shinyapps.io itself enforces regardless of this option
# on the free/starter tiers -- setting this higher wouldn't help further.
options(shiny.maxRequestSize = 30 * 1024^2)

if (!requireNamespace("cauda", quietly = TRUE)) {
  message("cauda not found — installing from GitHub...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("Cauda-R/cauda", quiet = TRUE)
}

# Load required packages
library(cauda)
library(pdftools)
