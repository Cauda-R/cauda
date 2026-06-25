# Deploy Cauda Shiny App to ShinyApps.io
# Run this in RStudio Terminal or Console:
# source("DEPLOY_APP.R")

library(rsconnect)

# Deploy the Shiny app
deployApp(
  appDir = "inst/shiny",
  appName = "git-ready",
  account = "aadisoni",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\n✓ Deployment complete! The app should be live in a few moments.\n")
cat("Visit: https://aadisoni.shinyapps.io/git-ready/\n")
