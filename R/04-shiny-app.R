#' Launch the Cauda Shiny App for PDF Causal Claim Extraction
#'
#' Opens an interactive Shiny application where users can upload PDF research
#' papers and automatically extract causal claims using the OpenAI API.
#'
#' @details
#' The app provides:
#' - File upload interface for PDF papers
#' - Automatic text extraction from PDFs
#' - Causal claim extraction via OpenAI's GPT-3.5-turbo
#' - Results displayed in formatted output
#'
#' Before running this app, ensure your OpenAI API key is set:
#' `Sys.setenv(OPENAI_API_KEY = "your-api-key")`
#'
#' Cost: approximately $0.23 per 100 research papers
#'
#' @return
#' Opens the Shiny app in your default browser. No return value.
#'
#' @examples
#' \dontrun{
#'   Sys.setenv(OPENAI_API_KEY = "sk-...")
#'   run_cauda_app()
#' }
#'
#' @export
run_cauda_app <- function() {
  app_dir <- system.file("shiny", package = "cauda")
  
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Make sure cauda is properly installed.")
  }
  
  shiny::runApp(app_dir, launch.browser = TRUE)
}
