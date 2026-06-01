#' Extract Causal Claims from Text Using OpenAI API
#'
#' Uses OpenAI's GPT models to extract causal claims from academic papers
#' and other text sources. Claims are extracted using direct HTTP calls
#' to the OpenAI API.
#'
#' @param text Character string containing the text to analyze
#' @param model Character string specifying the GPT model to use.
#'   Default: "gpt-3.5-turbo" (fast and cost-effective)
#' @param temperature Numeric between 0 and 1 controlling randomness.
#'   Default: 0.3 (lower = more focused)
#' @param max_tokens Integer maximum length of response.
#'   Default: 2000
#'
#' @return Character string containing extracted causal claims formatted as:
#'   CLAIM: [statement]
#'   SOURCE: [cause]
#'   TARGET: [effect]
#'   CONFIDENCE: [high/medium/low]
#'   ---
#'
#' @details
#' Requires OpenAI API key set in environment variable OPENAI_API_KEY.
#' Set via: Sys.setenv(OPENAI_API_KEY = "sk-...")
#'
#' Uses direct HTTP calls via httr and jsonlite packages rather than
#' the openai R package, for better stability and compatibility.
#'
#' @examples
#' \dontrun{
#'   # Set your API key first
#'   Sys.setenv(OPENAI_API_KEY = "sk-your-key-here")
#'
#'   # Extract claims
#'   text <- "Economic stress increases addiction risk. Job loss leads to depression."
#'   claims <- extract_causal_claims(text)
#'   print(claims)
#'
#'   # Use with cauda functions
#'   claims_df <- data.frame(
#'     source = c("Economic stress", "Job loss"),
#'     target = c("Addiction", "Depression"),
#'     type = c("causal", "causal")
#'   )
#'   dag <- cauda.claims_to_dag(claims_df)
#'   plot(dag)
#' }
#'
#' @export
#' @importFrom httr POST add_headers
#' @importFrom jsonlite toJSON fromJSON
extract_causal_claims <- function(
  text,
  model = "gpt-3.5-turbo",
  temperature = 0.3,
  max_tokens = 2000
) {

  # Verify API key is set
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    stop("OPENAI_API_KEY environment variable not set. ",
         "Run: Sys.setenv(OPENAI_API_KEY = 'sk-...')")
  }

  # Build the extraction prompt
  prompt <- paste0(
    "Extract ALL causal claims from the following text.\n",
    "For each claim, format EXACTLY as:\n",
    "CLAIM: [exact statement from text]\n",
    "SOURCE: [variable/factor that causes]\n",
    "TARGET: [variable/factor that is affected]\n",
    "CONFIDENCE: [high/medium/low]\n",
    "---\n\n",
    "TEXT:\n",
    text
  )

  # Build the request body
  request_body <- list(
    model = model,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    temperature = temperature,
    max_tokens = max_tokens
  )

  # Make the API call
  tryCatch({
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        `Authorization` = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )

    # Check for errors
    if (httr::status_code(response) != 200) {
      error_content <- tryCatch(
        jsonlite::fromJSON(httr::content(response, as = "text")),
        error = function(e) list(error = list(message = "Unknown error"))
      )
      stop(sprintf("OpenAI API Error (%d): %s",
                   httr::status_code(response),
                   error_content$error$message))
    }

    # Parse response
    result <- jsonlite::fromJSON(httr::content(response, as = "text"))
    claims_text <- result$choices[[1]]$message$content

    return(claims_text)

  }, error = function(e) {
    stop(sprintf("Error extracting claims: %s", conditionMessage(e)))
  })
}
