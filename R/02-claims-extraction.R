#' Extract and Assess Causal Claims from Academic Text
#'
#' Comprehensive claims extraction using GPT-4-turbo that:
#' 1. Extracts novel causal claims from academic papers
#' 2. Assesses evidence strength and confidence levels
#' 3. Classifies mechanistic pathways
#' 4. Returns structured dataframe compatible with cauda.claims_to_dag()
#'
#' @param text Character string containing the text to analyze (typically from PDF)
#' @param model Character string specifying GPT model. Default: "gpt-4-turbo"
#' @param temperature Numeric between 0 and 1. Default: 0.3 (focused/deterministic)
#' @param max_tokens Integer maximum response length. Default: 4000
#' @param return_raw_text Logical. If TRUE, return raw GPT response for debugging. Default: FALSE
#' @param verbose Logical. Print status messages. Default: TRUE
#'
#' @return Either:
#'   - If return_raw_text=FALSE (default): Data frame with columns:
#'     * source: independent variable/causal factor
#'     * target: dependent variable/outcome
#'     * claim: full claim statement with specifics
#'     * claim_type: "causal_effect", "mechanism", "conditional", "dose-response", "moderated"
#'     * confidence: "high", "medium", or "low" based on evidence quality
#'     * effect_size: e.g., "β=0.45", "d=1.2", "HR=0.58", or "unclear"
#'     * p_value: e.g., "p<0.001", "p=0.034", or "unreported"
#'     * sample_size: e.g., "N=284", or "unreported"
#'     * pathway: "gateway", "common_liability", "structural", "behavioral", "physiological"
#'     * established: logical (TRUE=strong evidence, FALSE=preliminary/speculative)
#'     * evidence: supporting evidence (test stats, mechanism, effect magnitude)
#'     * notes: important qualifications, moderators, limitations
#'   - If return_raw_text=TRUE: Character string with raw GPT-4 response
#'
#' @details
#' This is the main extraction function for Cauda. It uses GPT-4-turbo with 128k
#' context window for high-quality claim extraction from academic papers.
#'
#' Requires OPENAI_API_KEY environment variable. Set via .Renviron or:
#'   Sys.setenv(OPENAI_API_KEY = "sk-...")
#'
#' The prompt emphasizes QUALITY over QUANTITY. It includes examples of good vs bad
#' claims and explicitly rejects generic statements. Better to extract fewer high-quality
#' claims than many weak ones.
#'
#' @examples
#' \dontrun{
#'   # Set your API key
#'   Sys.setenv(OPENAI_API_KEY = "sk-your-key")
#'
#'   # Extract from paper text
#'   text <- readLines("paper.txt", warn = FALSE) |> paste(collapse = "\n")
#'   claims <- cauda.extract(text)
#'
#'   # View results
#'   print(claims)
#'
#'   # Convert to DAG
#'   dag <- cauda.claims_to_dag(claims)
#'   plot(dag)
#' }
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
cauda.extract <- function(
  text,
  model = "gpt-4-turbo",
  temperature = 0.3,
  max_tokens = 4000,
  return_raw_text = FALSE,
  verbose = TRUE
) {

  # Load .Renviron if it exists (important for ShinyApps.io)
  renviron_path <- ".Renviron"
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }

  # Verify API key is set
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    stop("OPENAI_API_KEY environment variable not set. ",
         "Set in .Renviron or via: Sys.setenv(OPENAI_API_KEY = 'sk-...')")
  }

  # GPT-4-turbo has 128k token context
  # Estimate: 1 character ≈ 1.3 tokens
  # Reserve 30k tokens for prompt + response, use ~70k for text
  max_text_chars <- 60000
  if (nchar(text) > max_text_chars) {
    text <- substr(text, 1, max_text_chars)
    message(sprintf("Text truncated to %d characters (GPT-4-turbo limit)", max_text_chars))
  }

  # Build advanced extraction prompt for GPT-4 with examples
  prompt <- paste0(
    "You are a world-class causal inference expert reviewing academic research.\n",
    "Extract ONLY the novel, specific, high-quality causal claims this paper contributes.\n\n",

    "DEFINITION OF GOOD CLAIMS:\n",
    "✓ Novel findings specific to THIS paper (not textbook knowledge)\n",
    "✓ Based on actual data/results (not speculation or background)\n",
    "✓ From Results/Discussion sections with empirical support\n",
    "✓ Includes magnitude/direction (X increases Y by 30%, X → Y p<0.01)\n",
    "✓ Mechanistic specificity (not just 'X affects Y')\n",
    "✓ Properly qualified (conditional on Z, under conditions C)\n\n",

    "DEFINITION OF BAD CLAIMS (REJECT THESE):\n",
    "✗ Generic textbook statements ('Stress affects health')\n",
    "✗ Background knowledge appearing in introduction\n",
    "✗ Vague unsupported claims from abstract\n",
    "✗ 'X is important' without specific causal mechanism\n",
    "✗ Obvious or trivial relationships\n",
    "✗ Speculation not grounded in data\n\n",

    "EXAMPLES OF HIGH-QUALITY CLAIMS:\n",
    "1. SOURCE: Sleep deprivation (hours/night) | TARGET: Error rate (%) | EVIDENCE: Each hour lost increases errors 12%, 95%CI[8-16], N=284 | PATHWAY: behavioral\n",
    "2. SOURCE: Maternal stress (cortisol ng/ml) | TARGET: Birth weight (g) | TYPE: conditional | EVIDENCE: Effect significant only in 3rd trimester, β=-45, p=0.003 | PATHWAY: physiological\n",
    "3. SOURCE: Social isolation | TARGET: Depression symptoms | TYPE: mechanism | EVIDENCE: Mediated through reduced BDNF, indirect effect b=0.34, p<0.001 | PATHWAY: common_liability\n",
    "4. SOURCE: Treatment A vs Control | TARGET: Survival (months) | TYPE: causal_effect | EVIDENCE: Hazard ratio=0.58 [0.42-0.79], p<0.001, N=412 RCT | PATHWAY: structural\n",
    "5. SOURCE: Physical activity (hours/week) | TARGET: Cognitive decline risk | TYPE: dose-response | EVIDENCE: Each 5 hrs/week activity reduces risk 8%, linear trend p=0.02, 10-year follow-up | PATHWAY: behavioral\n\n",

    "EVIDENCE STRENGTH CLASSIFICATION:\n",
    "HIGH confidence: RCT, clear p<0.01, large effect, n>100, replicated findings\n",
    "MEDIUM confidence: Observational with controls, p<0.05, moderate effect, medium n, clear mechanism\n",
    "LOW confidence: p<0.10, small effects, high uncertainty, preliminary findings, small n\n\n",

    "PATHWAY CLASSIFICATION:\n",
    "- gateway: Initial causal event that triggers cascade\n",
    "- common_liability: Shared genetic/environmental cause of both X and Y\n",
    "- structural: Institutional/systemic mechanism\n",
    "- behavioral: Behavioral pathway (learning, habituation, etc)\n",
    "- physiological: Biological/medical mechanism\n",
    "- unknown: Mechanism unclear from paper\n\n",

    "FOR EACH CLAIM, output EXACTLY this format (separated by ----):\n",
    "CLAIM: [exact statement from paper with specificity, e.g., 'A increases B by 20%, p<0.01']\n",
    "SOURCE: [independent variable / cause]\n",
    "TARGET: [dependent variable / effect]\n",
    "TYPE: [causal_effect / mechanism / conditional / dose-response / other]\n",
    "CONFIDENCE: [high / medium / low]\n",
    "EFFECT_SIZE: [e.g., β=0.45, r=0.32, HR=0.58, Cohen's d=1.2, or 'unclear']\n",
    "P_VALUE: [e.g., p<0.001, p=0.034, or 'unreported']\n",
    "SAMPLE_SIZE: [N value if stated, or 'unreported']\n",
    "EVIDENCE: [specific support: test statistics, mechanism, citation number]\n",
    "PATHWAY: [gateway / common_liability / structural / behavioral / physiological / unknown]\n",
    "ESTABLISHED: [true = strong evidence / false = preliminary/speculative]\n",
    "NOTES: [any important qualifications, moderators, or limitations]\n",
    "----\n\n",

    "CRITICAL RULES:\n",
    "1. Extract fewer high-quality claims rather than many weak claims\n",
    "2. Always include specific numbers, p-values, effect sizes when available\n",
    "3. Distinguish between causal claims vs descriptive associations\n",
    "4. Mark speculative findings as ESTABLISHED: false\n",
    "5. Include conditional factors (when, where, for whom effects occur)\n",
    "6. Verify claims are from Results/Discussion, NOT just abstract\n",
    "7. If paper shows NO strong causal claims, return empty (that's fine)\n\n",

    "PAPER TEXT:\n",
    text
  )

  # Build API request
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

  # Make API call
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

    # Check for HTTP errors
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
    response_text <- httr::content(response, as = "text")
    result <- tryCatch(
      jsonlite::fromJSON(response_text, simplifyVector = FALSE),
      error = function(e) stop(sprintf("Failed to parse API response: %s", conditionMessage(e)))
    )

    # Extract claims text
    if (is.null(result$choices) || length(result$choices) == 0) {
      stop("No choices in API response")
    }

    if (is.null(result$choices[[1]]$message$content)) {
      stop("No message content in API response")
    }

    claims_text <- result$choices[[1]]$message$content

    # Return raw text if requested (useful for debugging)
    if (return_raw_text) {
      return(claims_text)
    }

    # Parse text response into structured dataframe
    claims_df <- parse_claims_to_dataframe(claims_text)
    return(claims_df)

  }, error = function(e) {
    stop(sprintf("Error extracting claims: %s", conditionMessage(e)))
  })
}


#' Parse GPT-4 Claims Response into Structured Dataframe
#'
#' Converts the raw text output from GPT-4 extraction into a properly formatted
#' dataframe compatible with cauda.claims_to_dag().
#'
#' @param claims_text Character string containing GPT-4 response with claims blocks
#' @return Data frame with columns: source, target, claim, claim_type, confidence,
#'   pathway, established, evidence
#'
#' @keywords internal
#' @importFrom stats na.omit
parse_claims_to_dataframe <- function(claims_text) {

  # Split by claim blocks (----)
  blocks <- strsplit(claims_text, "----")[[1]]
  blocks <- trimws(blocks)
  blocks <- blocks[blocks != ""]

  # Initialize result dataframe
  claims_list <- list()

  # Parse each block
  for (block in blocks) {
    lines <- strsplit(block, "\n")[[1]]

    # Extract fields using pattern matching
    claim_data <- list(
      source = NA_character_,
      target = NA_character_,
      claim = NA_character_,
      claim_type = NA_character_,
      confidence = NA_character_,
      effect_size = NA_character_,
      p_value = NA_character_,
      sample_size = NA_character_,
      pathway = NA_character_,
      established = NA,
      evidence = NA_character_,
      notes = NA_character_
    )

    for (line in lines) {
      line <- trimws(line)

      # Match field patterns (case-insensitive)
      if (grepl("^CLAIM:", line, ignore.case = TRUE)) {
        claim_data$claim <- trimws(sub("^CLAIM:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^SOURCE:", line, ignore.case = TRUE)) {
        claim_data$source <- trimws(sub("^SOURCE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^TARGET:", line, ignore.case = TRUE)) {
        claim_data$target <- trimws(sub("^TARGET:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^TYPE:", line, ignore.case = TRUE)) {
        claim_data$claim_type <- trimws(sub("^TYPE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^CONFIDENCE:", line, ignore.case = TRUE)) {
        conf_raw <- trimws(sub("^CONFIDENCE:\\s*", "", line, ignore.case = TRUE))
        conf_lower <- tolower(conf_raw)
        if (grepl("high", conf_lower)) {
          claim_data$confidence <- "high"
        } else if (grepl("medium", conf_lower)) {
          claim_data$confidence <- "medium"
        } else if (grepl("low", conf_lower)) {
          claim_data$confidence <- "low"
        } else {
          claim_data$confidence <- "medium"
        }
      } else if (grepl("^EFFECT_SIZE:", line, ignore.case = TRUE)) {
        claim_data$effect_size <- trimws(sub("^EFFECT_SIZE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^P_VALUE:", line, ignore.case = TRUE)) {
        claim_data$p_value <- trimws(sub("^P_VALUE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^SAMPLE_SIZE:", line, ignore.case = TRUE)) {
        claim_data$sample_size <- trimws(sub("^SAMPLE_SIZE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^PATHWAY:", line, ignore.case = TRUE)) {
        claim_data$pathway <- trimws(sub("^PATHWAY:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^ESTABLISHED:", line, ignore.case = TRUE)) {
        est_raw <- trimws(sub("^ESTABLISHED:\\s*", "", line, ignore.case = TRUE))
        est_lower <- tolower(est_raw)
        if (grepl("^(true|yes|1|established)", est_lower)) {
          claim_data$established <- TRUE
        } else if (grepl("^(false|no|0|speculative|preliminary)", est_lower)) {
          claim_data$established <- FALSE
        } else {
          claim_data$established <- NA
        }
      } else if (grepl("^EVIDENCE:", line, ignore.case = TRUE)) {
        claim_data$evidence <- trimws(sub("^EVIDENCE:\\s*", "", line, ignore.case = TRUE))
      } else if (grepl("^NOTES:", line, ignore.case = TRUE)) {
        claim_data$notes <- trimws(sub("^NOTES:\\s*", "", line, ignore.case = TRUE))
      }
    }

    # Only add if we have source and target
    if (!is.na(claim_data$source) && claim_data$source != "" &&
        !is.na(claim_data$target) && claim_data$target != "") {
      claims_list[[length(claims_list) + 1]] <- claim_data
    }
  }

  # Convert list to dataframe
  if (length(claims_list) == 0) {
    # Return empty dataframe with correct structure
    return(data.frame(
      source = character(),
      target = character(),
      claim = character(),
      claim_type = character(),
      confidence = character(),
      effect_size = character(),
      p_value = character(),
      sample_size = character(),
      pathway = character(),
      established = logical(),
      evidence = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build dataframe from list
  claims_df <- do.call(rbind, lapply(claims_list, function(x) {
    data.frame(
      source = x$source %||% NA_character_,
      target = x$target %||% NA_character_,
      claim = x$claim %||% NA_character_,
      claim_type = x$claim_type %||% NA_character_,
      confidence = x$confidence %||% NA_character_,
      effect_size = x$effect_size %||% NA_character_,
      p_value = x$p_value %||% NA_character_,
      sample_size = x$sample_size %||% NA_character_,
      pathway = x$pathway %||% NA_character_,
      established = x$established %||% NA,
      evidence = x$evidence %||% NA_character_,
      notes = x$notes %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  rownames(claims_df) <- NULL

  # Normalize confidence levels
  claims_df$confidence <- tolower(claims_df$confidence)
  claims_df$confidence[!claims_df$confidence %in% c("high", "medium", "low")] <- "medium"

  # Normalize pathway types
  valid_pathways <- c("gateway", "common_liability", "structural", "behavioral", "physiological", "unknown")
  claims_df$pathway <- tolower(claims_df$pathway)
  claims_df$pathway[!claims_df$pathway %in% valid_pathways] <- "unknown"

  # Normalize claim types
  valid_types <- c("causal_effect", "mechanism", "conditional", "dose-response", "moderated", "other")
  claims_df$claim_type <- tolower(claims_df$claim_type)
  claims_df$claim_type[!claims_df$claim_type %in% valid_types] <- "causal_effect"

  return(claims_df)
}


# Helper function for NULL coalescing operator (||) for base R
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || (is.character(x) && x == "")) y else x
}


#' Extract Causal Claims from a PDF File
#'
#' Convenience wrapper that reads a PDF paper and extracts causal claims in one step.
#' Internally calls cauda.extract() on the PDF text.
#'
#' @param pdf_path Character string path to PDF file
#' @param ... Additional arguments passed to cauda.extract()
#'
#' @return Data frame with extracted claims (see cauda.extract)
#'
#' @details
#' Extracts text from all pages of the PDF and passes to cauda.extract().
#' Requires pdftools package.
#'
#' @examples
#' \dontrun{
#'   # Extract claims from a paper
#'   claims <- cauda.extract_pdf("paper.pdf")
#'
#'   # Generate DAG from claims
#'   dag <- cauda.claims_to_dag(claims)
#'   plot(dag)
#' }
#'
#' @export
#' @importFrom pdftools pdf_text
cauda.extract_pdf <- function(pdf_path, ...) {

  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("pdftools required. Install with: install.packages('pdftools')")
  }

  # Extract text from PDF
  pdf_pages <- pdftools::pdf_text(pdf_path)
  full_text <- paste(pdf_pages, collapse = "\n")

  if (nchar(full_text) == 0) {
    stop("Could not extract text from PDF: ", pdf_path)
  }

  # Extract claims using main function
  cauda.extract(full_text, ...)
}
