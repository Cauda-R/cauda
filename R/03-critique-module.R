#' Critique Extracted Causal Claims for Strength and Gaps
#'
#' Takes extracted claims and evaluates each for:
#' - Actual causal strength (is it truly causal or just correlational?)
#' - Evidence gaps and confounders not addressed
#' - Mechanism-to-real-world translation gaps
#' - Support level (well vs partly vs questionable)
#'
#' @param claims Data frame from cauda.extract() with extracted claims
#' @param verbose Logical. Print status messages. Default: TRUE
#'
#' @return Data frame with columns:
#'   * All original claim columns
#'   * critique: detailed critique of the claim
#'   * causal_strength: "strong" / "moderate" / "weak" (how causal is this really?)
#'   * key_gaps: critical evidence gaps
#'   * confidence_adjusted: adjusted confidence after considering gaps
#'   * support_summary: brief support assessment
#'   * mechanism_valid: mechanism actually explains outcome? (yes/no/unclear)
#'   * translation_gap: gap between mechanism and real-world effect
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
cauda.critique <- function(claims, verbose = TRUE) {

  if (nrow(claims) == 0) {
    if (verbose) cat("No claims to critique.\n")
    return(claims)
  }

  # Load .Renviron if it exists
  renviron_path <- ".Renviron"
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    stop("OPENAI_API_KEY environment variable not set.")
  }

  # Initialize critique columns
  claims$critique <- NA_character_
  claims$causal_strength <- NA_character_
  claims$key_gaps <- NA_character_
  claims$confidence_adjusted <- NA_character_
  claims$support_summary <- NA_character_
  claims$mechanism_valid <- NA_character_
  claims$translation_gap <- NA_character_

  n <- nrow(claims)
  if (verbose) cat(sprintf("Critiquing all %d claims in a single gpt-4-turbo call...\n", n))

  # Build batch prompt — all claims in one call instead of N sequential calls
  claims_text <- paste(sapply(seq_len(n), function(i) {
    row <- claims[i, ]
    paste0(
      "### CLAIM_", i, "\n",
      "Source: ", row$source, "\n",
      "Target: ", row$target, "\n",
      "Claim: ", row$claim, "\n",
      "Type: ", row$claim_type, "\n",
      "Confidence (extraction): ", row$confidence, "\n",
      "Effect Size: ", row$effect_size, "\n",
      "P-Value: ", row$p_value, "\n",
      "Sample Size: ", row$sample_size, "\n",
      "Pathway: ", row$pathway, "\n",
      "Evidence: ", row$evidence, "\n",
      "Notes: ", row$notes
    )
  }), collapse = "\n\n")

  batch_prompt <- paste0(
    "You are a critical scientific reviewer. Evaluate each of these ", n, " causal claims from one paper.\n\n",
    "CLAIMS:\n\n", claims_text, "\n\n",
    "For EACH claim respond using EXACTLY this format (keep the ### CLAIM_N header):\n\n",
    "### CLAIM_1\n",
    "CAUSAL_STRENGTH: [strong/moderate/weak]\n",
    "KEY_GAPS: [2-3 most critical gaps, comma-separated]\n",
    "MECHANISM_VALID: [yes/no/unclear — does the proposed mechanism actually explain the outcome?]\n",
    "TRANSLATION_GAP: [one sentence: gap between mechanism and real-world effect, or 'none' if tight]\n",
    "CONFIDENCE_ADJUSTED: [high/medium/low]\n",
    "SUPPORT_SUMMARY: [well_supported/partly_supported/questionable]\n",
    "CRITIQUE: [2-3 sentences on causal validity, evidence gaps, and alternative explanations]\n",
    "\n### CLAIM_2\n",
    "...\n\n",
    "RULES:\n",
    "- Vary your assessments — not all claims have the same strength\n",
    "- strong = clear experimental design + large effect + clear mechanism\n",
    "- moderate = experimental but gaps in design, mechanism, or effect size\n",
    "- weak = mainly correlational, indirect, or major confounds unaddressed\n",
    "- well_supported = evidence clearly backs the claim as stated\n",
    "- partly_supported = some support but important gaps remain\n",
    "- questionable = evidence is weak, contradictory, or design is flawed\n",
    "- Adjust CONFIDENCE_ADJUSTED down if extraction over-stated it; up if under-stated\n",
    "- Put each field on its own line with the label; CRITIQUE may span 2-3 sentences on one line"
  )

  tryCatch({
    request_body <- list(
      model = "gpt-4-turbo",
      messages = list(list(role = "user", content = batch_prompt)),
      temperature = 0.2,
      max_tokens = min(3500, 600 * n)
    )

    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        `Authorization` = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )

    if (httr::status_code(response) != 200) {
      err_body <- httr::content(response, as = "text", encoding = "UTF-8")
      warning(sprintf("Batch critique API error: status %d\n%s", httr::status_code(response), err_body))
    } else {
      response_text <- httr::content(response, as = "text", encoding = "UTF-8")
      result <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
      full_critique <- result$choices[[1]]$message$content

      # Parse each claim's section by splitting on ### CLAIM_N headers
      for (i in seq_len(n)) {
        header <- paste0("### CLAIM_", i)
        next_header <- if (i < n) paste0("### CLAIM_", i + 1) else NULL

        start_pos <- regexpr(header, full_critique, fixed = TRUE)[1]
        if (start_pos == -1) {
          if (verbose) cat(sprintf("  Warning: no section found for claim %d\n", i))
          next
        }

        end_pos <- if (!is.null(next_header)) {
          p <- regexpr(next_header, full_critique, fixed = TRUE)[1]
          if (p == -1) nchar(full_critique) + 1L else p
        } else {
          nchar(full_critique) + 1L
        }

        section <- substr(full_critique, start_pos + nchar(header), end_pos - 1L)
        parsed <- parse_critique_response(section)

        claims$critique[i]            <- parsed$critique
        claims$causal_strength[i]     <- parsed$causal_strength
        claims$key_gaps[i]            <- parsed$key_gaps
        claims$confidence_adjusted[i] <- parsed$confidence_adjusted
        claims$support_summary[i]     <- parsed$support_summary
        claims$mechanism_valid[i]     <- parsed$mechanism_valid
        claims$translation_gap[i]     <- parsed$translation_gap
      }
    }

  }, error = function(e) {
    if (verbose) cat(sprintf("Error in batch critique: %s\n", conditionMessage(e)))
  })

  if (verbose) cat("Critique complete.\n")
  return(claims)
}


#' Build Critique Prompt for a Single Claim
#'
#' @keywords internal
build_critique_prompt <- function(row) {
  paste0(
    "You are a critical scientific reviewer evaluating causal claims in research papers.\n\n",

    "CLAIM TO EVALUATE:\n",
    "Source: ", row$source, "\n",
    "Target: ", row$target, "\n",
    "Claim: ", row$claim, "\n",
    "Claim Type: ", row$claim_type, "\n",
    "Confidence (extraction): ", row$confidence, "\n",
    "Effect Size: ", row$effect_size, "\n",
    "P-Value: ", row$p_value, "\n",
    "Sample Size: ", row$sample_size, "\n",
    "Pathway: ", row$pathway, "\n",
    "Established: ", row$established, "\n",
    "Evidence: ", row$evidence, "\n",
    "Notes: ", row$notes, "\n\n",

    "YOUR TASK:\n",
    "1. Evaluate whether this claim is TRULY causal or just correlational\n",
    "2. Identify CRITICAL EVIDENCE GAPS (what's missing?)\n",
    "3. Assess alternative explanations not addressed\n",
    "4. Note any mechanism-to-real-world translation gaps\n",
    "5. Rate: CAUSAL_STRENGTH (strong/moderate/weak)\n",
    "6. Rate: SUPPORT_SUMMARY (well/partly/questionable supported)\n",
    "7. Adjust confidence downward if major gaps exist\n\n",

    "RESPONSE FORMAT (use exactly these labels):\n",
    "CAUSAL_STRENGTH: [strong/moderate/weak]\n",
    "KEY_GAPS: [list 2-3 most critical gaps]\n",
    "MECHANISM_VALID: [does the proposed mechanism actually explain the outcome? yes/no/unclear]\n",
    "TRANSLATION_GAP: [explain: does mechanism → real-world effect work? is there a gap?]\n",
    "CONFIDENCE_ADJUSTED: [adjusted confidence: high/medium/low]\n",
    "SUPPORT_SUMMARY: [well_supported/partly_supported/questionable]\n",
    "CRITIQUE: [2-3 sentence summary of main strengths and weaknesses]\n\n",

    "MECHANISM VALIDATION (critical):\n",
    "- Does the proposed pathway actually cause the observed outcome?\n",
    "- Is there a clear causal chain from mechanism to behavior?\n",
    "- Could the mechanism be present without the outcome (dissociation)?\n",
    "- Or could the outcome occur without the mechanism (alternative cause)?\n\n",

    "Be direct. Flag confounding, alternative explanations, weak designs, and\n",
    "unsupported mechanism-to-outcome leaps. Quality > politeness."
  )
}


#' Parse GPT Critique Response
#'
#' @keywords internal
parse_critique_response <- function(critique_text) {
  lines <- strsplit(critique_text, "\n")[[1]]

  result <- list(
    causal_strength     = "unknown",
    key_gaps            = NA_character_,
    mechanism_valid     = NA_character_,
    translation_gap     = NA_character_,
    confidence_adjusted = NA_character_,
    support_summary     = NA_character_,
    critique            = NA_character_
  )

  # Known field header patterns — order matters for grepl matching
  field_patterns <- list(
    causal_strength     = "^CAUSAL_STRENGTH:",
    key_gaps            = "^KEY_GAPS:",
    mechanism_valid     = "^MECHANISM_VALID:",
    translation_gap     = "^TRANSLATION_GAP:",
    confidence_adjusted = "^CONFIDENCE_ADJUSTED:",
    support_summary     = "^SUPPORT_SUMMARY:",
    critique            = "^CRITIQUE:"
  )

  # Flush accumulated value into result
  flush_field <- function(field, value) {
    value <- trimws(value)
    if (value == "" || is.null(field)) return()
    if (field == "causal_strength") {
      v <- tolower(value)
      result$causal_strength <<- if (grepl("strong|moderate|weak", v)) sub(".*(strong|moderate|weak).*", "\\1", v) else "unknown"
    } else if (field == "confidence_adjusted") {
      v <- tolower(value)
      result$confidence_adjusted <<- if (grepl("high|medium|low", v)) sub(".*(high|medium|low).*", "\\1", v) else NA_character_
    } else if (field == "support_summary") {
      result$support_summary <<- tolower(trimws(value))
    } else if (field == "mechanism_valid") {
      result$mechanism_valid <<- tolower(trimws(value))
    } else if (field == "translation_gap") {
      result$translation_gap <<- trimws(value)
    } else if (field == "key_gaps") {
      result$key_gaps <<- trimws(value)
    } else if (field == "critique") {
      result$critique <<- trimws(value)
    }
  }

  current_field <- NULL
  accumulated   <- character(0)

  for (line in lines) {
    line <- trimws(line)
    if (line == "") next

    # Check if this line starts a new field
    matched_field <- NULL
    for (fname in names(field_patterns)) {
      if (grepl(field_patterns[[fname]], line, ignore.case = TRUE)) {
        matched_field <- fname
        break
      }
    }

    if (!is.null(matched_field)) {
      # Flush previous field before starting new one
      flush_field(current_field, paste(accumulated, collapse = " "))
      current_field <- matched_field
      # Strip the label from the start of the line
      value_start <- trimws(sub(field_patterns[[matched_field]], "", line, ignore.case = TRUE))
      accumulated <- if (nchar(value_start) > 0) value_start else character(0)
    } else if (!is.null(current_field)) {
      # Continuation line — accumulate
      accumulated <- c(accumulated, line)
    }
  }

  # Flush the last field
  flush_field(current_field, paste(accumulated, collapse = " "))

  return(result)
}


# Helper: NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || (is.character(x) && x == "")) y else x
}
