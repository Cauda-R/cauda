#' Generate Comprehensive Multi-Module Synthesis Report
#'
#' Combines Summary, Claims, Critique, and Mechanisms into a unified appraisal
#' similar to the ASP (Automated Systematic appraisal Platform) format.
#'
#' @param text Character string - full paper text
#' @param claims Data frame - extracted claims (from cauda.extract)
#' @param critique Data frame - critique results (from cauda.critique)
#' @param verbose Logical. Print status messages. Default: TRUE
#'
#' @return List with:
#'   * summary: paper summary
#'   * key_strengths: key strengths and better-supported conclusions
#'   * key_limitations: limitations, gaps, assumptions, caveats
#'   * claims_appraisal: table of claims with support categories
#'   * cross_module_consistency: consistency check results
#'   * bottom_line: overall appraisal
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
cauda.synthesize <- function(text, claims, critique, verbose = TRUE) {

  if (nrow(claims) == 0) {
    stop("No claims to synthesize.")
  }

  # Load API key
  renviron_path <- ".Renviron"
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || nchar(api_key) < 10) {
    stop("OPENAI_API_KEY environment variable not set.")
  }

  if (verbose) cat("Phase 3: Multi-Module Synthesis\n")
  if (verbose) cat("================================\n\n")

  # 1. Generate summary
  if (verbose) cat("1. Generating paper summary...\n")
  summary <- generate_summary(text, api_key, verbose)

  # 2. Assess key strengths
  if (verbose) cat("2. Identifying key strengths...\n")
  strengths <- assess_strengths(text, claims, critique, api_key, verbose)

  # 3. Identify key limitations
  if (verbose) cat("3. Identifying key limitations...\n")
  limitations <- assess_limitations(text, claims, critique, api_key, verbose)

  # 4. Create claims appraisal table
  if (verbose) cat("4. Creating claims appraisal table...\n")
  claims_appraisal <- create_claims_appraisal(claims, critique)

  # 4.5. Aggregate confounders across all claims
  if (verbose) cat("4.5. Aggregating confounders and alternative explanations...\n")
  confounder_summary <- create_confounder_summary(claims)

  # 5. Check cross-module consistency
  if (verbose) cat("5. Checking cross-module consistency...\n")
  consistency <- check_consistency(claims, critique)

  # 5.5. Create consistency visualizations
  if (verbose) cat("5.5. Creating consistency matrices...\n")
  consistency_matrices <- create_consistency_matrices(claims, critique)

  # 6. Detect confidence vs support mismatches
  if (verbose) cat("6. Detecting confidence vs support mismatches...\n")
  mismatches <- detect_mismatches(claims, critique)

  # 7. Generate bottom-line appraisal
  if (verbose) cat("7. Generating bottom-line appraisal...\n")
  bottom_line <- generate_bottom_line(claims, critique, strengths, limitations, api_key, verbose)

  result <- list(
    summary = summary,
    key_strengths = strengths,
    key_limitations = limitations,
    claims_appraisal = claims_appraisal,
    confounder_summary = confounder_summary,
    consistency_matrices = consistency_matrices,
    mismatches = mismatches,
    cross_module_consistency = consistency,
    bottom_line = bottom_line
  )

  if (verbose) cat("\nSynthesis complete.\n")
  return(result)
}


#' Generate Paper Summary
#' @keywords internal
generate_summary <- function(text, api_key, verbose) {
  # Truncate text for summary
  text_sample <- substr(text, 1, 10000)

  prompt <- paste0(
    "Provide a 2-3 sentence summary of this paper's main contribution and type:\n\n",
    text_sample, "\n\n",
    "Summary (max 3 sentences):"
  )

  tryCatch({
    request_body <- list(
      model = "gpt-3.5-turbo",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3,
      max_tokens = 200
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

    if (httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      return(result$choices[[1]]$message$content)
    }
  }, error = function(e) {
    if (verbose) cat(sprintf("  Error generating summary: %s\n", conditionMessage(e)))
  })

  return("Summary generation failed.")
}


#' Assess Key Strengths
#' @keywords internal
assess_strengths <- function(text, claims, critique, api_key, verbose) {
  well_supported <- sum(critique$support_summary == "well_supported", na.rm = TRUE)
  partly_supported <- sum(critique$support_summary == "partly_supported", na.rm = TRUE)
  questionable <- sum(critique$support_summary == "questionable", na.rm = TRUE)
  unknown <- sum(is.na(critique$support_summary))

  evidence_summary <- sprintf(
    "Well-supported: %d | Partly-supported: %d | Questionable: %d | Unassessed: %d",
    well_supported, partly_supported, questionable, unknown
  )

  claims_summary <- paste(sapply(seq_len(nrow(claims)), function(i) {
    sprintf("- %s → %s (confidence: %s, support: %s, causal strength: %s)",
            claims$source[i], claims$target[i],
            claims$confidence[i],
            critique$support_summary[i] %||% "unassessed",
            critique$causal_strength[i] %||% "unassessed")
  }), collapse = "\n")

  prompt <- paste0(
    "You are reviewing a scientific paper. Based on the claims and critique below, ",
    "identify the key STRENGTHS of this paper's evidence in 3-4 sentences. ",
    "Be specific to THIS paper — do not use generic language.\n\n",
    "Evidence summary: ", evidence_summary, "\n\n",
    "Claims assessed:\n", claims_summary, "\n\n",
    "Paper excerpt:\n", substr(text, 1, 3000), "\n\n",
    "Describe: (1) what the evidence actually shows, (2) methodological strengths, ",
    "(3) which claims have the strongest support and why. Be concrete."
  )

  tryCatch({
    request_body <- list(
      model = "gpt-4o-mini",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3,
      max_tokens = 350
    )
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE), encode = "raw"
    )
    if (httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      gpt_text <- result$choices[[1]]$message$content
      return(paste0("Evidence Summary:\n", evidence_summary, "\n\nWhat the evidence shows:\n", gpt_text))
    }
  }, error = function(e) {
    if (verbose) cat(sprintf("  Error generating strengths: %s\n", conditionMessage(e)))
  })

  return(paste0("Evidence Summary:\n", evidence_summary))
}


#' Assess Key Limitations
#' @keywords internal
assess_limitations <- function(text, claims, critique, api_key, verbose) {
  # Collect key gaps from all critiqued claims
  all_gaps <- critique$key_gaps[!is.na(critique$key_gaps) & critique$key_gaps != ""]
  gaps_text <- if (length(all_gaps) > 0) paste(all_gaps, collapse = "; ") else "None identified"

  prompt <- paste0(
    "You are reviewing a scientific paper. Based on the critique findings below, ",
    "identify the key LIMITATIONS, gaps, and caveats in 4-5 sentences. ",
    "Be specific to THIS paper — no generic boilerplate.\n\n",
    "Evidence gaps identified by critique:\n", gaps_text, "\n\n",
    "Paper excerpt:\n", substr(text, 1, 3000), "\n\n",
    "Cover: (1) sample size and generalizability, (2) confounders not addressed, ",
    "(3) mechanism gaps, (4) follow-up duration and long-term unknowns. ",
    "Use specific numbers and details from this paper."
  )

  tryCatch({
    request_body <- list(
      model = "gpt-4o-mini",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3,
      max_tokens = 400
    )
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE), encode = "raw"
    )
    if (httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      return(paste0("Major Limitations:\n\n", result$choices[[1]]$message$content))
    }
  }, error = function(e) {
    if (verbose) cat(sprintf("  Error generating limitations: %s\n", conditionMessage(e)))
  })

  return(paste0("Major Limitations:\n\nEvidence gaps: ", gaps_text))
}


#' Create Confounder Summary Table
#' @keywords internal
create_confounder_summary <- function(claims) {
  # Extract notes as proxy for confounders (claims df doesn't have a confounders column)
  all_confounders <- if ("confounders" %in% colnames(claims)) {
    claims$confounders[!is.na(claims$confounders) & claims$confounders != ""]
  } else {
    character(0)
  }

  if (length(all_confounders) == 0) {
    return(data.frame(
      confounder = character(),
      frequency = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Split confounders by comma and clean up
  split_confounders <- unlist(strsplit(paste(all_confounders, collapse = ", "), ", "))
  split_confounders <- trimws(split_confounders)
  split_confounders <- split_confounders[split_confounders != ""]

  # Count frequency
  confounder_freq <- table(split_confounders)
  confounder_summary <- data.frame(
    confounder = names(confounder_freq),
    frequency = as.numeric(confounder_freq),
    stringsAsFactors = FALSE
  )

  # Sort by frequency
  confounder_summary <- confounder_summary[order(-confounder_summary$frequency), ]
  rownames(confounder_summary) <- NULL

  return(confounder_summary)
}


#' Create Claims Appraisal Table
#' @keywords internal
create_claims_appraisal <- function(claims, critique) {
  appraisal <- data.frame(
    claim_id = seq_len(nrow(claims)),
    source = claims$source,
    target = claims$target,
    causal_strength = critique$causal_strength,
    support_category = critique$support_summary,
    confidence_original = claims$confidence,
    confidence_adjusted = critique$confidence_adjusted,
    key_gaps = critique$key_gaps,
    stringsAsFactors = FALSE
  )

  return(appraisal)
}


#' Create Consistency Matrices
#' @keywords internal
create_consistency_matrices <- function(claims, critique) {
  matrices <- list()

  # Matrix 1: Confidence vs Causal Strength
  matrices$confidence_vs_strength <- table(
    Original_Confidence = claims$confidence,
    Causal_Strength = critique$causal_strength
  )

  # Matrix 2: Support Level vs Causal Strength
  matrices$support_vs_strength <- table(
    Support_Level = critique$support_summary,
    Causal_Strength = critique$causal_strength
  )

  # Matrix 3: Pathway vs Evidence Quality (using pathway since study_design not in extract output)
  matrices$design_vs_quality <- table(
    Pathway = claims$pathway,
    Evidence_Quality = critique$support_summary
  )

  # Matrix 4: Confidence Downgrade Summary
  confidence_order <- c("high" = 1, "medium" = 2, "low" = 3)
  downgrade_count <- sum(confidence_order[claims$confidence] < confidence_order[critique$confidence_adjusted], na.rm = TRUE)
  no_change <- sum(claims$confidence == critique$confidence_adjusted, na.rm = TRUE)
  upgrade <- sum(confidence_order[claims$confidence] > confidence_order[critique$confidence_adjusted], na.rm = TRUE)

  matrices$confidence_change <- data.frame(
    Category = c("Confidence Downgraded", "No Change", "Confidence Upgraded"),
    Count = c(downgrade_count, no_change, upgrade),
    Percentage = c(
      round(downgrade_count / nrow(claims) * 100, 1),
      round(no_change / nrow(claims) * 100, 1),
      round(upgrade / nrow(claims) * 100, 1)
    ),
    stringsAsFactors = FALSE
  )

  return(matrices)
}


#' Detect Confidence vs Support Mismatches
#' @keywords internal
detect_mismatches <- function(claims, critique) {
  mismatches <- list()

  # Find claims where original confidence is high but support is weak/questionable
  high_conf <- claims$confidence == "high"
  weak_support <- critique$support_summary %in% c("weak", "questionable", "partly_supported")
  mismatched <- which(high_conf & weak_support)

  if (length(mismatched) > 0) {
    mismatch_df <- data.frame(
      claim_id = mismatched,
      source = claims$source[mismatched],
      target = claims$target[mismatched],
      original_confidence = claims$confidence[mismatched],
      actual_support = critique$support_summary[mismatched],
      causal_strength = critique$causal_strength[mismatched],
      stringsAsFactors = FALSE
    )
    mismatches$flag_claims <- mismatch_df
    mismatches$count <- nrow(mismatch_df)
    mismatches$has_mismatches <- TRUE
  } else {
    mismatches$count <- 0
    mismatches$has_mismatches <- FALSE
  }

  return(mismatches)
}


#' Check Cross-Module Consistency
#' @keywords internal
check_consistency <- function(claims, critique) {
  consistency_report <- list()

  # Check 1: Do claimed and critiqued confidence levels align?
  confidence_alignment <- sum(claims$confidence == critique$confidence_adjusted, na.rm = TRUE) / nrow(claims)

  consistency_report$confidence_alignment <- sprintf(
    "Confidence downgrade rate: %.1f%% of claims had adjusted confidence lower than original",
    (1 - confidence_alignment) * 100
  )

  # Check 2: Evidence quality distribution & create matrix
  support_levels <- c("well_supported", "partly_supported", "questionable")
  support_counts <- sapply(support_levels, function(level) {
    sum(critique$support_summary == level, na.rm = TRUE)
  })

  consistency_report$evidence_distribution <- sprintf(
    "Well-supported: %d | Partly-supported: %d | Questionable: %d",
    support_counts["well_supported"], support_counts["partly_supported"], support_counts["questionable"]
  )

  # Build support matrix (claims x support categories)
  support_matrix <- data.frame(
    claim_id = seq_len(nrow(claims)),
    source = claims$source,
    target = claims$target,
    support = critique$support_summary,
    stringsAsFactors = FALSE
  )

  consistency_report$support_matrix <- support_matrix

  # Check 3: How many established claims have strong causal strength?
  established_claims <- isTRUE(claims$established) | claims$established == TRUE
  strong_causal <- critique$causal_strength == "strong"
  established_strong <- sum(established_claims & strong_causal, na.rm = TRUE)
  total_established <- sum(established_claims, na.rm = TRUE)
  pct <- if (total_established > 0) established_strong / total_established * 100 else 0

  consistency_report$design_strength_alignment <- sprintf(
    "Established claims with 'strong' causal rating: %.1f%%",
    pct
  )

  return(consistency_report)
}


#' Generate Bottom-Line Appraisal
#' @keywords internal
generate_bottom_line <- function(claims, critique, strengths, limitations, api_key, verbose) {
  well_supported <- sum(critique$support_summary == "well_supported", na.rm = TRUE)
  partly_supported <- sum(critique$support_summary == "partly_supported", na.rm = TRUE)
  questionable <- sum(critique$support_summary == "questionable", na.rm = TRUE)
  total <- nrow(claims)

  stats_header <- sprintf(
    "Total Claims: %d (%d well-supported, %d partly-supported, %d questionable)",
    total, well_supported, partly_supported, questionable
  )

  claims_summary <- paste(sapply(seq_len(nrow(claims)), function(i) {
    sprintf("- %s → %s: strength=%s, support=%s, conf_adj=%s",
            claims$source[i], claims$target[i],
            critique$causal_strength[i] %||% "?",
            critique$support_summary[i] %||% "?",
            critique$confidence_adjusted[i] %||% "?")
  }), collapse = "\n")

  prompt <- paste0(
    "You are writing the bottom-line appraisal of a scientific paper for a research reviewer. ",
    "Be direct, specific to THIS paper, and practically useful.\n\n",
    stats_header, "\n\n",
    "Claims:\n", claims_summary, "\n\n",
    "Key strengths: ", substr(strengths, 1, 500), "\n\n",
    "Key limitations: ", substr(limitations, 1, 500), "\n\n",
    "Write 3 short paragraphs covering:\n",
    "1. 'What This Study Shows' — what can we actually conclude from these specific results?\n",
    "2. 'What We Don't Know' — most important unknowns and gaps\n",
    "3. 'The Bottom Line' — one overall verdict on reliability and what it means for practice/future research\n",
    "Use specific details from this paper. No generic statements."
  )

  tryCatch({
    request_body <- list(
      model = "gpt-4o-mini",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3,
      max_tokens = 500
    )
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE), encode = "raw"
    )
    if (httr::status_code(response) == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      gpt_text <- result$choices[[1]]$message$content
      return(paste0(stats_header, "\n\n", gpt_text))
    }
  }, error = function(e) {
    if (verbose) cat(sprintf("  Error generating bottom line: %s\n", conditionMessage(e)))
  })

  return(stats_header)
}


# Helper: NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || (is.character(x) && x == "")) y else x
}
