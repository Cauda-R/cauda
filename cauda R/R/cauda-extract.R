# =============================================================================
# cauda-extract.R
# OPTIONAL ATTACHMENT: Extract causal claims from papers using ChatGPT
#
# This module is a STANDALONE EXTENSION for cauda researchers who want to:
#   - Extract theory/claims from scientific papers
#   - Convert prose into formal DAG structures
#   - Validate extraction accuracy
#
# These functions do NOT modify or depend on core cauda functionality.
# Use them alongside cauda as an optional research tool.
#
# Main functions:
#   extract_claims()     - Extract causal claims from text using GPT
#   extract_from_pdf()   - Load PDF, then extract claims
#   claims_to_dag()      - Convert claims table into bnlearn DAG
#   validate_extraction()- Compare extracted DAG vs ground truth
#
# IMPORTANT: Functions use generic names (extract_claims, not cauda.extract_claims)
# to avoid namespace collision with main cauda package.
# =============================================================================


# =============================================================================
# extract_claims()
# Sends paper text to OpenAI GPT and extracts structured causal claims
#
# Arguments:
#   text       : full text of paper (or excerpt)
#   domain     : domain context (e.g. "opioid crisis", "climate change")
#   model      : GPT model (default "gpt-4-mini" for speed/cost)
#   api_key    : OpenAI API key (default reads from OPENAI_API_KEY env var)
#   verbose    : print progress and response summary
#
# Returns:
#   Data frame with columns:
#   - claim_type: "causal_effect", "confounder", "mediator", "collider"
#   - source: cause variable name
#   - target: effect variable name
#   - pathway: hypothesis type ("gateway", "common_liability", "structural", "behavioral", "unknown")
#   - direction: "positive", "negative", "unknown"
#   - strength: "strong", "moderate", "weak", "unknown"
#   - confidence: "high", "medium", "low"
#   - established: logical (TRUE = solid evidence, FALSE = speculative)
#   - quote: original text from paper
#
# DEPENDENCIES: httr2, jsonlite
# COST: ~$0.01-0.02 per paper with gpt-4-mini
#
# =============================================================================

extract_claims <- function(text,
                          domain = "opioid crisis",
                          model = "gpt-4-mini",
                          api_key = NULL,
                          verbose = TRUE) {

  # Check dependencies
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 required. Install with: install.packages('httr2')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite required. Install with: install.packages('jsonlite')")
  }

  # Get API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") {
      stop("OPENAI_API_KEY environment variable not set.\n",
           "Set it with: Sys.setenv(OPENAI_API_KEY = 'sk-proj-YOUR_KEY_HERE')")
    }
  }

  # Truncate long text
  max_chars <- 20000
  if (nchar(text) > max_chars) {
    text <- paste0(substr(text, 1, max_chars), "\n[... text truncated ...]")
    if (verbose) cat("Note: Text truncated to", max_chars, "characters\n")
  }

  # Build prompts
  system_prompt <- paste0(
    "You are an expert in causal inference and epidemiology. ",
    "You are reading scientific literature about the ", domain, ".\n",
    "Extract ALL causal claims, confounders, mediators, and mechanisms described.\n",
    "Return ONLY valid JSON with no preamble, no markdown backticks, no narrative text."
  )

  user_prompt <- paste0(
    "Extract causal claims from this text about the ", domain, ".\n\n",
    "For each claim, return a JSON object with these fields:\n",
    "{\n",
    '  "claim_type": "causal_effect" | "confounder" | "mediator" | "collider",\n',
    '  "source": "variable or node name",\n',
    '  "target": "variable or node name",\n',
    '  "pathway": "gateway" | "common_liability" | "structural" | "behavioral" | "unknown",\n',
    '  "direction": "positive" | "negative" | "unknown",\n',
    '  "strength": "strong" | "moderate" | "weak" | "unknown",\n',
    '  "confidence": "high" | "medium" | "low",\n',
    '  "established": true | false,\n',
    '  "quote": "exact quote from text"\n',
    "}\n\n",
    "Return a JSON array of objects. Nothing else.\n\n",
    "TEXT:\n", text
  )

  if (verbose) cat("Calling OpenAI API (model:", model, ")...\n")

  # Call API
  tryCatch({
    response <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) |>
      httr2::req_body_json(list(
        model = model,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_prompt)
        ),
        temperature = 0.3,
        max_tokens = 2000
      )) |>
      httr2::req_perform()

    parsed <- httr2::resp_body_json(response)
  }, error = function(e) {
    stop("API call failed: ", e$message)
  })

  # Check for API errors
  if (!is.null(parsed$error)) {
    stop("OpenAI API error (", parsed$error$code, "): ", parsed$error$message)
  }

  response_text <- parsed$choices[[1]]$message$content

  if (verbose) {
    cat("Response received. Parsing JSON...\n")
  }

  # Parse JSON
  tryCatch({
    claims_list <- jsonlite::fromJSON(response_text)
  }, error = function(e) {
    stop("Failed to parse GPT response as JSON.\n",
         "Response preview: ", substr(response_text, 1, 300), "\n",
         "Error: ", e$message)
  })

  # Convert to data frame
  if (!is.data.frame(claims_list)) {
    if (is.list(claims_list) && length(claims_list) > 0) {
      claims_list <- do.call(rbind, lapply(claims_list, as.data.frame))
    } else {
      claims_list <- as.data.frame(claims_list)
    }
  }

  if (!is.data.frame(claims_list) || nrow(claims_list) == 0) {
    claims_list <- data.frame(
      claim_type = character(),
      source = character(),
      target = character(),
      pathway = character(),
      direction = character(),
      strength = character(),
      confidence = character(),
      established = logical(),
      quote = character(),
      stringsAsFactors = FALSE
    )
  } else {
    # Ensure all required columns exist
    required_cols <- c("claim_type", "source", "target", "pathway", "direction",
                       "strength", "confidence", "established", "quote")
    for (col in required_cols) {
      if (!col %in% names(claims_list)) {
        claims_list[[col]] <- NA_character_
      }
    }
    claims_list <- claims_list[, required_cols]
  }

  # Summary output
  if (verbose) {
    cat("\n=== Extraction Summary ===\n")
    cat("Claims extracted:", nrow(claims_list), "\n")
    if (nrow(claims_list) > 0) {
      cat("  Causal effects :", sum(claims_list$claim_type == "causal_effect", na.rm = TRUE), "\n")
      cat("  Confounders   :", sum(claims_list$claim_type == "confounder", na.rm = TRUE), "\n")
      cat("  Mediators     :", sum(claims_list$claim_type == "mediator", na.rm = TRUE), "\n")
      cat("  High confidence:", sum(claims_list$confidence == "high", na.rm = TRUE), "\n")
    }
    cat("========================\n\n")
  }

  return(claims_list)
}


# =============================================================================
# extract_from_pdf()
# Load a PDF file and extract causal claims from it
#
# Arguments:
#   pdf_path   : path to PDF file
#   ...        : additional arguments passed to extract_claims()
#
# Returns:
#   Data frame of extracted claims (same format as extract_claims)
#
# DEPENDENCIES: pdftools
#
# =============================================================================

extract_from_pdf <- function(pdf_path, ...) {

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("pdftools required. Install with: install.packages('pdftools')")
  }

  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  cat("Loading PDF:", pdf_path, "\n")
  text <- pdftools::pdf_text(pdf_path)
  text <- paste(text, collapse = "\n")

  cat("Extracted", nchar(text), "characters from PDF.\n\n")

  extract_claims(text, ...)
}


# =============================================================================
# claims_to_dag()
# Convert extracted claims into a bnlearn DAG object
#
# Arguments:
#   claims     : output from extract_claims()
#   confidence_threshold : minimum confidence to include ("low", "medium", "high")
#   include_speculative : include claims marked established=FALSE
#   verbose    : print summary
#
# Returns:
#   A bnlearn bn object (directed acyclic graph) with metadata:
#   - attr(dag, "edge_metadata") : tibble of edges with pathway & evidence info
#   - attr(dag, "pathway_colors") : color mapping for visualization
#
# DEPENDENCIES: bnlearn
#
# =============================================================================

claims_to_dag <- function(claims,
                         confidence_threshold = "low",
                         include_speculative = TRUE,
                         verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }

  if (nrow(claims) == 0) {
    stop("No claims provided.")
  }

  # Filter by confidence
  confidence_order <- c("low" = 1, "medium" = 2, "high" = 3)
  threshold_num <- confidence_order[confidence_threshold]

  claims_filtered <- claims[
    which((confidence_order[claims$confidence] >= threshold_num) &
          (include_speculative | claims$established == TRUE | is.na(claims$established))),
    , drop = FALSE
  ]

  if (nrow(claims_filtered) == 0) {
    stop("No claims meet filtering criteria.")
  }

  if (verbose) {
    cat("Filtered to", nrow(claims_filtered), "claims\n")
    cat("  - confidence >=", confidence_threshold, "\n")
    cat("  - include speculative:", include_speculative, "\n\n")
  }

  # Extract unique nodes
  sources <- unique(claims_filtered$source[!is.na(claims_filtered$source) & claims_filtered$source != ""])
  targets <- unique(claims_filtered$target[!is.na(claims_filtered$target) & claims_filtered$target != ""])
  all_nodes <- unique(c(sources, targets))

  if (verbose) {
    cat("  Nodes:", length(all_nodes), "\n")
    print(all_nodes)
    cat("\n")
  }

  # Create empty DAG
  dag <- bnlearn::empty.graph(all_nodes)

  # Add edges from causal_effect claims
  edge_metadata <- data.frame(
    from = character(),
    to = character(),
    pathway = character(),
    established = logical(),
    stringsAsFactors = FALSE
  )

  causal_claims <- claims_filtered[claims_filtered$claim_type == "causal_effect", ]

  for (i in seq_len(nrow(causal_claims))) {
    from <- causal_claims$source[i]
    to <- causal_claims$target[i]
    pathway <- causal_claims$pathway[i]
    established <- causal_claims$established[i]

    if (!is.na(from) && !is.na(to) && from != "" && to != "") {
      tryCatch({
        dag <- bnlearn::set.arc(dag, from = from, to = to)
        edge_metadata <- rbind(edge_metadata, data.frame(
          from = from,
          to = to,
          pathway = if (is.na(pathway)) "unknown" else pathway,
          established = if (is.na(established)) TRUE else established,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        if (verbose) cat("Note: Could not add edge", from, "->", to, "\n")
      })
    }
  }

  # Store metadata
  attr(dag, "edge_metadata") <- edge_metadata
  attr(dag, "pathway_colors") <- c(
    gateway = "#E84545",
    common_liability = "chartreuse4",
    structural = "royalblue3",
    behavioral = "#F2A623",
    unknown = "#888888"
  )

  if (verbose) {
    cat("=== DAG Created ===\n")
    cat("  Nodes:", length(all_nodes), "\n")
    cat("  Edges:", nrow(edge_metadata), "\n")
    cat("\n  Pathways:\n")
    pathway_counts <- table(edge_metadata$pathway)
    for (pathway in names(pathway_counts)) {
      cat("   ", pathway, ":", pathway_counts[[pathway]], "\n")
    }
    cat("\n  Established:", sum(edge_metadata$established), "\n")
    cat("  Speculative:", sum(!edge_metadata$established), "\n")
    cat("================\n\n")
  }

  return(dag)
}


# =============================================================================
# validate_extraction()
# Compare extracted DAG against a ground truth DAG
#
# Arguments:
#   extracted_dag  : DAG from claims_to_dag()
#   ground_truth_dag : reference DAG (bnlearn bn object)
#   verbose        : print detailed comparison
#
# Returns:
#   List with:
#   - precision: fraction of extracted edges that are correct
#   - recall: fraction of true edges that were found
#   - f1: harmonic mean of precision and recall
#   - true_positives: number of correct edges
#   - false_positives: edges in extracted but not in truth
#   - false_negatives: edges in truth but not in extracted
#   - direction_errors: edges where direction was reversed
#
# DEPENDENCIES: bnlearn
#
# =============================================================================

validate_extraction <- function(extracted_dag,
                               ground_truth_dag,
                               verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }

  # Get arcs from both DAGs
  extracted_arcs <- bnlearn::arcs(extracted_dag)
  truth_arcs <- bnlearn::arcs(ground_truth_dag)

  if (verbose) {
    cat("=== Extraction Validation ===\n")
    cat("Extracted edges:", nrow(extracted_arcs), "\n")
    cat("Truth edges    :", nrow(truth_arcs), "\n\n")
  }

  # Convert to edge strings
  extracted_edges <- paste0(extracted_arcs[, 1], " -> ", extracted_arcs[, 2])
  truth_edges <- paste0(truth_arcs[, 1], " -> ", truth_arcs[, 2])

  # Find matches
  true_positives <- intersect(extracted_edges, truth_edges)
  false_positives <- setdiff(extracted_edges, truth_edges)
  false_negatives <- setdiff(truth_edges, extracted_edges)

  # Check for reversed edges
  reversed_edges <- c()
  for (fp in false_positives) {
    parts <- strsplit(fp, " -> ")[[1]]
    reversed <- paste0(parts[2], " -> ", parts[1])
    if (reversed %in% truth_edges) {
      reversed_edges <- c(reversed_edges, fp)
    }
  }

  # Calculate metrics
  precision <- if (nrow(extracted_arcs) == 0) 0 else length(true_positives) / nrow(extracted_arcs)
  recall <- if (nrow(truth_arcs) == 0) 0 else length(true_positives) / nrow(truth_arcs)
  f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)

  if (verbose) {
    cat("Accuracy Metrics:\n")
    cat("  Precision:", round(precision, 3), "(", length(true_positives), "/", nrow(extracted_arcs), ")\n")
    cat("  Recall   :", round(recall, 3), "(", length(true_positives), "/", nrow(truth_arcs), ")\n")
    cat("  F1-score :", round(f1, 3), "\n\n")

    if (length(true_positives) > 0) {
      cat("True positives (", length(true_positives), "):\n")
      for (edge in head(true_positives, 5)) cat("  ", edge, "\n")
      if (length(true_positives) > 5) cat("  ... and", length(true_positives) - 5, "more\n")
      cat("\n")
    }

    if (length(false_positives) > 0) {
      cat("False positives (", length(false_positives), "):\n")
      for (edge in head(false_positives, 5)) cat("  ", edge, "\n")
      if (length(false_positives) > 5) cat("  ... and", length(false_positives) - 5, "more\n")
      cat("\n")
    }

    if (length(false_negatives) > 0) {
      cat("False negatives (", length(false_negatives), "):\n")
      for (edge in head(false_negatives, 5)) cat("  ", edge, "\n")
      if (length(false_negatives) > 5) cat("  ... and", length(false_negatives) - 5, "more\n")
      cat("\n")
    }

    if (length(reversed_edges) > 0) {
      cat("Direction reversals (", length(reversed_edges), "):\n")
      for (edge in head(reversed_edges, 3)) cat("  ", edge, " (should be reversed)\n")
      cat("\n")
    }

    cat("=============================\n\n")
  }

  invisible(list(
    precision = precision,
    recall = recall,
    f1 = f1,
    true_positives = length(true_positives),
    false_positives = false_positives,
    false_negatives = false_negatives,
    direction_errors = reversed_edges
  ))
}
