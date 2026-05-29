# =============================================================================
# test-extract.R
# Test harness for cauda extraction attachment
#
# These are helper functions for validating extraction accuracy.
# They do NOT modify cauda core functions.
#
# Main test functions:
#   smoke_test()              - Quick test (no API needed)
#   test_with_fake_claims()   - Test claims_to_dag without GPT
#   test_full_pipeline()      - Full extraction + validation (requires API)
#
# =============================================================================


# =============================================================================
# Ground truth DAG from Tony's opioid paper
# This is hard-coded so it doesn't require accessing external files
# =============================================================================

get_ground_truth_opioid_dag <- function(verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required")
  }

  # Solid edges (established, strong evidence)
  solid_edges <- data.frame(
    from = c(
      "Marketing", "Beliefs", "Prescribing", "HistoricalIllicitUse",
      "CurrentIllicitUse", "IllicitAvail", "PlacePolicy", "EconomicStress",
      "Oversupply", "Addiction", "CommonLiability", "CommonLiability",
      "CommonLiability"
    ),
    to = c(
      "Beliefs", "Prescribing", "MedRxUse", "CurrentIllicitUse",
      "Overdose", "CurrentIllicitUse", "Addiction", "MentalHealth",
      "Diversion", "Overdose", "NUPO", "OtherDrugUse",
      "Addiction"
    ),
    pathway = c(
      "gateway", "gateway", "gateway", "behavioral",
      "behavioral", "behavioral", "structural", "structural",
      "structural", "behavioral", "common_liability", "common_liability",
      "common_liability"
    ),
    established = TRUE,
    stringsAsFactors = FALSE
  )

  # Dashed edges (speculative, contested)
  dashed_edges <- data.frame(
    from = c(
      "Marketing", "NUPO", "Oversupply", "Oversupply",
      "MedRxUse", "NUPO", "OtherDrugUse", "MentalHealth", "OtherDrugUse", "PlacePolicy", "Diversion",
      "EconomicStress", "SocialFamily", "DistControlFails", "DistControlFails", "RedFlagFails"
    ),
    to = c(
      "Prescribing", "HistoricalIllicitUse", "HistoricalIllicitUse", "Overdose",
      "Addiction", "Addiction", "HistoricalIllicitUse", "Addiction", "Addiction", "NUPO", "HistoricalIllicitUse",
      "Overdose", "OtherDrugUse", "Oversupply", "Diversion", "Diversion"
    ),
    pathway = c(
      "gateway", "gateway", "gateway", "gateway",
      "behavioral", "behavioral", "behavioral", "behavioral", "behavioral", "behavioral", "behavioral",
      "structural", "structural", "structural", "structural", "structural"
    ),
    established = FALSE,
    stringsAsFactors = FALSE
  )

  all_edges <- rbind(solid_edges, dashed_edges)
  all_nodes <- unique(c(all_edges$from, all_edges$to))

  dag <- bnlearn::empty.graph(all_nodes)

  for (i in seq_len(nrow(all_edges))) {
    dag <- bnlearn::set.arc(dag, from = all_edges$from[i], to = all_edges$to[i])
  }

  attr(dag, "edge_metadata") <- all_edges

  if (verbose) {
    cat("Ground truth opioid DAG loaded:\n")
    cat("  Nodes:", length(all_nodes), "\n")
    cat("  Edges (total):", nrow(all_edges), "\n")
    cat("    - established:", sum(all_edges$established), "\n")
    cat("    - speculative:", sum(!all_edges$established), "\n")
  }

  return(dag)
}


# =============================================================================
# smoke_test()
# Quick validation test (does NOT require OpenAI API)
# Tests that all functions load and work with fake data
# =============================================================================

smoke_test <- function() {

  cat("\n=== SMOKE TEST (No API Required) ===\n\n")

  # Load ground truth
  cat("Loading ground truth DAG...\n")
  truth_dag <- get_ground_truth_opioid_dag(verbose = TRUE)
  cat("\n")

  # Test claims_to_dag with fake data
  cat("Testing claims_to_dag with fake claims...\n")
  fake_claims <- data.frame(
    claim_type = c("causal_effect", "causal_effect", "causal_effect"),
    source = c("A", "B", "A"),
    target = c("B", "C", "C"),
    pathway = c("gateway", "gateway", "common_liability"),
    direction = c("positive", "positive", "negative"),
    strength = c("strong", "moderate", "weak"),
    confidence = c("high", "medium", "low"),
    established = c(TRUE, TRUE, FALSE),
    quote = c("quote 1", "quote 2", "quote 3"),
    stringsAsFactors = FALSE
  )

  extracted_dag <- claims_to_dag(fake_claims, verbose = TRUE)
  cat("\n")

  # Test validation
  cat("Testing validate_extraction...\n")
  val <- validate_extraction(extracted_dag, truth_dag, verbose = FALSE)
  cat("Validation test: OK\n\n")

  cat("=== SMOKE TEST PASSED ===\n\n")

  invisible(list(
    truth_dag = truth_dag,
    fake_claims = fake_claims,
    extracted_dag = extracted_dag
  ))
}


# =============================================================================
# test_with_fake_claims()
# Slightly more realistic test with synthetic claims
# Still does NOT require API
# =============================================================================

test_with_fake_claims <- function() {

  cat("\n=== TEST WITH SYNTHETIC CLAIMS (No API Required) ===\n\n")

  truth_dag <- get_ground_truth_opioid_dag(verbose = FALSE)

  # Create synthetic claims that partially match ground truth
  synthetic_claims <- data.frame(
    claim_type = c(
      "causal_effect", "causal_effect", "causal_effect", "causal_effect",
      "causal_effect", "causal_effect", "causal_effect"
    ),
    source = c(
      "Marketing", "Beliefs", "Prescribing", "CurrentIllicitUse",
      "Addiction", "CommonLiability", "CommonLiability"
    ),
    target = c(
      "Beliefs", "Prescribing", "MedRxUse", "Overdose",
      "Overdose", "NUPO", "OtherDrugUse"
    ),
    pathway = c(
      "gateway", "gateway", "gateway", "behavioral",
      "behavioral", "common_liability", "common_liability"
    ),
    direction = c("positive", "positive", "positive", "positive",
                  "positive", "positive", "positive"),
    strength = c("strong", "strong", "moderate", "strong",
                 "medium", "strong", "strong"),
    confidence = c("high", "high", "high", "medium",
                   "medium", "high", "high"),
    established = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    quote = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
    stringsAsFactors = FALSE
  )

  cat("Creating DAG from synthetic claims...\n")
  extracted_dag <- claims_to_dag(synthetic_claims, verbose = TRUE)
  cat("\n")

  cat("Validating against ground truth...\n")
  validation <- validate_extraction(extracted_dag, truth_dag, verbose = TRUE)

  cat("=== TEST COMPLETE ===\n\n")

  invisible(list(
    truth_dag = truth_dag,
    synthetic_claims = synthetic_claims,
    extracted_dag = extracted_dag,
    validation = validation
  ))
}


# =============================================================================
# test_full_pipeline()
# Full pipeline test WITH OpenAI API
# Requires: OPENAI_API_KEY environment variable set
# =============================================================================

test_full_pipeline <- function(api_key = NULL) {

  cat("\n=== FULL PIPELINE TEST (Requires OpenAI API) ===\n\n")

  # Check API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }

  if (api_key == "") {
    cat("ERROR: OPENAI_API_KEY not set\n")
    cat("Set it with: Sys.setenv(OPENAI_API_KEY = 'sk-proj-YOUR_KEY')\n\n")
    return(invisible(NULL))
  }

  # Get ground truth
  cat("Step 1: Loading ground truth DAG\n")
  cat("-----------------------------------\n")
  truth_dag <- get_ground_truth_opioid_dag(verbose = TRUE)
  cat("\n")

  # Synthetic paper excerpt (in real usage, you'd load from PDF or paste text)
  cat("Step 2: Preparing paper text\n")
  cat("-----------------------------------\n")
  paper_text <- "
  Marketing of prescription opioids has been identified as increasing physician beliefs
  about the safety of opioid medications. These beliefs directly increase prescribing rates.
  Increased prescribing leads to higher rates of nonmedical opioid use.
  Nonmedical prescription opioid use is strongly associated with the development of addiction.
  Current illicit drug use significantly increases overdose risk.
  Historical illicit drug use predicts current illicit drug use.
  Common liability factors (genetic vulnerability, environmental stress) drive multiple outcomes
  including addiction, nonmedical opioid use, and other drug use.
  Economic stress contributes to addiction risk.
  The availability of illicit drugs increases current illicit drug use.
  "
  cat("Paper text prepared (", nchar(paper_text), "chars )\n\n")

  # Extract claims
  cat("Step 3: Extracting claims from text using GPT\n")
  cat("-----------------------------------\n")
  claims <- extract_claims(
    paper_text,
    domain = "opioid crisis",
    model = "gpt-4-mini",
    api_key = api_key,
    verbose = TRUE
  )
  cat("\n")

  # Convert to DAG
  cat("Step 4: Converting claims to DAG\n")
  cat("-----------------------------------\n")
  extracted_dag <- claims_to_dag(claims, verbose = TRUE)
  cat("\n")

  # Validate
  cat("Step 5: Validating extraction\n")
  cat("-----------------------------------\n")
  validation <- validate_extraction(extracted_dag, truth_dag, verbose = TRUE)
  cat("\n")

  cat("=== TEST COMPLETE ===\n\n")

  invisible(list(
    truth_dag = truth_dag,
    claims = claims,
    extracted_dag = extracted_dag,
    validation = validation
  ))
}
