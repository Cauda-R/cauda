# =============================================================================
# test-eval.R
# Extraction evaluation test harness
# Validates extraction quality before scaling to multiple papers
#
# Workflow:
#   1. Load extraction functions
#   2. Define ground truth DAG (opioid example)
#   3. Run extraction on test text
#   4. Evaluate comprehensively
#   5. Get detailed error analysis & refinement suggestions
#
# =============================================================================

# Ground truth opioid DAG with metadata
get_ground_truth_opioid_metadata <- function() {
  data.frame(
    from = c(
      "Marketing", "Beliefs", "Prescribing", "Prescribing",
      "MedRxUse", "Addiction", "HistoricalIllicitUse",
      "CurrentIllicitUse", "CurrentIllicitUse", "PlacePolicy",
      "EconomicStress", "CommonLiability", "CommonLiability", "CommonLiability"
    ),
    to = c(
      "Beliefs", "Prescribing", "MedRxUse", "NUPO",
      "Addiction", "Overdose", "CurrentIllicitUse",
      "NUPO", "Overdose", "Addiction",
      "MentalHealth", "NUPO", "OtherDrugUse", "Addiction"
    ),
    pathway = c(
      "gateway", "gateway", "gateway", "gateway",
      "gateway", "behavioral", "behavioral",
      "behavioral", "behavioral", "structural",
      "structural", "common_liability", "common_liability", "common_liability"
    ),
    established = c(
      TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE,
      FALSE, TRUE, FALSE,
      TRUE, TRUE, TRUE, TRUE
    ),
    stringsAsFactors = FALSE
  )
}


# Build ground truth DAG
get_ground_truth_opioid_dag <- function(verbose = FALSE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required")
  }

  metadata <- get_ground_truth_opioid_metadata()

  # All nodes
  all_nodes <- unique(c(metadata$from, metadata$to))

  # Create empty DAG
  dag <- bnlearn::empty.graph(all_nodes)

  # Add edges
  for (i in seq_len(nrow(metadata))) {
    dag <- bnlearn::set.arc(dag, from = metadata$from[i], to = metadata$to[i])
  }

  # Store metadata
  attr(dag, "edge_metadata") <- metadata
  attr(dag, "pathway_colors") <- c(
    gateway = "#E84545",
    common_liability = "chartreuse4",
    structural = "royalblue3",
    behavioral = "#F2A623",
    unknown = "#888888"
  )

  if (verbose) {
    cat("Ground truth DAG loaded:\n")
    cat("  Nodes:", length(all_nodes), "\n")
    cat("  Edges:", nrow(metadata), "\n")
    cat("  Established:", sum(metadata$established), "\n")
    cat("  Speculative:", sum(!metadata$established), "\n")
  }

  return(dag)
}


# Test extraction eval
run_eval_test <- function() {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║              EXTRACTION EVALUATION TEST                       ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  # Load ground truth
  cat("STEP 1: Loading ground truth opioid DAG\n")
  cat("─────────────────────────────────────────\n")
  truth_dag <- get_ground_truth_opioid_dag(verbose = TRUE)
  truth_metadata <- get_ground_truth_opioid_metadata()

  cat("\n\nSTEP 2: Creating mock extracted claims\n")
  cat("─────────────────────────────────────────\n")
  cat("(Simulating what GPT extraction would return)\n\n")

  # Simulate extraction with some errors
  mock_extracted <- data.frame(
    claim_type = c(
      "causal_effect", "causal_effect", "causal_effect", "causal_effect",
      "causal_effect", "causal_effect", "causal_effect", "causal_effect",
      "causal_effect", "causal_effect", "causal_effect",
      "causal_effect",  # Hallucination
      "confounder"      # Different claim type
    ),
    source = c(
      "Marketing", "Beliefs", "Prescribing", "Prescribing",
      "MedRxUse", "Addiction", "HistoricalIllicitUse", "CurrentIllicitUse",
      "CurrentIllicitUse", "PlacePolicy", "EconomicStress",
      "UnknownNode",  # Hallucination
      NA
    ),
    target = c(
      "Beliefs", "Prescribing", "MedRxUse", "NUPO",
      "Addiction", "Overdose", "CurrentIllicitUse", "NUPO",
      "Overdose", "Addiction", "MentalHealth",
      "RandomNode",  # Hallucination
      NA
    ),
    pathway = c(
      "gateway", "gateway", "gateway", "gateway",
      "gateway", "behavioral", "behavioral", "behavioral",
      "behavioral", "structural", "structural",
      "unknown",
      "common_liability"
    ),
    direction = rep("positive", 13),
    strength = rep("high", 13),
    confidence = c(
      rep("high", 10), "medium", "low", "medium"
    ),
    established = c(
      rep(TRUE, 9), FALSE, TRUE, FALSE, NA
    ),
    quote = c(
      "Marketing increases beliefs",
      "Beliefs drive prescribing",
      "Prescribing leads to use",
      "NUPO emerges from prescribing",
      "Medical use causes addiction",
      "Addiction contributes to overdose",
      "Historical use predicts current use",
      "Current illicit use increases NUPO",
      "Current illicit use drives overdoses",
      "Policy affects addiction pathways",
      "Economic stress affects mental health",
      "Unknown mechanism (hallucinated)",
      "Common liability confounds"
    ),
    stringsAsFactors = FALSE
  )

  cat("Mock extracted claims:\n")
  cat("  Total claims:", nrow(mock_extracted), "\n")
  cat("  Causal effects:", sum(mock_extracted$claim_type == "causal_effect"), "\n")
  cat("  Confounders:", sum(mock_extracted$claim_type == "confounder"), "\n")
  cat("  Hallucinations:", 1, "(UnknownNode -> RandomNode)\n\n")

  cat("\n\nSTEP 3: Running comprehensive evaluation\n")
  cat("─────────────────────────────────────────\n")

  results <- cauda.eval_extraction(
    extracted_claims = mock_extracted,
    ground_truth_dag = truth_dag,
    ground_truth_metadata = truth_metadata,
    verbose = TRUE
  )

  cat("\n\nSTEP 4: Refinement Recommendations\n")
  cat("─────────────────────────────────────────\n")

  composite <- results$composite_score

  if (results$edges$f1 < 0.75) {
    cat("⚠️  EDGE DETECTION needs work:\n")
    cat("   - False positives:", results$edges$fp_count, "\n")
    cat("   - False negatives:", results$edges$fn_count, "\n")
    cat("   → Refinement: Tighten extraction prompt, add negative examples\n\n")
  }

  if (!is.null(results$pathways) && results$pathways$accuracy < 0.80) {
    cat("⚠️  PATHWAY CLASSIFICATION needs work:\n")
    cat("   - Accuracy:", sprintf("%.1f%%", 100 * results$pathways$accuracy), "\n")
    cat("   - Mismatches:", results$pathways$mismatches, "\n")
    cat("   → Refinement: Add pathway examples to extraction prompt\n\n")
  }

  if (!is.null(results$confidence) && results$confidence$overall_accuracy < 0.75) {
    cat("⚠️  CONFIDENCE CALIBRATION needs work:\n")
    cat("   - Accuracy:", sprintf("%.1f%%", 100 * results$confidence$overall_accuracy), "\n")
    cat("   → Refinement: Validate confidence assessment in prompt\n\n")
  }

  if (composite > 0.80) {
    cat("✓ READY FOR SCALING\n")
    cat("  Next: Extract from 5-10 papers, test generalization\n")
  } else if (composite > 0.70) {
    cat("~ MARGINAL - Test on second paper before scaling\n")
    cat("  Next: Apply to 1-2 additional papers, identify patterns\n")
  } else {
    cat("✗ NOT READY - Needs refinement\n")
    cat("  Next: Adjust extraction prompt and re-test\n")
  }

  cat("\n")
  cat("════════════════════════════════════════════════════════════════\n\n")

  return(invisible(results))
}


# Quick test: just run eval on extracted claims
quick_eval <- function(extracted_claims, ground_truth_dag, ground_truth_metadata) {
  cauda.eval_extraction(
    extracted_claims = extracted_claims,
    ground_truth_dag = ground_truth_dag,
    ground_truth_metadata = ground_truth_metadata,
    verbose = TRUE
  )
}
