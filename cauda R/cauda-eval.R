# =============================================================================
# cauda-eval.R
# Comprehensive extraction evaluation framework
# Tests extraction quality across multiple dimensions before scaling
#
# Functions:
#   cauda.eval_extraction()      - full evaluation pipeline
#   cauda.eval_edges()           - edge detection accuracy
#   cauda.eval_pathways()        - pathway classification accuracy
#   cauda.eval_confidence()      - confidence calibration
#   cauda.eval_claim_types()     - claim type detection
#   cauda.eval_error_analysis()  - detailed error breakdown
#   cauda.eval_visualization()   - side-by-side truth vs extracted
#
# =============================================================================

# =============================================================================
# cauda.eval_edges()
# Compare extracted edges against ground truth
# Measures: TP, FP, FN, precision, recall, F1
#
# Arguments:
#   extracted_claims : claims dataframe from extraction
#   ground_truth_dag : bnlearn DAG object (ground truth)
#   verbose          : print detailed results
#
# Returns:
#   list with precision, recall, F1, TP/FP/FN counts, edge lists
#
# =============================================================================

cauda.eval_edges <- function(extracted_claims, ground_truth_dag, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required")
  }

  # Get ground truth edges
  truth_arcs <- bnlearn::arcs(ground_truth_dag)
  truth_edges <- paste0(truth_arcs[, 1], " -> ", truth_arcs[, 2])

  # Get extracted edges (only causal_effect claims)
  extracted_causal <- extracted_claims[extracted_claims$claim_type == "causal_effect", ]
  extracted_edges <- paste0(
    extracted_causal$source, " -> ", extracted_causal$target
  )

  # Remove NAs
  extracted_edges <- extracted_edges[!is.na(extracted_edges) & extracted_edges != " -> "]
  truth_edges <- truth_edges[!is.na(truth_edges) & truth_edges != " -> "]

  # Find matches
  tp <- intersect(extracted_edges, truth_edges)
  fp <- setdiff(extracted_edges, truth_edges)
  fn <- setdiff(truth_edges, extracted_edges)

  # Check for reversed edges
  reversed <- c()
  for (fp_edge in fp) {
    parts <- strsplit(fp_edge, " -> ")[[1]]
    reversed_edge <- paste0(parts[2], " -> ", parts[1])
    if (reversed_edge %in% truth_edges) {
      reversed <- c(reversed, fp_edge)
    }
  }

  # Calculate metrics
  precision <- if (length(extracted_edges) == 0) 0 else length(tp) / length(extracted_edges)
  recall <- if (length(truth_edges) == 0) 0 else length(tp) / length(truth_edges)
  f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)

  if (verbose) {
    cat("\n=== Edge Accuracy ===\n")
    cat("Ground truth edges:", length(truth_edges), "\n")
    cat("Extracted edges  :", length(extracted_edges), "\n\n")
    cat("True positives  :", length(tp), "\n")
    cat("False positives :", length(fp), "\n")
    cat("False negatives :", length(fn), "\n\n")
    cat("Precision:", sprintf("%.3f", precision), "(", length(tp), "/", length(extracted_edges), ")\n")
    cat("Recall   :", sprintf("%.3f", recall), "(", length(tp), "/", length(truth_edges), ")\n")
    cat("F1-score :", sprintf("%.3f", f1), "\n")

    if (length(reversed) > 0) {
      cat("\nDirection reversals:", length(reversed), "\n")
      for (edge in head(reversed, 3)) {
        cat("  ", edge, "\n")
      }
    }
  }

  invisible(list(
    precision = precision,
    recall = recall,
    f1 = f1,
    tp_count = length(tp),
    fp_count = length(fp),
    fn_count = length(fn),
    tp_edges = tp,
    fp_edges = fp,
    fn_edges = fn,
    reversed_edges = reversed,
    total_truth = length(truth_edges),
    total_extracted = length(extracted_edges)
  ))
}


# =============================================================================
# cauda.eval_pathways()
# For edges that match, how often did we classify the pathway correctly?
#
# Arguments:
#   extracted_claims : claims from extraction
#   ground_truth_metadata : dataframe with ground truth edges & pathways
#   verbose          : print results
#
# Returns:
#   list with pathway accuracy metrics
#
# =============================================================================

cauda.eval_pathways <- function(extracted_claims, ground_truth_metadata, verbose = TRUE) {

  # Extract causal claims only
  extracted_causal <- extracted_claims[extracted_claims$claim_type == "causal_effect", ]

  # Build edge strings for matching
  extracted_edges <- data.frame(
    edge = paste0(extracted_causal$source, " -> ", extracted_causal$target),
    pathway = extracted_causal$pathway,
    stringsAsFactors = FALSE
  )

  truth_edges <- data.frame(
    edge = paste0(ground_truth_metadata$from, " -> ", ground_truth_metadata$to),
    pathway = ground_truth_metadata$pathway,
    stringsAsFactors = FALSE
  )

  # Find matching edges
  matching <- intersect(extracted_edges$edge, truth_edges$edge)

  if (length(matching) == 0) {
    if (verbose) cat("\nNo matching edges to evaluate pathways.\n")
    return(invisible(list(accuracy = NA, matches = 0)))
  }

  # Compare pathways for matching edges
  correct_pathways <- 0
  pathway_mismatches <- data.frame(edge = character(), truth = character(), extracted = character(),
                                   stringsAsFactors = FALSE)

  for (edge in matching) {
    truth_pathway <- truth_edges$pathway[truth_edges$edge == edge]
    extracted_pathway <- extracted_edges$pathway[extracted_edges$edge == edge]

    if (length(truth_pathway) > 0 && length(extracted_pathway) > 0) {
      if (truth_pathway[1] == extracted_pathway[1]) {
        correct_pathways <- correct_pathways + 1
      } else {
        pathway_mismatches <- rbind(pathway_mismatches, data.frame(
          edge = edge,
          truth = truth_pathway[1],
          extracted = extracted_pathway[1],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  pathway_accuracy <- correct_pathways / length(matching)

  if (verbose) {
    cat("\n=== Pathway Classification ===\n")
    cat("Matching edges:", length(matching), "\n")
    cat("Correct pathways:", correct_pathways, "\n")
    cat("Accuracy:", sprintf("%.3f", pathway_accuracy), "\n")

    if (nrow(pathway_mismatches) > 0) {
      cat("\nPathway mismatches:\n")
      for (i in seq_len(min(5, nrow(pathway_mismatches)))) {
        cat("  ", pathway_mismatches$edge[i], "\n")
        cat("    Expected:", pathway_mismatches$truth[i], "\n")
        cat("    Got     :", pathway_mismatches$extracted[i], "\n")
      }
      if (nrow(pathway_mismatches) > 5) {
        cat("  ... and", nrow(pathway_mismatches) - 5, "more\n")
      }
    }
  }

  invisible(list(
    accuracy = pathway_accuracy,
    matches = length(matching),
    correct = correct_pathways,
    mismatches = nrow(pathway_mismatches),
    mismatch_details = pathway_mismatches
  ))
}


# =============================================================================
# cauda.eval_confidence()
# Calibration: when we say "high confidence", how often is it correct?
#
# Arguments:
#   extracted_claims : claims from extraction
#   ground_truth_metadata : dataframe with ground truth (has established column)
#   verbose          : print results
#
# Returns:
#   list with confidence calibration metrics
#
# =============================================================================

cauda.eval_confidence <- function(extracted_claims, ground_truth_metadata, verbose = TRUE) {

  extracted_causal <- extracted_claims[extracted_claims$claim_type == "causal_effect", ]

  # Build edge matching
  extracted_edges <- data.frame(
    edge = paste0(extracted_causal$source, " -> ", extracted_causal$target),
    confidence = extracted_causal$confidence,
    stringsAsFactors = FALSE
  )

  truth_edges <- data.frame(
    edge = paste0(ground_truth_metadata$from, " -> ", ground_truth_metadata$to),
    established = ground_truth_metadata$established,
    stringsAsFactors = FALSE
  )

  # Find matches and check confidence vs ground truth
  calibration <- data.frame(
    confidence = character(),
    correct = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(extracted_edges))) {
    edge <- extracted_edges$edge[i]
    conf <- extracted_edges$confidence[i]

    is_correct <- edge %in% truth_edges$edge

    calibration <- rbind(calibration, data.frame(
      confidence = conf,
      correct = is_correct,
      stringsAsFactors = FALSE
    ))
  }

  # Accuracy by confidence level
  conf_levels <- c("high", "medium", "low")
  results <- data.frame(
    confidence = conf_levels,
    total = numeric(3),
    correct = numeric(3),
    accuracy = numeric(3),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(conf_levels)) {
    level <- conf_levels[i]
    subset <- calibration[calibration$confidence == level, ]

    if (nrow(subset) > 0) {
      results$total[i] <- nrow(subset)
      results$correct[i] <- sum(subset$correct)
      results$accuracy[i] <- sum(subset$correct) / nrow(subset)
    }
  }

  if (verbose) {
    cat("\n=== Confidence Calibration ===\n")
    cat("(Higher is better: confident claims should be correct)\n\n")

    for (i in seq_len(nrow(results))) {
      if (results$total[i] > 0) {
        cat(results$confidence[i], "confidence: ", results$correct[i], "/",
            results$total[i], " correct (", sprintf("%.1f%%", 100 * results$accuracy[i]), ")\n")
      }
    }

    # Overall calibration score
    all_correct <- sum(results$correct)
    all_total <- sum(results$total)
    overall <- all_correct / all_total

    cat("\nOverall accuracy:", sprintf("%.3f", overall), "\n")

    # Check for calibration bias
    high_conf <- results$accuracy[1]
    low_conf <- results$accuracy[3]
    if (!is.na(high_conf) && !is.na(low_conf)) {
      bias <- high_conf - low_conf
      cat("Calibration bias (high vs low):", sprintf("%.3f", bias), "\n")
      if (abs(bias) > 0.2) {
        cat("  WARNING: Poor calibration! High confidence not much better than low.\n")
      }
    }
  }

  invisible(list(
    by_confidence = results,
    overall_accuracy = sum(results$correct) / sum(results$total)
  ))
}


# =============================================================================
# cauda.eval_claim_types()
# How well do we distinguish causal_effect vs confounder vs mediator?
#
# Arguments:
#   extracted_claims : claims from extraction
#   ground_truth_claims : ground truth claims (if available)
#   verbose          : print results
#
# Returns:
#   list with claim type breakdown
#
# =============================================================================

cauda.eval_claim_types <- function(extracted_claims, ground_truth_claims = NULL, verbose = TRUE) {

  extracted_breakdown <- as.data.frame(table(extracted_claims$claim_type))
  names(extracted_breakdown) <- c("claim_type", "extracted_count")

  if (verbose) {
    cat("\n=== Claim Type Distribution ===\n")
    cat("Extracted claims:\n")
    print(extracted_breakdown)

    if (!is.null(ground_truth_claims)) {
      truth_breakdown <- as.data.frame(table(ground_truth_claims$claim_type))
      names(truth_breakdown) <- c("claim_type", "truth_count")

      cat("\nGround truth claims:\n")
      print(truth_breakdown)

      # Compare
      comparison <- merge(extracted_breakdown, truth_breakdown, by = "claim_type", all = TRUE)
      comparison[is.na(comparison)] <- 0

      cat("\nComparison:\n")
      print(comparison)
    }
  }

  invisible(extracted_breakdown)
}


# =============================================================================
# cauda.eval_error_analysis()
# Detailed breakdown of extraction errors by type
#
# Arguments:
#   extracted_claims : claims from extraction
#   edge_eval        : result from cauda.eval_edges()
#   verbose          : print detailed analysis
#
# Returns:
#   list with error patterns
#
# =============================================================================

cauda.eval_error_analysis <- function(extracted_claims, edge_eval, verbose = TRUE) {

  if (verbose) {
    cat("\n=== Error Analysis ===\n\n")

    # False positives: what did we hallucinate?
    if (length(edge_eval$fp_edges) > 0) {
      cat("FALSE POSITIVES (hallucinated edges):\n")
      cat("Count:", length(edge_eval$fp_edges), "\n\n")

      for (edge in head(edge_eval$fp_edges, 5)) {
        parts <- strsplit(edge, " -> ")[[1]]
        source <- parts[1]
        target <- parts[2]

        # Find corresponding claim
        claim <- extracted_claims[
          extracted_claims$source == source & extracted_claims$target == target, ]

        if (nrow(claim) > 0) {
          cat("  ", edge, "\n")
          cat("    Confidence:", claim$confidence[1], "\n")
          cat("    Pathway   :", claim$pathway[1], "\n")
          if (!is.na(claim$quote[1])) {
            cat("    Quote     :", substr(claim$quote[1], 1, 60), "...\n")
          }
          cat("\n")
        }
      }

      if (length(edge_eval$fp_edges) > 5) {
        cat("  ... and", length(edge_eval$fp_edges) - 5, "more false positives\n\n")
      }
    }

    # False negatives: what did we miss?
    if (length(edge_eval$fn_edges) > 0) {
      cat("FALSE NEGATIVES (missed edges):\n")
      cat("Count:", length(edge_eval$fn_edges), "\n\n")

      for (edge in head(edge_eval$fn_edges, 5)) {
        cat("  ", edge, "\n")
      }

      if (length(edge_eval$fn_edges) > 5) {
        cat("  ... and", length(edge_eval$fn_edges) - 5, "more false negatives\n\n")
      }
    }

    # Direction errors
    if (length(edge_eval$reversed_edges) > 0) {
      cat("DIRECTION ERRORS (reversed edges):\n")
      cat("Count:", length(edge_eval$reversed_edges), "\n\n")

      for (edge in head(edge_eval$reversed_edges, 3)) {
        cat("  Extracted:", edge, "\n")
        parts <- strsplit(edge, " -> ")[[1]]
        cat("  Should be:", parts[2], "->", parts[1], "\n\n")
      }
    }
  }

  invisible(list(
    fp_count = length(edge_eval$fp_edges),
    fn_count = length(edge_eval$fn_edges),
    reversed_count = length(edge_eval$reversed_edges)
  ))
}


# =============================================================================
# cauda.eval_extraction()
# MASTER EVALUATION FUNCTION: runs all tests and produces summary report
#
# Arguments:
#   extracted_claims : claims dataframe from extraction
#   ground_truth_dag : bnlearn DAG object (required)
#   ground_truth_metadata : edge metadata with pathway/established (optional)
#   ground_truth_claims : original claims used to build ground truth (optional)
#   verbose          : print full report
#
# Returns:
#   list with all evaluation results
#
# =============================================================================

cauda.eval_extraction <- function(extracted_claims,
                                  ground_truth_dag,
                                  ground_truth_metadata = NULL,
                                  ground_truth_claims = NULL,
                                  verbose = TRUE) {

  if (verbose) {
    cat("\n")
    cat("════════════════════════════════════════════════════════════════\n")
    cat("  EXTRACTION EVALUATION REPORT\n")
    cat("════════════════════════════════════════════════════════════════\n")
    cat("\n")
  }

  # 1. Edge accuracy
  edge_results <- cauda.eval_edges(extracted_claims, ground_truth_dag, verbose = verbose)

  # 2. Pathway classification (if metadata provided)
  pathway_results <- NULL
  if (!is.null(ground_truth_metadata)) {
    pathway_results <- cauda.eval_pathways(extracted_claims, ground_truth_metadata, verbose = verbose)
  }

  # 3. Confidence calibration (if metadata provided)
  conf_results <- NULL
  if (!is.null(ground_truth_metadata)) {
    conf_results <- cauda.eval_confidence(extracted_claims, ground_truth_metadata, verbose = verbose)
  }

  # 4. Claim type distribution
  claim_type_results <- cauda.eval_claim_types(extracted_claims, ground_truth_claims, verbose = verbose)

  # 5. Error analysis
  error_results <- cauda.eval_error_analysis(extracted_claims, edge_results, verbose = verbose)

  if (verbose) {
    cat("\n════════════════════════════════════════════════════════════════\n")
    cat("  SUMMARY SCORE\n")
    cat("════════════════════════════════════════════════════════════════\n\n")

    cat("Edge F1-score     :", sprintf("%.3f", edge_results$f1), "\n")

    if (!is.null(pathway_results)) {
      cat("Pathway accuracy  :", sprintf("%.3f", pathway_results$accuracy), "\n")
    }

    if (!is.null(conf_results)) {
      cat("Confidence calib  :", sprintf("%.3f", conf_results$overall_accuracy), "\n")
    }

    # Composite score
    scores <- c(edge_results$f1)
    if (!is.null(pathway_results)) scores <- c(scores, pathway_results$accuracy)
    if (!is.null(conf_results)) scores <- c(scores, conf_results$overall_accuracy)

    composite <- mean(scores, na.rm = TRUE)
    cat("\nComposite score   :", sprintf("%.3f", composite), "\n")

    if (composite > 0.80) {
      cat("Rating: EXCELLENT - Ready for scaling\n")
    } else if (composite > 0.70) {
      cat("Rating: GOOD - Refine prompt and test again\n")
    } else if (composite > 0.60) {
      cat("Rating: FAIR - Significant refinement needed\n")
    } else {
      cat("Rating: POOR - Major rework required\n")
    }

    cat("\n════════════════════════════════════════════════════════════════\n\n")
  }

  invisible(list(
    edges = edge_results,
    pathways = pathway_results,
    confidence = conf_results,
    claim_types = claim_type_results,
    errors = error_results,
    composite_score = mean(c(edge_results$f1,
                             if (!is.null(pathway_results)) pathway_results$accuracy,
                             if (!is.null(conf_results)) conf_results$overall_accuracy),
                          na.rm = TRUE)
  ))
}
