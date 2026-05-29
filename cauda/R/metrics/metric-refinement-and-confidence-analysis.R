# ============================================================================
# OPTION B: METRIC REFINEMENT - TEST DIFFERENT WEIGHTINGS & THRESHOLDS
# ============================================================================

refine_composite_score <- function(f1, calibration, high_conf_halluc_rate,
                                   f1_weight = 0.60,
                                   calib_weight = 0.25,
                                   halluc_weight = 0.15) {
  # Flexible composite score with adjustable weights
  # Ensures weights sum to 1.0

  weights_sum <- f1_weight + calib_weight + halluc_weight
  f1_w <- f1_weight / weights_sum
  calib_w <- calib_weight / weights_sum
  halluc_w <- halluc_weight / weights_sum

  composite <- (f1 * f1_w) + (calibration * calib_w) + ((1 - high_conf_halluc_rate) * halluc_w)
  return(composite)
}

# ============================================================================
# METRIC SENSITIVITY ANALYSIS
# ============================================================================

analyze_metric_sensitivity <- function(all_results,
                                       halluc_clarity_weights = c(0.1, 0.3, 0.5, 0.7),
                                       thresholds = c(0.70, 0.72, 0.75, 0.78, 0.80),
                                       verbose = TRUE) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         METRIC SENSITIVITY ANALYSIS                        ║\n")
  cat("║  Testing different hallucination_clarity weights &         ║\n")
  cat("║  production readiness thresholds                          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results_summary <- list()

  for (hc_weight in halluc_clarity_weights) {
    for (threshold in thresholds) {

      # Recalculate composite with different weights
      calib_weight <- 0.7 * (1 - hc_weight/10)  # Scale calibration weight
      f1_weight <- 0.60
      halluc_weight <- hc_weight / 10

      # Apply to all results
      updated_results <- all_results
      for (i in seq_along(updated_results)) {
        r <- updated_results[[i]]

        # Recalculate calibration with adjusted halluc_clarity weight
        high_conf_prec <- r$high_confidence_precision_v2
        halluc_clarity <- r$hallucination_clarity_v2
        calibration_adj <- (high_conf_prec * 0.7) + (halluc_clarity * calib_weight)

        # Recalculate composite with new weights
        updated_results[[i]]$composite_adj <- refine_composite_score(
          r$f1,
          calibration_adj,
          r$high_confidence_hallucination_rate,
          f1_weight = f1_weight,
          calib_weight = calibration_adj,
          halluc_weight = halluc_weight
        )
        updated_results[[i]]$ready_adj <- (updated_results[[i]]$composite_adj > threshold)
      }

      # Count results
      ready_count <- sum(sapply(updated_results, \(x) x$ready_adj))
      total_count <- length(updated_results)
      pass_rate <- ready_count / total_count

      # Extract realistic scenario only
      realistic_results <- updated_results[sapply(updated_results, \(x) x$extraction_scenario == "realistic")]
      realistic_ready <- sum(sapply(realistic_results, \(x) x$ready_adj))
      realistic_rate <- realistic_ready / length(realistic_results)

      results_summary[[length(results_summary) + 1]] <- list(
        halluc_weight = hc_weight,
        threshold = threshold,
        total_ready = ready_count,
        total_count = total_count,
        overall_pass_rate = pass_rate,
        realistic_pass_rate = realistic_rate,
        realistic_ready = realistic_ready
      )
    }
  }

  # Convert to dataframe for display
  summary_df <- do.call(rbind, lapply(results_summary, as.data.frame))

  if (verbose) {
    cat("\n--- SENSITIVITY MATRIX ---\n")
    cat("(Overall pass rate | Realistic pass rate)\n\n")

    # Create pivot table
    for (hc_weight in halluc_clarity_weights) {
      cat(sprintf("Halluc Weight = %.1f: ", hc_weight))
      for (threshold in thresholds) {
        row <- summary_df[summary_df$halluc_weight == hc_weight & summary_df$threshold == threshold, ]
        cat(sprintf("T%.2f: %.1f%%|%.1f%%  ",
                    threshold,
                    row$overall_pass_rate * 100,
                    row$realistic_pass_rate * 100))
      }
      cat("\n")
    }

    # Find optimal
    cat("\n--- RECOMMENDATIONS ---\n")

    # Optimize for realistic extraction (most important)
    best_realistic <- summary_df[which.max(summary_df$realistic_pass_rate), ]
    cat(sprintf("✓ Best for realistic extraction:\n"))
    cat(sprintf("  Halluc weight: %.1f, Threshold: %.2f\n",
                best_realistic$halluc_weight, best_realistic$threshold))
    cat(sprintf("  → Realistic pass rate: %.1f%% (%d/%d papers)\n",
                best_realistic$realistic_pass_rate * 100,
                best_realistic$realistic_ready,
                nrow(realistic_results)))

    # Balance overall + realistic
    summary_df$balance_score <- (summary_df$overall_pass_rate * 0.3) + (summary_df$realistic_pass_rate * 0.7)
    best_balance <- summary_df[which.max(summary_df$balance_score), ]
    cat(sprintf("\n✓ Best balance (realistic-focused):\n"))
    cat(sprintf("  Halluc weight: %.1f, Threshold: %.2f\n",
                best_balance$halluc_weight, best_balance$threshold))
    cat(sprintf("  → Overall: %.1f%%, Realistic: %.1f%%\n",
                best_balance$overall_pass_rate * 100,
                best_balance$realistic_pass_rate * 100))
  }

  return(list(
    summary = summary_df,
    best_for_realistic = best_realistic,
    best_balanced = best_balance
  ))
}

# ============================================================================
# OPTION C: CONFIDENCE-AWARE ANALYSIS
# ============================================================================

# Split claims by confidence level
separate_by_confidence <- function(extracted_claims) {
  high_conf <- extracted_claims[extracted_claims$confidence == "high", ]
  medium_conf <- extracted_claims[extracted_claims$confidence == "medium", ]
  low_conf <- extracted_claims[extracted_claims$confidence == "low", ]

  return(list(
    high = high_conf,
    medium = medium_conf,
    low = low_conf,
    all = extracted_claims
  ))
}

# Analyze confidence distribution
analyze_confidence_distribution <- function(extracted_claims, verbose = TRUE) {

  separated <- separate_by_confidence(extracted_claims)

  total_claims <- nrow(extracted_claims)

  distribution <- list(
    total = total_claims,
    high_count = nrow(separated$high),
    high_pct = nrow(separated$high) / total_claims * 100,
    medium_count = nrow(separated$medium),
    medium_pct = nrow(separated$medium) / total_claims * 100,
    low_count = nrow(separated$low),
    low_pct = nrow(separated$low) / total_claims * 100
  )

  if (verbose) {
    cat("\n--- CONFIDENCE DISTRIBUTION ---\n")
    cat(sprintf("Total claims: %d\n", distribution$total))
    cat(sprintf("  High:   %d (%.1f%%)\n", distribution$high_count, distribution$high_pct))
    cat(sprintf("  Medium: %d (%.1f%%)\n", distribution$medium_count, distribution$medium_pct))
    cat(sprintf("  Low:    %d (%.1f%%)\n", distribution$low_count, distribution$low_pct))
  }

  return(distribution)
}

# Compare high-conf claims vs all claims
confidence_aware_accuracy <- function(paper_obj, extracted_claims, verbose = TRUE) {

  gt_edges <- paper_obj$ground_truth_edges
  gt_edge_set <- paste(gt_edges$source, "->", gt_edges$target)

  separated <- separate_by_confidence(extracted_claims)

  # Metrics for each confidence level
  metrics <- list()

  for (conf_level in c("high", "medium", "low", "all")) {
    claims <- separated[[conf_level]]

    if (nrow(claims) == 0) {
      metrics[[conf_level]] <- list(
        count = 0,
        correct = 0,
        accuracy = NA,
        precision = NA
      )
      next
    }

    edge_set <- paste(claims$source, "->", claims$target)
    correct <- sum(edge_set %in% gt_edge_set)

    metrics[[conf_level]] <- list(
      count = nrow(claims),
      correct = correct,
      accuracy = correct / nrow(claims),
      precision = correct / nrow(claims)
    )
  }

  if (verbose) {
    cat("\n--- ACCURACY BY CONFIDENCE LEVEL ---\n")
    for (conf_level in c("high", "medium", "low", "all")) {
      m <- metrics[[conf_level]]
      if (m$count == 0) {
        cat(sprintf("%-6s: No claims\n", conf_level))
      } else {
        cat(sprintf("%-6s: %d claims, %d correct (%.1f%% accurate)\n",
                    conf_level, m$count, m$correct, m$accuracy * 100))
      }
    }
  }

  return(metrics)
}

# Create confidence-filtered DAG
create_confidence_dag <- function(extracted_claims, confidence_level = "high") {

  separated <- separate_by_confidence(extracted_claims)

  if (confidence_level == "high_only") {
    filtered_claims <- separated$high
  } else if (confidence_level == "high_and_medium") {
    filtered_claims <- rbind(separated$high, separated$medium)
  } else if (confidence_level == "all") {
    filtered_claims <- separated$all
  } else if (confidence_level == "low_only") {
    filtered_claims <- separated$low
  }

  # Remove NAs
  filtered_claims <- filtered_claims[!is.na(filtered_claims$source) &
                                     !is.na(filtered_claims$target), ]

  if (nrow(filtered_claims) == 0) {
    cat(sprintf("No claims at confidence level: %s\n", confidence_level))
    return(NULL)
  }

  # Build DAG using bnlearn
  nodes <- unique(c(filtered_claims$source, filtered_claims$target))

  dag <- bnlearn::empty.graph(nodes)

  for (i in 1:nrow(filtered_claims)) {
    from <- filtered_claims$source[i]
    to <- filtered_claims$target[i]
    dag <- bnlearn::set.arc(dag, from, to)
  }

  return(dag)
}

# Comprehensive confidence analysis report
generate_confidence_analysis_report <- function(paper_obj, extracted_claims,
                                               ground_truth_dag = NULL,
                                               verbose = TRUE) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║      CONFIDENCE-AWARE ANALYSIS REPORT                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  cat("\nPaper:", paper_obj$title, "\n")

  # 1. Confidence distribution
  dist <- analyze_confidence_distribution(extracted_claims, verbose = TRUE)

  # 2. Accuracy by confidence level
  acc <- confidence_aware_accuracy(paper_obj, extracted_claims, verbose = TRUE)

  # 3. High-confidence filtering impact
  cat("\n--- FILTERING IMPACT ---\n")
  high_only <- nrow(extracted_claims[extracted_claims$confidence == "high", ])
  all_claims <- nrow(extracted_claims)
  cat(sprintf("Keeping high-confidence only: %.1f%% of claims (%d/%d)\n",
              high_only / all_claims * 100, high_only, all_claims))

  if (!is.null(ground_truth_dag)) {
    gt_edges <- ground_truth_dag$ground_truth_edges
    gt_edge_set <- paste(gt_edges$source, "->", gt_edges$target)

    high_edges <- paste(extracted_claims[extracted_claims$confidence == "high", ]$source, "->",
                        extracted_claims[extracted_claims$confidence == "high", ]$target)
    high_correct <- sum(high_edges %in% gt_edge_set)

    all_correct <- 0
    for (i in 1:nrow(extracted_claims)) {
      edge <- paste(extracted_claims$source[i], "->", extracted_claims$target[i])
      if (edge %in% gt_edge_set) all_correct <- all_correct + 1
    }

    cat(sprintf("Precision improvement: %.1f%% (high-only) vs %.1f%% (all claims)\n",
                high_correct / length(high_edges) * 100,
                all_correct / nrow(extracted_claims) * 100))
  }

  # 4. DAGs at different confidence levels
  cat("\n--- DAG COMPARISON ---\n")

  dag_high <- create_confidence_dag(extracted_claims, "high_only")
  dag_high_med <- create_confidence_dag(extracted_claims, "high_and_medium")
  dag_all <- create_confidence_dag(extracted_claims, "all")

  if (!is.null(dag_high)) {
    cat(sprintf("High-confidence DAG:     %d nodes, %d edges\n",
                length(dag_high$nodes), length(bnlearn::arcs(dag_high)) / 2 + nrow(bnlearn::arcs(dag_high))))
  }
  if (!is.null(dag_high_med)) {
    cat(sprintf("High+Medium DAG:         %d nodes, %d edges\n",
                length(dag_high_med$nodes), length(bnlearn::arcs(dag_high_med)) / 2 + nrow(bnlearn::arcs(dag_high_med))))
  }
  if (!is.null(dag_all)) {
    cat(sprintf("All claims DAG:          %d nodes, %d edges\n",
                length(dag_all$nodes), nrow(bnlearn::arcs(dag_all))))
  }

  # 5. Recommendations
  cat("\n--- RECOMMENDATIONS ---\n")

  high_accuracy <- acc$high$accuracy
  if (!is.na(high_accuracy) && high_accuracy >= 0.8) {
    cat("✓ High-confidence claims are reliable (>80% accurate)\n")
    cat("  → Use high-confidence claims for core analysis\n")
    cat("  → Use medium-confidence for exploratory findings\n")
  } else if (!is.na(high_accuracy) && high_accuracy >= 0.6) {
    cat("⚠ High-confidence claims are somewhat reliable (60-80%)\n")
    cat("  → Verify with domain experts\n")
  } else {
    cat("✗ High-confidence claims unreliable (<60%)\n")
    cat("  → Extraction quality may be poor\n")
  }

  return(list(
    distribution = dist,
    accuracy = acc,
    dag_high = dag_high,
    dag_high_med = dag_high_med,
    dag_all = dag_all
  ))
}

# ============================================================================
# COMBINED ANALYSIS: METRICS + CONFIDENCE
# ============================================================================

run_combined_refinement_analysis <- function(all_results) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║    COMBINED ANALYSIS: METRICS REFINEMENT + CONFIDENCE     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Part 1: Metric sensitivity
  cat("\n\n=== PART 1: METRIC SENSITIVITY ===\n")
  sensitivity <- analyze_metric_sensitivity(
    all_results,
    halluc_clarity_weights = c(0.2, 0.3, 0.4, 0.5),
    thresholds = c(0.70, 0.75, 0.80),
    verbose = TRUE
  )

  # Part 2: Extract realistic scenarios for confidence analysis
  cat("\n\n=== PART 2: CONFIDENCE ANALYSIS (Realistic Scenarios) ===\n")

  realistic_results <- all_results[sapply(all_results, \(x) x$extraction_scenario == "realistic")]

  cat(sprintf("\nAnalyzing %d realistic extraction results:\n", length(realistic_results)))

  for (i in 1:min(3, length(realistic_results))) {  # Show first 3
    r <- realistic_results[[i]]
    cat(sprintf("\n▸ %s\n", r$paper_title))
    cat(sprintf("  Scenario: %s\n", r$scenario))
    cat(sprintf("  Composite Score V2: %.3f %s\n",
                r$composite_v2,
                ifelse(r$ready_for_production_v2, "✓", "⚠")))
  }

  return(sensitivity)
}

# ============================================================================
# INITIALIZATION MESSAGE
# ============================================================================

cat("\n✓ Refinement & Confidence Analysis module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. analyze_metric_sensitivity(all_results)     - Test weightings & thresholds\n")
cat("  2. analyze_confidence_distribution(claims)     - Show confidence breakdown\n")
cat("  3. confidence_aware_accuracy(paper, claims)    - Accuracy by confidence level\n")
cat("  4. create_confidence_dag(claims, level)        - DAG filtered by confidence\n")
cat("  5. generate_confidence_analysis_report(paper, claims) - Full report\n")
cat("  6. run_combined_refinement_analysis(all_results) - Both analyses together\n")
cat("\nRun: sensitivity <- run_combined_refinement_analysis(results)\n")
