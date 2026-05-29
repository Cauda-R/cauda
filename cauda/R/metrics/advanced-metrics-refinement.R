# ============================================================================
# PHASE 2: ADVANCED METRICS REFINEMENT MODULE
# ============================================================================
# Adds statistical rigor to extraction quality evaluation
# Features:
#   - Confidence intervals (bootstrap & analytical)
#   - Statistical significance testing (t-tests, effect sizes)
#   - Calibration analysis (reliability of confidence scores)
#   - Robustness scoring (consistency across scenarios)
#   - Cross-validation support
#   - Accuracy bounds estimation

library(stats)

# ============================================================================
# PART 1: BOOTSTRAP CONFIDENCE INTERVALS
# ============================================================================

calculate_bootstrap_ci <- function(metrics_vector, n_bootstrap = 1000, ci = 0.95) {
  # Calculate confidence intervals using bootstrap resampling
  # Returns: mean, lower bound, upper bound, std error

  original_mean <- mean(metrics_vector)
  n <- length(metrics_vector)

  bootstrap_means <- numeric(n_bootstrap)
  set.seed(42)

  for (i in 1:n_bootstrap) {
    # Resample with replacement
    bootstrap_sample <- sample(metrics_vector, size = n, replace = TRUE)
    bootstrap_means[i] <- mean(bootstrap_sample)
  }

  # Calculate percentile CI
  alpha <- 1 - ci
  lower_percentile <- alpha / 2 * 100
  upper_percentile <- (1 - alpha / 2) * 100

  ci_lower <- quantile(bootstrap_means, probs = lower_percentile / 100)[[1]]
  ci_upper <- quantile(bootstrap_means, probs = upper_percentile / 100)[[1]]

  std_error <- sd(bootstrap_means)

  return(list(
    mean = original_mean,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    std_error = std_error,
    margin_of_error = ci_upper - original_mean,
    bootstrap_distribution = bootstrap_means
  ))
}

# ============================================================================
# PART 2: ANALYTICAL CONFIDENCE INTERVALS (BINOMIAL)
# ============================================================================

calculate_analytical_ci_binomial <- function(successes, trials, ci = 0.95) {
  # Analytical CI for proportion (e.g., accuracy = correct/total)
  # Uses Wilson score interval (more accurate than normal approximation)

  p_hat <- successes / trials
  z <- qnorm((1 + ci) / 2)

  denominator <- 1 + z^2 / trials
  center <- (p_hat + z^2 / (2 * trials)) / denominator
  margin <- z * sqrt(p_hat * (1 - p_hat) / trials + z^2 / (4 * trials^2)) / denominator

  ci_lower <- max(0, center - margin)
  ci_upper <- min(1, center + margin)

  return(list(
    estimate = p_hat,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    margin_of_error = margin,
    method = "Wilson Score Interval"
  ))
}

# ============================================================================
# PART 3: STATISTICAL SIGNIFICANCE TESTING
# ============================================================================

test_metric_significance <- function(scenario_a, scenario_b, metric_name = "F1-Score",
                                     test_type = "t.test", verbose = TRUE) {
  # Test if two scenarios have significantly different metrics
  # test_type: "t.test", "wilcoxon", "mann_whitney"

  if (test_type == "t.test") {
    test_result <- t.test(scenario_a, scenario_b, paired = FALSE)
  } else if (test_type == "wilcoxon") {
    test_result <- wilcox.test(scenario_a, scenario_b, paired = FALSE)
  } else if (test_type == "mann_whitney") {
    test_result <- wilcox.test(scenario_a, scenario_b, paired = FALSE)
  }

  # Calculate effect size (Cohen's d for t-test)
  if (test_type == "t.test") {
    pooled_std <- sqrt(((length(scenario_a) - 1) * sd(scenario_a)^2 +
                        (length(scenario_b) - 1) * sd(scenario_b)^2) /
                       (length(scenario_a) + length(scenario_b) - 2))
    cohens_d <- (mean(scenario_a) - mean(scenario_b)) / pooled_std
  } else {
    cohens_d <- NA
  }

  is_significant <- test_result$p.value < 0.05

  result <- list(
    test_type = test_type,
    p_value = test_result$p.value,
    is_significant = is_significant,
    effect_size_cohens_d = cohens_d,
    mean_a = mean(scenario_a),
    mean_b = mean(scenario_b),
    mean_difference = mean(scenario_a) - mean(scenario_b)
  )

  if (verbose) {
    cat(sprintf("\n--- SIGNIFICANCE TEST: %s ---\n", metric_name))
    cat(sprintf("Test type: %s\n", test_type))
    cat(sprintf("Scenario A mean: %.3f\n", mean(scenario_a)))
    cat(sprintf("Scenario B mean: %.3f\n", mean(scenario_b)))
    cat(sprintf("Difference: %.3f\n", mean(scenario_a) - mean(scenario_b)))
    cat(sprintf("p-value: %.4f %s\n",
                test_result$p.value,
                ifelse(is_significant, "(SIGNIFICANT ✓)", "(NOT significant)")))

    if (!is.na(cohens_d)) {
      effect_interpretation <- if (abs(cohens_d) < 0.2) "negligible" else
                              if (abs(cohens_d) < 0.5) "small" else
                              if (abs(cohens_d) < 0.8) "medium" else "large"
      cat(sprintf("Cohen's d: %.3f (%s effect)\n", cohens_d, effect_interpretation))
    }
  }

  return(result)
}

# ============================================================================
# PART 4: CALIBRATION ANALYSIS
# ============================================================================

analyze_calibration <- function(predicted_probs, actual_labels, n_bins = 10, verbose = TRUE) {
  # Analyze if predicted confidence scores match actual accuracy
  # Perfect calibration: when model is 70% confident, it's correct 70% of time

  predicted_probs <- pmin(pmax(predicted_probs, 0), 1)  # Clamp to [0,1]

  # Bin predictions
  bin_edges <- seq(0, 1, by = 1 / n_bins)
  bin_centers <- (bin_edges[-length(bin_edges)] + bin_edges[-1]) / 2

  calibration_data <- data.frame(
    bin = numeric(),
    confidence = numeric(),
    accuracy = numeric(),
    count = numeric(),
    ece_contribution = numeric()
  )

  total_ece <- 0

  for (i in 1:(n_bins)) {
    in_bin <- predicted_probs >= bin_edges[i] & predicted_probs < bin_edges[i + 1]

    if (sum(in_bin) == 0) next

    bin_probs <- predicted_probs[in_bin]
    bin_labels <- actual_labels[in_bin]

    mean_confidence <- mean(bin_probs)
    accuracy <- mean(bin_labels)
    count <- sum(in_bin)
    ece_contribution <- abs(mean_confidence - accuracy) * count / length(predicted_probs)

    calibration_data <- rbind(calibration_data, data.frame(
      bin = i,
      confidence = mean_confidence,
      accuracy = accuracy,
      count = count,
      ece_contribution = ece_contribution
    ))

    total_ece <- total_ece + ece_contribution
  }

  # Calculate metrics
  mce <- max(abs(calibration_data$confidence - calibration_data$accuracy), na.rm = TRUE)
  brier_score <- mean((predicted_probs - actual_labels)^2)

  result <- list(
    ece = total_ece,  # Expected Calibration Error
    mce = mce,        # Maximum Calibration Error
    brier = brier_score,
    calibration_data = calibration_data,
    is_well_calibrated = total_ece < 0.1
  )

  if (verbose) {
    cat("\n--- CALIBRATION ANALYSIS ---\n")
    cat(sprintf("ECE (Expected Calibration Error): %.3f\n", total_ece))
    cat(sprintf("MCE (Max Calibration Error): %.3f\n", mce))
    cat(sprintf("Brier Score: %.3f\n", brier_score))
    cat(sprintf("\nCalibration Status: %s\n",
                ifelse(total_ece < 0.1, "✓ WELL-CALIBRATED", "⚠ NEEDS IMPROVEMENT")))

    if (nrow(calibration_data) > 0) {
      cat("\nPer-bin analysis:\n")
      for (j in 1:nrow(calibration_data)) {
        row <- calibration_data[j, ]
        cat(sprintf("  Bin %d: Confidence=%.2f, Accuracy=%.2f, Gap=%.2f, N=%d\n",
                    row$bin, row$confidence, row$accuracy,
                    abs(row$confidence - row$accuracy), row$count))
      }
    }
  }

  return(result)
}

# ============================================================================
# PART 5: ROBUSTNESS SCORING
# ============================================================================

calculate_robustness_score <- function(results_by_scenario,
                                      scenario_names = c("perfect", "realistic", "degraded"),
                                      verbose = TRUE) {
  # Score how consistently well the system performs across scenarios
  # Higher = more robust (performs well even under degradation)

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              ROBUSTNESS ANALYSIS                           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  scenario_f1_scores <- list()

  for (i in seq_along(scenario_names)) {
    scenario <- scenario_names[i]
    results <- results_by_scenario[[i]]

    if (is.null(results) || length(results) == 0) next

    f1_scores <- sapply(results, \(x) x$f1_score)
    scenario_f1_scores[[scenario]] <- f1_scores

    if (verbose) {
      cat(sprintf("\n%s scenario:\n", toupper(scenario)))
      cat(sprintf("  Mean F1: %.3f (SD: %.3f)\n", mean(f1_scores), sd(f1_scores)))
      cat(sprintf("  Min F1: %.3f, Max F1: %.3f\n", min(f1_scores), max(f1_scores)))
      cat(sprintf("  Papers passing (F1 > 0.75): %d/%d\n",
                  sum(f1_scores > 0.75), length(f1_scores)))
    }
  }

  # Calculate consistency (variance ratio: should be low across scenarios)
  if (length(scenario_f1_scores) >= 2) {
    # Degradation ratio: how much does realistic drop vs perfect?
    if (!is.null(scenario_f1_scores$perfect) && !is.null(scenario_f1_scores$realistic)) {
      perfect_mean <- mean(scenario_f1_scores$perfect)
      realistic_mean <- mean(scenario_f1_scores$realistic)
      degradation <- (perfect_mean - realistic_mean) / perfect_mean

      # Robustness = 1 - degradation (perfect = 1, high drop = low score)
      robustness_metric <- 1 - degradation

      if (verbose) {
        cat(sprintf("\n--- ROBUSTNESS METRICS ---\n"))
        cat(sprintf("Perfect→Realistic degradation: %.1f%%\n", degradation * 100))
        cat(sprintf("Robustness score: %.3f (1.0 = no degradation)\n", robustness_metric))
      }

      return(list(
        robustness_score = robustness_metric,
        degradation_ratio = degradation,
        scenario_means = list(
          perfect = perfect_mean,
          realistic = realistic_mean
        )
      ))
    }
  }

  return(NULL)
}

# ============================================================================
# PART 6: COMPREHENSIVE METRICS REPORT
# ============================================================================

generate_advanced_metrics_report <- function(all_results, verbose = TRUE) {
  # Comprehensive quality report with all advanced metrics

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          ADVANCED METRICS QUALITY REPORT                  ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Extract metrics from all results
  f1_scores <- sapply(all_results, \(x) x$f1_score)
  composite_scores <- sapply(all_results, \(x) x$composite_score)
  precisions <- sapply(all_results, \(x) x$precision)
  recalls <- sapply(all_results, \(x) x$recall)

  # --- SECTION 1: BOOTSTRAP CONFIDENCE INTERVALS ---
  cat("\n[1/5] BOOTSTRAP CONFIDENCE INTERVALS (1000 resamples)\n")
  cat("──────────────────────────────────────────────────────────\n")

  f1_ci <- calculate_bootstrap_ci(f1_scores, n_bootstrap = 1000, ci = 0.95)
  composite_ci <- calculate_bootstrap_ci(composite_scores, n_bootstrap = 1000, ci = 0.95)

  cat(sprintf("\nF1-Score:\n"))
  cat(sprintf("  Mean: %.3f\n", f1_ci$mean))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", f1_ci$ci_lower, f1_ci$ci_upper))
  cat(sprintf("  Margin of error: ±%.3f\n", f1_ci$margin_of_error))

  cat(sprintf("\nComposite Score:\n"))
  cat(sprintf("  Mean: %.3f\n", composite_ci$mean))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", composite_ci$ci_lower, composite_ci$ci_upper))
  cat(sprintf("  Margin of error: ±%.3f\n", composite_ci$margin_of_error))

  # --- SECTION 2: BINOMIAL ACCURACY CI ---
  cat("\n[2/5] ACCURACY BOUNDS (Wilson Score Interval)\n")
  cat("──────────────────────────────────────────────────────────\n")

  # Assume papers passing (composite > 0.75) as "success"
  papers_passing <- sum(composite_scores > 0.75)
  total_papers <- length(composite_scores)

  accuracy_ci <- calculate_analytical_ci_binomial(papers_passing, total_papers, ci = 0.95)
  cat(sprintf("\nProduction-Ready Papers:\n"))
  cat(sprintf("  Observed: %d/%d (%.1f%%)\n", papers_passing, total_papers, accuracy_ci$estimate * 100))
  cat(sprintf("  95%% CI: [%.1f%%, %.1f%%]\n",
              accuracy_ci$ci_lower * 100, accuracy_ci$ci_upper * 100))

  # --- SECTION 3: STATISTICAL TESTS (if multiple scenarios exist) ---
  cat("\n[3/5] STATISTICAL SIGNIFICANCE TESTS\n")
  cat("──────────────────────────────────────────────────────────\n")

  # Group by scenario
  scenarios <- unique(sapply(all_results, \(x) x$scenario))

  if (length(scenarios) >= 2) {
    realistic_results <- all_results[sapply(all_results, \(x) x$scenario == "realistic")]
    perfect_results <- all_results[sapply(all_results, \(x) x$scenario == "perfect")]

    if (length(realistic_results) > 0 && length(perfect_results) > 0) {
      realistic_f1 <- sapply(realistic_results, \(x) x$f1_score)
      perfect_f1 <- sapply(perfect_results, \(x) x$f1_score)

      sig_test <- test_metric_significance(perfect_f1, realistic_f1,
                                          metric_name = "F1-Score (Perfect vs Realistic)",
                                          verbose = TRUE)
    }
  } else {
    cat("\n(Skipped: only one scenario type in dataset)\n")
  }

  # --- SECTION 4: CALIBRATION ANALYSIS ---
  cat("\n[4/5] CONFIDENCE SCORE CALIBRATION\n")
  cat("──────────────────────────────────────────────────────────\n")

  # Use composite scores as predicted confidence
  # Use pass/fail (>0.75) as actual outcome
  predicted_conf <- composite_scores
  actual_labels <- as.numeric(composite_scores > 0.75)

  calibration <- analyze_calibration(predicted_conf, actual_labels, n_bins = 5, verbose = TRUE)

  # --- SECTION 5: ROBUSTNESS SCORING ---
  cat("\n[5/5] ROBUSTNESS ASSESSMENT\n")
  cat("──────────────────────────────────────────────────────────\n")

  if (length(scenarios) >= 2) {
    results_by_scenario <- list()
    for (scenario in scenarios) {
      results_by_scenario[[length(results_by_scenario) + 1]] <-
        all_results[sapply(all_results, \(x) x$scenario == scenario)]
    }

    robustness <- calculate_robustness_score(results_by_scenario, scenarios, verbose = TRUE)
  } else {
    cat("\n(Skipped: only one scenario type in dataset)\n")
    robustness <- NULL
  }

  # === FINAL RECOMMENDATIONS ===
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║                  RECOMMENDATIONS                           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Recommendation 1: Confidence intervals
  if (f1_ci$margin_of_error < 0.10) {
    cat("\n✓ Precise estimates: F1 margin of error < 0.10\n")
    cat("  → Results are reliable for decision-making\n")
  } else {
    cat("\n⚠ High uncertainty in estimates (margin > 0.10)\n")
    cat("  → Consider testing on more papers for better precision\n")
  }

  # Recommendation 2: Pass rate
  if (papers_passing / total_papers >= 0.80) {
    cat("\n✓ High pass rate: ", sprintf("%.1f%% of papers ready", papers_passing / total_papers * 100), "\n")
    cat("  → System is production-ready\n")
  } else if (papers_passing / total_papers >= 0.60) {
    cat("\n⚠ Moderate pass rate: ", sprintf("%.1f%% of papers ready", papers_passing / total_papers * 100), "\n")
    cat("  → Additional refinement recommended\n")
  } else {
    cat("\n✗ Low pass rate: ", sprintf("%.1f%% of papers ready", papers_passing / total_papers * 100), "\n")
    cat("  → Significant improvement needed\n")
  }

  # Recommendation 3: Calibration
  if (calibration$is_well_calibrated) {
    cat("\n✓ Well-calibrated confidence scores\n")
    cat("  → Confidence estimates are trustworthy\n")
  } else {
    cat("\n⚠ Poorly calibrated confidence scores\n")
    cat("  → Model confidence may not reflect true accuracy\n")
  }

  # Recommendation 4: Robustness
  if (!is.null(robustness) && robustness$robustness_score > 0.80) {
    cat("\n✓ High robustness: ", sprintf("%.1f%% of perfect performance retained", robustness$robustness_score * 100), "\n")
    cat("  → System handles degradation well\n")
  }

  cat("\n✓ Report complete.\n")

  return(list(
    f1_ci = f1_ci,
    composite_ci = composite_ci,
    accuracy_ci = accuracy_ci,
    calibration = calibration,
    robustness = robustness,
    summary = list(
      n_papers = total_papers,
      n_passing = papers_passing,
      mean_f1 = mean(f1_scores),
      mean_composite = mean(composite_scores)
    )
  ))
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Advanced Metrics Refinement module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. calculate_bootstrap_ci()              - Bootstrap confidence intervals\n")
cat("  2. calculate_analytical_ci_binomial()    - Analytical CIs for accuracy\n")
cat("  3. test_metric_significance()            - Statistical significance tests\n")
cat("  4. analyze_calibration()                 - Confidence score calibration\n")
cat("  5. calculate_robustness_score()          - Robustness across scenarios\n")
cat("  6. generate_advanced_metrics_report()    - Complete quality report\n")
cat("\nRun: report <- generate_advanced_metrics_report(all_results)\n")
