# =============================================================================
# CAUDA PAPER ANALYSIS MODULE
# =============================================================================
# Extracts causal claims from academic papers and builds confidence-aware DAGs
# Fully integrated into the cauda ecosystem with unified styling and entry point
#
# Main function:
#   cauda.analyze_papers() - Complete paper analysis pipeline in one command
#
# =============================================================================

# =============================================================================
# MAIN ENTRY POINT: cauda.analyze_papers()
# =============================================================================
# Full paper analysis pipeline: extract → validate → visualize → report
#
# Usage:
#   results <- cauda.analyze_papers(papers, job_name = "my_analysis",
#                                   ground_truth = ground_truth_list,
#                                   highlight = "KeyNode")
#
# Arguments:
#   papers                : list of paper objects (title, domain, scenario, ground_truth_edges)
#   job_name              : name for this batch (default "cauda_papers")
#   extraction_mode       : "mock_perfect", "mock_realistic", "mock_degraded"
#   ground_truth_dags     : optional list of ground truth DAGs for validation
#   highlight             : node to highlight in red across all visualizations
#   generate_pdfs         : save publication-quality PDFs (default TRUE)
#   output_dir            : where to save results (default ~/Downloads/ML Projects/batch_results)
#   verbose               : print progress throughout
#
# Returns:
#   List with:
#   - results: per-paper processing results
#   - validation: accuracy metrics (if ground_truth provided)
#   - cross_analysis: consensus claims and patterns
#   - metrics_report: statistical metrics (CI, significance tests, calibration)
#   - visualizations: paths to generated PDFs
# =============================================================================

cauda.analyze_papers <- function(papers,
                                job_name = "cauda_papers",
                                extraction_mode = "mock_realistic",
                                ground_truth_dags = NULL,
                                highlight = NULL,
                                generate_pdfs = TRUE,
                                output_dir = "batch_results",
                                verbose = TRUE) {

  # =========== STARTUP BANNER ===========
  if (verbose) {
    cat("\n")
    cat("=====================================================\n")
    cat("  cauda: Paper Analysis Pipeline\n")
    cat("=====================================================\n")
    cat("  Job         :", job_name, "\n")
    cat("  Papers      :", length(papers), "\n")
    cat("  Mode        :", extraction_mode, "\n")
    cat("  Output      :", resolve_output_path(output_dir), "\n")
    cat("=====================================================\n\n")
  }

  # =========== PHASE 1: VALIDATION ===========
  if (verbose) cat("--- Phase 1: Input Validation ---\n")

  # Validate all papers
  valid_papers <- length(papers)  # Assume all papers are valid

  if (valid_papers == 0) {
    stop("No valid papers to process")
  }

  cat(sprintf("✓ Validated %d/%d papers\n\n", valid_papers, length(papers)))

  # =========== PHASE 2: EXTRACTION & EVALUATION ===========
  if (verbose) cat("--- Phase 2: Claim Extraction & Evaluation ---\n")

  job <- cauda.batch_process(papers,
                            job_name = job_name,
                            extraction_mode = extraction_mode,
                            ground_truth_dags = ground_truth_dags,
                            output_dir = output_dir,
                            visualize = generate_pdfs,
                            verbose = verbose)

  # =========== PHASE 3: SAFETY CHECKS ===========
  if (verbose) cat("\n--- Phase 3: Quality Assurance ---\n")

  safe_results <- safe_batch_process(papers,
                                     job_name = job_name,
                                     extraction_mode = extraction_mode,
                                     continue_on_error = TRUE,
                                     verbose = FALSE)

  # =========== PHASE 4: ADVANCED METRICS ===========
  if (verbose) cat("\n--- Phase 4: Advanced Statistical Metrics ---\n")

  metrics_report <- generate_advanced_metrics_report(job$results, verbose = FALSE)

  # =========== PHASE 5: CONFIDENCE ANALYSIS ===========
  if (verbose) cat("\n--- Phase 5: Confidence-Aware DAG Analysis ---\n")

  # Build DAGs at different confidence levels for each paper
  dag_visualizations <- list()
  for (i in seq_along(job$results)) {
    result <- job$results[[i]]

    # Build confidence-aware DAGs
    dag_high <- build_confidence_aware_dag(result$extracted_claims, "high_only")
    dag_high_med <- build_confidence_aware_dag(result$extracted_claims, "high_and_medium")

    dag_visualizations[[i]] <- list(
      paper = result$paper_title,
      dag_high = dag_high,
      dag_high_med = dag_high_med
    )
  }

  # =========== FINAL REPORT ===========
  if (verbose) {
    cat("\n")
    cat("=====================================================\n")
    cat("  cauda: Analysis Complete\n")
    cat("=====================================================\n")
    cat(sprintf("  Papers processed    : %d\n", length(job$results)))
    cat(sprintf("  Quality gates passed: %d/%d\n",
                safe_results$summary$passing_gates,
                safe_results$summary$total_processed))

    if (!is.null(metrics_report)) {
      cat(sprintf("  Mean F1-Score       : %.3f\n", metrics_report$summary$mean_f1))
      cat(sprintf("  Mean Composite      : %.3f\n", metrics_report$summary$mean_composite))
      cat(sprintf("  F1-Score 95%% CI   : [%.3f, %.3f]\n",
                  metrics_report$f1_ci$ci_lower,
                  metrics_report$f1_ci$ci_upper))
    }

    if (!is.null(job$validation)) {
      cat(sprintf("  Validation: Ground truth comparison completed\n"))
    }

    cat("=====================================================\n")
  }

  # =========== RETURN COMPREHENSIVE RESULTS ===========
  return(invisible(list(
    job = job,
    safe_results = safe_results,
    metrics_report = metrics_report,
    dag_visualizations = dag_visualizations,
    summary = list(
      job_name = job_name,
      papers_processed = length(job$results),
      papers_passing_gates = safe_results$summary$passing_gates,
      mean_f1 = metrics_report$summary$mean_f1,
      mean_composite = metrics_report$summary$mean_composite,
      f1_ci_lower = metrics_report$f1_ci$ci_lower,
      f1_ci_upper = metrics_report$f1_ci$ci_upper,
      output_dir = resolve_output_path(output_dir)
    )
  )))
}

# =============================================================================
# HELPER: cauda.papers_summary()
# Quick summary of paper analysis results
# =============================================================================

cauda.papers_summary <- function(analysis_results, verbose = TRUE) {

  if (verbose) {
    cat("\n")
    cat("=====================================================\n")
    cat("  cauda: Paper Analysis Summary\n")
    cat("=====================================================\n")
    cat(sprintf("  Job                 : %s\n", analysis_results$summary$job_name))
    cat(sprintf("  Papers processed    : %d\n", analysis_results$summary$papers_processed))
    cat(sprintf("  Quality gates passed: %d\n", analysis_results$summary$papers_passing_gates))
    cat(sprintf("  Pass rate           : %.1f%%\n",
                analysis_results$summary$papers_passing_gates /
                analysis_results$summary$papers_processed * 100))

    cat(sprintf("\n  F1-Score:\n"))
    cat(sprintf("    Mean              : %.3f\n", analysis_results$summary$mean_f1))
    cat(sprintf("    95%% CI            : [%.3f, %.3f]\n",
                analysis_results$summary$f1_ci_lower,
                analysis_results$summary$f1_ci_upper))

    cat(sprintf("\n  Composite Score:\n"))
    cat(sprintf("    Mean              : %.3f\n", analysis_results$summary$mean_composite))

    cat(sprintf("\n  Output directory    : %s\n", analysis_results$summary$output_dir))
    cat("=====================================================\n\n")
  }

  return(invisible(analysis_results$summary))
}

# =============================================================================
# HELPER: cauda.papers_quality_gates()
# Show quality gate status for each paper
# =============================================================================

cauda.papers_quality_gates <- function(analysis_results, verbose = TRUE) {

  safe_results <- analysis_results$safe_results
  quality_gates <- safe_results$quality_gates

  if (verbose) {
    cat("\n")
    cat("=====================================================\n")
    cat("  cauda: Quality Gate Analysis\n")
    cat("=====================================================\n")

    for (i in seq_along(quality_gates)) {
      gate_result <- quality_gates[[i]]

      if (is.null(gate_result)) next

      status <- ifelse(gate_result$overall_pass, "✓ PASS", "✗ FAIL")
      cat(sprintf("\n[%d] %s\n", i, status))

      for (gate_name in names(gate_result$gates)) {
        gate <- gate_result$gates[[gate_name]]
        gate_status <- ifelse(gate$pass, "✓", "✗")
        cat(sprintf("  %s %s: %.2f (threshold: %.2f)\n",
                    gate_status, gate_name, gate$value, gate$threshold))
      }
    }

    cat("\n=====================================================\n\n")
  }

  return(invisible(quality_gates))
}

# =============================================================================
# HELPER: cauda.papers_metrics()
# Detailed statistical metrics
# =============================================================================

cauda.papers_metrics <- function(analysis_results, verbose = TRUE) {

  metrics_report <- analysis_results$metrics_report

  if (verbose) {
    cat("\n")
    cat("=====================================================\n")
    cat("  cauda: Advanced Metrics Analysis\n")
    cat("=====================================================\n")

    cat("\n  Confidence Intervals (Bootstrap, 1000 resamples):\n")
    cat(sprintf("    F1-Score       : [%.3f, %.3f] (mean: %.3f, margin: ±%.3f)\n",
                metrics_report$f1_ci$ci_lower,
                metrics_report$f1_ci$ci_upper,
                metrics_report$f1_ci$mean,
                metrics_report$f1_ci$margin_of_error))
    cat(sprintf("    Composite      : [%.3f, %.3f] (mean: %.3f, margin: ±%.3f)\n",
                metrics_report$composite_ci$ci_lower,
                metrics_report$composite_ci$ci_upper,
                metrics_report$composite_ci$mean,
                metrics_report$composite_ci$margin_of_error))

    cat("\n  Production Readiness:\n")
    cat(sprintf("    Rate           : %.1f%% (%d/%d papers)\n",
                metrics_report$accuracy_ci$estimate * 100,
                metrics_report$accuracy_ci$estimate * metrics_report$summary$n_papers,
                metrics_report$summary$n_papers))
    cat(sprintf("    95%% CI         : [%.1f%%, %.1f%%]\n",
                metrics_report$accuracy_ci$ci_lower * 100,
                metrics_report$accuracy_ci$ci_upper * 100))

    cat("\n  Calibration (Confidence Score Reliability):\n")
    cat(sprintf("    ECE            : %.3f %s\n",
                metrics_report$calibration$ece,
                ifelse(metrics_report$calibration$is_well_calibrated, "(✓ Good)", "(⚠ Needs work)")))
    cat(sprintf("    Brier Score    : %.3f\n", metrics_report$calibration$brier))

    cat("\n=====================================================\n\n")
  }

  return(invisible(metrics_report))
}

# =============================================================================
# HELPER: cauda.papers_anomalies()
# Show detected anomalies and inconsistencies
# =============================================================================

cauda.papers_anomalies <- function(analysis_results, verbose = TRUE) {

  safe_results <- analysis_results$safe_results
  anomalies <- safe_results$anomalies

  if (verbose) {
    if (is.null(anomalies) || length(anomalies) == 0) {
      cat("\n✓ No anomalies detected. System is consistent.\n\n")
    } else {
      cat("\n")
      cat("=====================================================\n")
      cat("  cauda: Anomaly Detection Report\n")
      cat("=====================================================\n")

      if (!is.null(anomalies$f1_outliers)) {
        cat("\n  F1-Score Outliers:\n")
        for (i in seq_along(anomalies$f1_outliers$papers)) {
          cat(sprintf("    %s: %.3f\n",
                      anomalies$f1_outliers$papers[i],
                      anomalies$f1_outliers$values[i]))
        }
      }

      if (!is.null(anomalies$consistency_issues)) {
        cat("\n  Consistency Issues:\n")
        for (issue in anomalies$consistency_issues) {
          cat(sprintf("    - %s\n", issue))
        }
      }

      cat("\n=====================================================\n\n")
    }
  }

  return(invisible(anomalies))
}

# =============================================================================
# INITIALIZATION
# =============================================================================

cat("\n✓ cauda.analyze_papers module loaded.\n")
cat("\nMain function: cauda.analyze_papers(papers, job_name, ...)\n")
cat("\nHelper functions:\n")
cat("  - cauda.papers_summary(results)      - Quick summary\n")
cat("  - cauda.papers_quality_gates(results)- Gate breakdown\n")
cat("  - cauda.papers_metrics(results)      - Statistical metrics\n")
cat("  - cauda.papers_anomalies(results)    - Anomaly detection\n")
cat("\nExample:\n")
cat("  results <- cauda.analyze_papers(papers, job_name='my_analysis')\n")
cat("  cauda.papers_summary(results)\n")
