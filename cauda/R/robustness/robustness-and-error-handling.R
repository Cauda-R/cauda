# ============================================================================
# PHASE 3: ROBUSTNESS & ERROR HANDLING MODULE
# ============================================================================
# Comprehensive error detection, edge case handling, data validation
# Features:
#   - Input validation (papers, claims, DAGs)
#   - Anomaly detection (outliers, missing data)
#   - Error recovery (graceful fallbacks)
#   - Quality gates (minimum thresholds)
#   - Detailed error reporting
#   - System health checks

# ============================================================================
# PART 1: COMPREHENSIVE INPUT VALIDATION
# ============================================================================

validate_paper_structure <- function(paper, verbose = TRUE) {
  # Validate that paper object has all required fields
  # Returns: list with is_valid, errors, warnings

  errors <- character()
  warnings <- character()

  # Check required fields
  required_fields <- c("title", "domain", "ground_truth_edges", "scenario")
  missing <- setdiff(required_fields, names(paper))

  if (length(missing) > 0) {
    errors <- c(errors, sprintf("Missing required fields: %s", paste(missing, collapse = ", ")))
  }

  # Validate field types and content
  if (!is.null(paper$title)) {
    if (!is.character(paper$title) || nchar(paper$title) == 0) {
      errors <- c(errors, "Title must be non-empty string")
    }
  }

  if (!is.null(paper$domain)) {
    if (!is.character(paper$domain) || nchar(paper$domain) == 0) {
      errors <- c(errors, "Domain must be non-empty string")
    }
  }

  if (!is.null(paper$ground_truth_edges)) {
    if (!is.data.frame(paper$ground_truth_edges)) {
      errors <- c(errors, "ground_truth_edges must be a data frame")
    } else {
      if (!all(c("source", "target") %in% names(paper$ground_truth_edges))) {
        errors <- c(errors, "ground_truth_edges must have 'source' and 'target' columns")
      }
      if (nrow(paper$ground_truth_edges) == 0) {
        warnings <- c(warnings, "Paper has no ground truth edges")
      }
    }
  }

  # Check for suspicious values
  if (!is.null(paper$title) && nchar(paper$title) > 500) {
    warnings <- c(warnings, "Title is very long (>500 characters)")
  }

  result <- list(
    is_valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )

  if (verbose) {
    cat(sprintf("\n--- Validating Paper: %s ---\n", ifelse(!is.null(paper$title), paper$title, "UNKNOWN")))

    if (length(errors) == 0) {
      cat("✓ VALID\n")
    } else {
      cat("✗ INVALID:\n")
      for (err in errors) {
        cat(sprintf("  ✗ %s\n", err))
      }
    }

    if (length(warnings) > 0) {
      cat("⚠ Warnings:\n")
      for (warn in warnings) {
        cat(sprintf("  ⚠ %s\n", warn))
      }
    }
  }

  return(result)
}

validate_extracted_claims <- function(claims_df, paper_title = NULL, verbose = TRUE) {
  # Validate extracted claims dataframe
  # Returns: validation result with error count, data quality metrics

  errors <- character()
  warnings <- character()

  if (!is.data.frame(claims_df)) {
    return(list(
      is_valid = FALSE,
      errors = c("Claims must be a data frame"),
      warnings = character()
    ))
  }

  # Check required columns
  required_cols <- c("source", "target", "confidence", "pathway", "strength")
  missing_cols <- setdiff(required_cols, names(claims_df))

  if (length(missing_cols) > 0) {
    errors <- c(errors, sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  if (nrow(claims_df) == 0) {
    warnings <- c(warnings, "No claims extracted from paper")
  }

  # Check data quality
  na_counts <- colSums(is.na(claims_df[required_cols]))
  na_cols <- names(na_counts[na_counts > 0])

  if (length(na_cols) > 0) {
    for (col in na_cols) {
      na_pct <- na_counts[col] / nrow(claims_df) * 100
      if (na_pct > 20) {
        errors <- c(errors, sprintf("%s: %.1f%% missing values", col, na_pct))
      } else if (na_pct > 5) {
        warnings <- c(warnings, sprintf("%s: %.1f%% missing values", col, na_pct))
      }
    }
  }

  # Check confidence values
  if ("confidence" %in% names(claims_df)) {
    invalid_conf <- !claims_df$confidence %in% c("high", "medium", "low", NA)
    if (sum(invalid_conf) > 0) {
      errors <- c(errors, sprintf("Invalid confidence values in %d rows", sum(invalid_conf)))
    }

    # Check confidence distribution
    conf_counts <- table(claims_df$confidence)
    high_pct <- conf_counts["high"] / nrow(claims_df) * 100
    if (high_pct < 20) {
      warnings <- c(warnings, sprintf("Low proportion of high-confidence claims (%.1f%%)", high_pct))
    }
  }

  # Check for duplicate edges
  if ("source" %in% names(claims_df) && "target" %in% names(claims_df)) {
    edges <- paste(claims_df$source, "->", claims_df$target)
    duplicates <- sum(duplicated(edges))
    if (duplicates > 0) {
      warnings <- c(warnings, sprintf("Found %d duplicate edges", duplicates))
    }
  }

  result <- list(
    is_valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    n_claims = nrow(claims_df),
    quality_score = 1.0 - (length(errors) * 0.2 + length(warnings) * 0.05)
  )

  if (verbose) {
    cat(sprintf("\n--- Validating Claims: %s ---\n",
                ifelse(!is.null(paper_title), paper_title, "UNKNOWN")))
    cat(sprintf("Total claims: %d\n", nrow(claims_df)))

    if (length(errors) == 0) {
      cat("✓ VALID\n")
    } else {
      cat("✗ INVALID:\n")
      for (err in errors) {
        cat(sprintf("  ✗ %s\n", err))
      }
    }

    if (length(warnings) > 0) {
      cat("⚠ Warnings:\n")
      for (warn in warnings) {
        cat(sprintf("  ⚠ %s\n", warn))
      }
    }

    cat(sprintf("Quality score: %.2f/1.00\n", result$quality_score))
  }

  return(result)
}

# ============================================================================
# PART 2: ANOMALY DETECTION
# ============================================================================

detect_anomalies <- function(results_list, verbose = TRUE) {
  # Detect unusual patterns in batch results
  # Returns: list of detected anomalies

  anomalies <- list()

  if (length(results_list) == 0) return(NULL)

  # Extract metrics
  f1_scores <- sapply(results_list, \(x) x$f1_score)
  composite_scores <- sapply(results_list, \(x) x$composite_score)
  claim_counts <- sapply(results_list, \(x) x$num_claims_extracted)

  # --- OUTLIER DETECTION (using IQR method) ---
  detect_outliers_iqr <- function(values, name) {
    Q1 <- quantile(values, 0.25)
    Q3 <- quantile(values, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    outlier_idx <- which(values < lower_bound | values > upper_bound)
    return(outlier_idx)
  }

  f1_outliers <- detect_outliers_iqr(f1_scores, "F1")
  composite_outliers <- detect_outliers_iqr(composite_scores, "Composite")
  claims_outliers <- detect_outliers_iqr(claim_counts, "Claims")

  # --- CONSISTENCY CHECKS ---
  consistency_issues <- character()

  # Check if F1 and composite are aligned
  f1_composite_corr <- cor(f1_scores, composite_scores)
  if (f1_composite_corr < 0.7) {
    consistency_issues <- c(consistency_issues,
                           sprintf("Low correlation between F1 and Composite (r=%.2f)", f1_composite_corr))
  }

  # Check if any paper has extremely high or low claims
  mean_claims <- mean(claim_counts)
  sd_claims <- sd(claim_counts)
  extreme_claims <- which(claim_counts < mean_claims - 2*sd_claims |
                          claim_counts > mean_claims + 2*sd_claims)

  # --- COMPILE ANOMALIES ---
  if (length(f1_outliers) > 0) {
    anomalies$f1_outliers <- list(
      papers = names(results_list)[f1_outliers],
      values = f1_scores[f1_outliers],
      severity = "medium"
    )
  }

  if (length(composite_outliers) > 0) {
    anomalies$composite_outliers <- list(
      papers = names(results_list)[composite_outliers],
      values = composite_scores[composite_outliers],
      severity = "medium"
    )
  }

  if (length(extreme_claims) > 0) {
    anomalies$extreme_claim_counts <- list(
      papers = names(results_list)[extreme_claims],
      values = claim_counts[extreme_claims],
      mean = mean_claims,
      severity = "low"
    )
  }

  if (length(consistency_issues) > 0) {
    anomalies$consistency_issues <- consistency_issues
  }

  if (verbose && length(anomalies) > 0) {
    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║                  ANOMALY DETECTION REPORT                 ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    if (!is.null(anomalies$f1_outliers)) {
      cat("\n⚠ F1-Score Outliers (unusual performance):\n")
      for (i in seq_along(anomalies$f1_outliers$papers)) {
        cat(sprintf("  %s: F1=%.3f\n",
                    anomalies$f1_outliers$papers[i],
                    anomalies$f1_outliers$values[i]))
      }
    }

    if (!is.null(anomalies$composite_outliers)) {
      cat("\n⚠ Composite Score Outliers:\n")
      for (i in seq_along(anomalies$composite_outliers$papers)) {
        cat(sprintf("  %s: Composite=%.3f\n",
                    anomalies$composite_outliers$papers[i],
                    anomalies$composite_outliers$values[i]))
      }
    }

    if (!is.null(anomalies$extreme_claim_counts)) {
      cat("\n⚠ Extreme Claim Counts (mean=", round(anomalies$extreme_claim_counts$mean, 1), "):\n")
      for (i in seq_along(anomalies$extreme_claim_counts$papers)) {
        cat(sprintf("  %s: %d claims\n",
                    anomalies$extreme_claim_counts$papers[i],
                    anomalies$extreme_claim_counts$values[i]))
      }
    }

    if (!is.null(anomalies$consistency_issues)) {
      cat("\n⚠ Consistency Issues:\n")
      for (issue in anomalies$consistency_issues) {
        cat(sprintf("  %s\n", issue))
      }
    }

    if (length(anomalies) == 0) {
      cat("\n✓ No anomalies detected. System is consistent.\n")
    }
  }

  return(anomalies)
}

# ============================================================================
# PART 3: QUALITY GATES (PASS/FAIL CRITERIA)
# ============================================================================

check_quality_gates <- function(result, thresholds = NULL, verbose = TRUE) {
  # Verify paper meets production quality standards
  # Returns: pass/fail status and breakdown by criterion

  if (is.null(thresholds)) {
    thresholds <- list(
      min_f1 = 0.70,
      min_composite = 0.75,
      min_high_conf_claims = 3,
      max_na_percentage = 0.10,
      min_claim_count = 5
    )
  }

  gates <- list(
    f1_threshold = list(
      value = result$f1_score,
      threshold = thresholds$min_f1,
      pass = result$f1_score >= thresholds$min_f1
    ),
    composite_threshold = list(
      value = result$composite_score,
      threshold = thresholds$min_composite,
      pass = result$composite_score >= thresholds$min_composite
    ),
    high_conf_claims = list(
      value = result$high_conf_count,
      threshold = thresholds$min_high_conf_claims,
      pass = result$high_conf_count >= thresholds$min_high_conf_claims
    ),
    claim_count = list(
      value = result$num_claims_extracted,
      threshold = thresholds$min_claim_count,
      pass = result$num_claims_extracted >= thresholds$min_claim_count
    )
  )

  overall_pass <- all(sapply(gates, \(x) x$pass))

  if (verbose) {
    cat(sprintf("\n--- QUALITY GATES: %s ---\n", result$paper_title))
    cat(sprintf("Overall Status: %s\n\n", ifelse(overall_pass, "✓ PASS", "✗ FAIL")))

    for (gate_name in names(gates)) {
      gate <- gates[[gate_name]]
      status <- ifelse(gate$pass, "✓", "✗")
      cat(sprintf("%s %s: %.2f (threshold: %.2f)\n",
                  status, gate_name, gate$value, gate$threshold))
    }
  }

  return(list(
    overall_pass = overall_pass,
    gates = gates,
    passing_gates = sum(sapply(gates, \(x) x$pass)),
    total_gates = length(gates)
  ))
}

# ============================================================================
# PART 4: ERROR RECOVERY & FALLBACK MECHANISMS
# ============================================================================

safe_batch_process <- function(papers, job_name = "safe_batch",
                               extraction_mode = "mock_realistic",
                               output_dir = "batch_results",
                               continue_on_error = TRUE,
                               verbose = TRUE) {
  # Robust batch processing with error recovery
  # Validates all inputs, catches errors, provides detailed reporting

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║         SAFE BATCH PROCESS - ERROR-RESILIENT MODE         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Phase 1: INPUT VALIDATION
  cat("\n[PHASE 1/3] Input Validation...\n")

  if (!is.list(papers) || length(papers) == 0) {
    stop("Papers must be a non-empty list")
  }

  valid_papers <- 0
  invalid_papers <- character()

  for (i in seq_along(papers)) {
    validation <- validate_paper_structure(papers[[i]], verbose = FALSE)
    if (validation$is_valid) {
      valid_papers <- valid_papers + 1
    } else {
      invalid_papers <- c(invalid_papers, papers[[i]]$title)
    }
  }

  cat(sprintf("✓ Valid papers: %d/%d\n", valid_papers, length(papers)))

  if (valid_papers == 0) {
    stop("No valid papers to process")
  }

  if (length(invalid_papers) > 0 && !continue_on_error) {
    stop(sprintf("Invalid papers found: %s", paste(invalid_papers, collapse = ", ")))
  }

  # Phase 2: BATCH PROCESSING WITH ERROR HANDLING
  cat("\n[PHASE 2/3] Processing Papers...\n")

  results <- list()
  failed_papers <- list()
  quality_gates_results <- list()

  for (i in seq_along(papers)) {
    paper <- papers[[i]]

    tryCatch(
      {
        cat(sprintf("  [%d/%d] %s... ", i, length(papers), substr(paper$title, 1, 30)))

        # Process paper
        result <- process_single_paper(paper, extraction_mode, verbose = FALSE)

        if (is.null(result)) {
          stop("Processing returned NULL")
        }

        # Validate extracted claims
        claims_validation <- validate_extracted_claims(result$extracted_claims,
                                                       paper$title, verbose = FALSE)

        if (!claims_validation$is_valid && !continue_on_error) {
          stop(sprintf("Claims validation failed: %s", paste(claims_validation$errors, collapse = "; ")))
        }

        # Check quality gates
        gate_result <- check_quality_gates(result, verbose = FALSE)
        quality_gates_results[[i]] <- gate_result

        results[[i]] <- result
        cat(sprintf("%s\n", ifelse(gate_result$overall_pass, "✓", "⚠")))
      },
      error = function(e) {
        cat(sprintf("✗ ERROR\n"))
        failed_papers[[length(failed_papers) + 1]] <<- list(
          title = paper$title,
          error = e$message
        )

        if (!continue_on_error) {
          stop(e$message)
        }
      }
    )
  }

  # Phase 3: QUALITY ASSURANCE
  cat("\n[PHASE 3/3] Quality Assurance & Anomaly Detection...\n")

  anomalies <- detect_anomalies(results, verbose = TRUE)

  # Summary report
  passing_gates <- sum(sapply(quality_gates_results, \(x) x$overall_pass))

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║                    SAFETY SUMMARY                          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  cat(sprintf("\n✓ Papers processed: %d/%d\n", length(results), length(papers)))
  cat(sprintf("⚠ Papers failed: %d\n", length(failed_papers)))
  cat(sprintf("✓ Papers passing quality gates: %d/%d\n", passing_gates, length(results)))

  if (length(failed_papers) > 0) {
    cat("\n⚠ Failed papers:\n")
    for (fp in failed_papers) {
      cat(sprintf("  - %s: %s\n", fp$title, fp$error))
    }
  }

  return(list(
    results = results,
    failed_papers = failed_papers,
    quality_gates = quality_gates_results,
    anomalies = anomalies,
    summary = list(
      total_processed = length(results),
      total_failed = length(failed_papers),
      passing_gates = passing_gates
    )
  ))
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Robustness & Error Handling module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. validate_paper_structure(paper)      - Validate paper object\n")
cat("  2. validate_extracted_claims(claims)    - Validate claims data\n")
cat("  3. detect_anomalies(results)            - Find unusual patterns\n")
cat("  4. check_quality_gates(result)          - Check production readiness\n")
cat("  5. safe_batch_process(papers)           - Error-resilient processing\n")
cat("\nRun: job <- safe_batch_process(papers, job_name='safe_job')\n")
