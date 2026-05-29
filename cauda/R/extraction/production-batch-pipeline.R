# ============================================================================
# PRODUCTION-READY BATCH PROCESSING PIPELINE
# ============================================================================
# End-to-end automated processing of multiple papers
# Extracts claims → builds DAGs → evaluates → generates reports
# Modular design: swap mock extraction for real API when ready

library(bnlearn)

# ============================================================================
# PART 1: BATCH JOB CONFIGURATION
# ============================================================================

# Define a batch job
create_batch_job <- function(job_name,
                            papers,  # list of paper objects
                            extraction_mode = "mock_realistic",  # "mock_perfect", "mock_realistic", "mock_degraded"
                            output_dir = "batch_results",
                            verbose = TRUE) {
  # papers: list of paper objects with title, domain, ground_truth_edges, etc.

  job <- list(
    job_name = job_name,
    created_at = Sys.time(),
    papers = papers,
    extraction_mode = extraction_mode,
    output_dir = output_dir,
    verbose = verbose,
    results = list()  # Will populate during processing
  )

  class(job) <- "batch_job"

  if (verbose) {
    cat(sprintf("\n✓ Batch job created: %s\n", job_name))
    cat(sprintf("  Papers: %d\n", length(papers)))
    cat(sprintf("  Extraction mode: %s\n", extraction_mode))
    cat(sprintf("  Output directory: %s\n", output_dir))
  }

  return(job)
}

# ============================================================================
# PART 2: EXTRACTION ADAPTER (swap mock ↔ real API)
# ============================================================================

extract_claims_batch <- function(paper, extraction_mode = "mock_realistic") {
  # Adapter function: allows easy swapping between mock and real extraction

  if (extraction_mode == "mock_perfect") {
    return(extract_mock_claims_v2(paper, scenario = "perfect"))

  } else if (extraction_mode == "mock_realistic") {
    return(extract_mock_claims_v2(paper, scenario = "realistic"))

  } else if (extraction_mode == "mock_degraded") {
    return(extract_mock_claims_v2(paper, scenario = "degraded"))

  } else if (extraction_mode == "api_claude") {
    # FUTURE: swap in real extraction here
    # return(extract_claims_claude(paper$text, domain = paper$domain))
    stop("API extraction not yet configured. Set ANTHROPIC_API_KEY and uncomment in extract_claims_batch()")

  } else if (extraction_mode == "api_openai") {
    # FUTURE: swap in OpenAI extraction here
    # return(extract_claims(paper$text, domain = paper$domain, model = "gpt-4o-mini"))
    stop("API extraction not yet configured. Set OPENAI_API_KEY and uncomment in extract_claims_batch()")

  } else {
    stop(sprintf("Unknown extraction mode: %s", extraction_mode))
  }
}

# ============================================================================
# PART 3: SINGLE PAPER PROCESSING
# ============================================================================

process_single_paper <- function(paper, extraction_mode = "mock_realistic",
                                verbose = TRUE) {
  # Full pipeline for one paper:
  # 1. Extract claims
  # 2. Evaluate claims
  # 3. Build DAGs at different confidence levels
  # 4. Analyze confidence
  # Returns: comprehensive result object

  if (verbose) {
    cat(sprintf("\n▸ Processing: %s\n", paper$title))
  }

  # Step 1: Extract claims
  if (verbose) cat("  [1/4] Extracting claims...")
  extracted <- extract_claims_batch(paper, extraction_mode)
  if (verbose) cat(" ✓\n")

  # Step 2: Evaluate
  if (verbose) cat("  [2/4] Evaluating...")
  eval_result <- evaluate_single_paper_v2(paper, extracted, verbose = FALSE)
  if (verbose) cat(" ✓\n")

  # Step 3: Build DAGs at different confidence levels
  if (verbose) cat("  [3/4] Building DAGs...")
  dag_high <- build_confidence_aware_dag(extracted, "high_only")
  dag_high_med <- build_confidence_aware_dag(extracted, "high_and_medium")
  dag_all <- build_confidence_aware_dag(extracted, "all")
  if (verbose) cat(" ✓\n")

  # Step 4: Confidence analysis
  if (verbose) cat("  [4/4] Analyzing confidence...")
  conf_analysis <- analyze_node_confidence(extracted, verbose = FALSE)
  if (verbose) cat(" ✓\n")

  # Compile results
  result <- list(
    paper_title = paper$title,
    domain = paper$domain,
    scenario = paper$scenario,
    extraction_mode = extraction_mode,
    processed_at = Sys.time(),

    # Extraction
    num_claims_extracted = nrow(extracted),

    # Evaluation
    composite_score = eval_result$composite_v2,
    f1_score = eval_result$f1,
    precision = eval_result$precision,
    recall = eval_result$recall,
    ready_for_production = eval_result$ready_for_production_v2,

    # Confidence breakdown
    high_conf_count = sum(extracted$confidence == "high"),
    medium_conf_count = sum(extracted$confidence == "medium"),
    low_conf_count = sum(extracted$confidence == "low"),

    # DAGs
    dag_high = dag_high,
    dag_high_med = dag_high_med,
    dag_all = dag_all,

    # Confidence analysis
    conf_analysis = conf_analysis,

    # Raw data
    extracted_claims = extracted,
    eval_result = eval_result
  )

  class(result) <- "paper_result"

  if (verbose) {
    status <- ifelse(result$ready_for_production, "✓ PASS", "⚠ FAIL")
    cat(sprintf("  Result: %.3f composite %s\n", result$composite_score, status))
  }

  return(result)
}

# ============================================================================
# PART 4: BATCH PROCESSING ENGINE
# ============================================================================

run_batch_job <- function(job) {
  # Execute a batch job: process all papers

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║          PRODUCTION BATCH PIPELINE                        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  cat(sprintf("\nJob: %s\n", job$job_name))
  cat(sprintf("Mode: %s\n", job$extraction_mode))
  cat(sprintf("Papers: %d\n\n", length(job$papers)))

  start_time <- Sys.time()

  # Process each paper
  job$results <- list()
  for (i in seq_along(job$papers)) {
    paper <- job$papers[[i]]
    result <- process_single_paper(paper, job$extraction_mode, verbose = job$verbose)
    job$results[[i]] <- result
  }

  job$completed_at <- Sys.time()
  job$elapsed_time <- difftime(job$completed_at, start_time, units = "secs")

  if (job$verbose) {
    cat(sprintf("\n✓ Batch complete in %.1f seconds\n", as.numeric(job$elapsed_time)))
  }

  return(job)
}

# ============================================================================
# PART 5: BATCH RESULTS SUMMARY
# ============================================================================

summarize_batch_results <- function(job, verbose = TRUE) {
  # Generate comprehensive summary of batch results

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           BATCH RESULTS SUMMARY                           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Build results dataframe
  results_df <- data.frame(
    Paper = sapply(job$results, \(x) substr(x$paper_title, 1, 35)),
    Domain = sapply(job$results, \(x) x$domain),
    Scenario = sapply(job$results, \(x) x$scenario),
    Claims = sapply(job$results, \(x) x$num_claims_extracted),
    HighConf = sapply(job$results, \(x) x$high_conf_count),
    F1 = round(sapply(job$results, \(x) x$f1_score), 3),
    Composite = round(sapply(job$results, \(x) x$composite_score), 3),
    Status = ifelse(sapply(job$results, \(x) x$ready_for_production), "✓", "⚠"),
    stringsAsFactors = FALSE
  )

  print(results_df)

  # Aggregate statistics
  cat("\n--- AGGREGATE STATISTICS ---\n")
  cat(sprintf("Total papers processed: %d\n", length(job$results)))
  cat(sprintf("Papers ready for production: %d (%.1f%%)\n",
              sum(results_df$Status == "✓"),
              sum(results_df$Status == "✓") / nrow(results_df) * 100))

  cat(sprintf("\nAverage metrics:\n"))
  cat(sprintf("  F1-score: %.3f\n", mean(results_df$F1)))
  cat(sprintf("  Composite: %.3f\n", mean(results_df$Composite)))
  cat(sprintf("  High-conf claims: %.1f per paper\n",
              mean(results_df$HighConf)))

  cat(sprintf("\nProcessing time: %.1f seconds\n", as.numeric(job$elapsed_time)))

  return(results_df)
}

# ============================================================================
# PART 6: EXPORT RESULTS
# ============================================================================

export_batch_results <- function(job, output_dir = job$output_dir) {
  # Save batch results to disk for later analysis

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("\n📁 Exporting results to: %s\n", output_dir))

  # 1. Summary CSV
  results_df <- data.frame(
    paper = sapply(job$results, \(x) x$paper_title),
    domain = sapply(job$results, \(x) x$domain),
    claims_extracted = sapply(job$results, \(x) x$num_claims_extracted),
    high_confidence = sapply(job$results, \(x) x$high_conf_count),
    f1_score = round(sapply(job$results, \(x) x$f1_score), 3),
    composite_score = round(sapply(job$results, \(x) x$composite_score), 3),
    ready_for_production = sapply(job$results, \(x) x$ready_for_production),
    processed_at = sapply(job$results, \(x) as.character(x$processed_at))
  )

  csv_path <- file.path(output_dir, "batch_summary.csv")
  write.csv(results_df, csv_path, row.names = FALSE)
  cat(sprintf("  ✓ Summary: %s\n", csv_path))

  # 2. Job metadata
  job_info <- list(
    job_name = job$job_name,
    extraction_mode = job$extraction_mode,
    papers_processed = length(job$results),
    papers_ready = sum(results_df$ready_for_production),
    elapsed_seconds = as.numeric(job$elapsed_time),
    created_at = as.character(job$created_at),
    completed_at = as.character(job$completed_at)
  )

  job_info_path <- file.path(output_dir, "job_info.txt")
  cat(sprintf("Job: %s\n", job$job_name), file = job_info_path)
  cat(sprintf("Created: %s\n", job$created_at), file = job_info_path, append = TRUE)
  cat(sprintf("Completed: %s\n", job$completed_at), file = job_info_path, append = TRUE)
  cat(sprintf("Mode: %s\n", job$extraction_mode), file = job_info_path, append = TRUE)
  cat(sprintf("Papers: %d processed, %d ready\n", length(job$results), sum(results_df$ready_for_production)),
      file = job_info_path, append = TRUE)
  cat(sprintf("Time: %.1f seconds\n", as.numeric(job$elapsed_time)),
      file = job_info_path, append = TRUE)

  cat(sprintf("  ✓ Job info: %s\n", job_info_path))

  # 3. Detailed results per paper
  details_dir <- file.path(output_dir, "paper_details")
  if (!dir.exists(details_dir)) {
    dir.create(details_dir)
  }

  for (i in seq_along(job$results)) {
    r <- job$results[[i]]
    paper_name <- gsub("[^a-zA-Z0-9]", "_", r$paper_title)
    paper_dir <- file.path(details_dir, paper_name)
    if (!dir.exists(paper_dir)) {
      dir.create(paper_dir)
    }

    # Save claims as CSV
    claims <- r$extracted_claims
    claims_path <- file.path(paper_dir, "claims.csv")
    write.csv(claims, claims_path, row.names = FALSE)

    # Save metrics as text
    metrics_path <- file.path(paper_dir, "metrics.txt")
    cat(sprintf("Paper: %s\n", r$paper_title), file = metrics_path)
    cat(sprintf("F1-score: %.3f\n", r$f1_score), file = metrics_path, append = TRUE)
    cat(sprintf("Composite: %.3f\n", r$composite_score), file = metrics_path, append = TRUE)
    cat(sprintf("Ready: %s\n", r$ready_for_production), file = metrics_path, append = TRUE)
    cat(sprintf("High-conf: %d, Medium: %d, Low: %d\n",
                r$high_conf_count, r$medium_conf_count, r$low_conf_count),
        file = metrics_path, append = TRUE)
  }

  cat(sprintf("  ✓ Paper details: %s\n", details_dir))

  cat(sprintf("\n✓ Export complete!\n"))
  cat(sprintf("All results saved to: %s\n", output_dir))

  return(output_dir)
}

# ============================================================================
# PART 7: QUICK BATCH RUNNER (helper function)
# ============================================================================

run_quick_batch <- function(papers, job_name = "quick_batch",
                           extraction_mode = "mock_realistic",
                           output_dir = "batch_results",
                           verbose = TRUE) {
  # One-liner to run a complete batch: create → process → summarize → export

  job <- create_batch_job(job_name, papers, extraction_mode, output_dir, verbose)
  job <- run_batch_job(job)
  summary <- summarize_batch_results(job, verbose = verbose)
  export_batch_results(job, output_dir)

  return(job)
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Production Batch Pipeline module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. create_batch_job()        - Create a batch job\n")
cat("  2. run_batch_job()           - Execute the job\n")
cat("  3. summarize_batch_results() - Generate summary\n")
cat("  4. export_batch_results()    - Save to disk\n")
cat("  5. run_quick_batch()         - One-liner (create→run→summarize→export)\n")
cat("\nExample:\n")
cat("  job <- run_quick_batch(papers, job_name='wind_energy_test')\n")
