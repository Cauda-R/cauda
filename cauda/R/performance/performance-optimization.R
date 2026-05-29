# ============================================================================
# PHASE 4: PERFORMANCE OPTIMIZATION MODULE
# ============================================================================
# Parallel processing, caching, memory optimization
# Features:
#   - Parallel paper processing (multi-core)
#   - Smart result caching with fingerprinting
#   - Memory-efficient batch processing
#   - Progress tracking with ETA
#   - Performance metrics (throughput, latency)
#   - Scalability testing utilities

library(parallel)

# ============================================================================
# PART 1: PARALLEL BATCH PROCESSING
# ============================================================================

get_optimal_cores <- function(n_papers, verbose = TRUE) {
  # Determine optimal number of cores
  # Rule: use min(available_cores, n_papers/2, 8)
  # Minimum: 1, Maximum: 8

  available_cores <- detectCores()
  optimal_cores <- min(available_cores - 1, max(1, n_papers %/% 2), 8)

  if (verbose) {
    cat(sprintf("Available cores: %d\n", available_cores))
    cat(sprintf("Optimal cores for %d papers: %d\n", n_papers, optimal_cores))
  }

  return(optimal_cores)
}

process_papers_parallel <- function(papers,
                                   extraction_mode = "mock_realistic",
                                   n_cores = NULL,
                                   verbose = TRUE) {
  # Process multiple papers in parallel
  # Uses mapply for cross-core distribution

  if (is.null(n_cores)) {
    n_cores <- get_optimal_cores(length(papers), verbose = FALSE)
  }

  if (verbose) {
    cat(sprintf("\n[PARALLEL PROCESSING] Starting with %d cores...\n", n_cores))
  }

  start_time <- Sys.time()

  # Create cluster
  cl <- makeCluster(n_cores, type = "FORK")

  tryCatch(
    {
      # Export functions and libraries to cluster
      clusterExport(cl, c("process_single_paper", "extract_claims_batch",
                          "evaluate_single_paper_v2", "build_confidence_aware_dag",
                          "analyze_node_confidence"),
                    envir = parent.frame())

      # Process papers in parallel
      results <- parLapply(cl, papers, function(paper) {
        tryCatch(
          {
            process_single_paper(paper, extraction_mode, verbose = FALSE)
          },
          error = function(e) {
            return(list(
              paper_title = paper$title,
              error = e$message
            ))
          }
        )
      })

      stopCluster(cl)
    },
    error = function(e) {
      stopCluster(cl)
      stop(e$message)
    }
  )

  elapsed_time <- difftime(Sys.time(), start_time, units = "secs")

  if (verbose) {
    cat(sprintf("✓ Parallel processing complete in %.2f seconds\n", as.numeric(elapsed_time)))
    throughput <- length(papers) / as.numeric(elapsed_time)
    cat(sprintf("  Throughput: %.2f papers/second\n", throughput))
  }

  return(list(
    results = results,
    elapsed_time = as.numeric(elapsed_time),
    throughput = length(papers) / as.numeric(elapsed_time)
  ))
}

# ============================================================================
# PART 2: SMART CACHING WITH FINGERPRINTING
# ============================================================================

# Global cache registry
.cauda_result_cache <- new.env()

compute_paper_fingerprint <- function(paper) {
  # Create unique fingerprint for paper
  # Based on: title, domain, ground truth edges
  # Used to detect if paper/parameters have changed

  fingerprint_string <- paste0(
    paper$title, "|",
    paper$domain, "|",
    nrow(paper$ground_truth_edges), "|",
    paste(paper$ground_truth_edges$source, collapse = ","), "|",
    paste(paper$ground_truth_edges$target, collapse = ",")
  )

  # Create hash
  fingerprint <- digest::digest(fingerprint_string, algo = "md5")
  return(fingerprint)
}

cache_result <- function(paper, extraction_mode, result, ttl = 3600) {
  # Cache a paper processing result
  # ttl: time-to-live in seconds (default: 1 hour)

  fingerprint <- compute_paper_fingerprint(paper)
  cache_key <- paste0(fingerprint, "_", extraction_mode)

  .cauda_result_cache[[cache_key]] <- list(
    paper_title = paper$title,
    result = result,
    extraction_mode = extraction_mode,
    cached_at = Sys.time(),
    ttl = ttl
  )

  return(cache_key)
}

retrieve_cached_result <- function(paper, extraction_mode) {
  # Try to retrieve cached result
  # Returns NULL if not found or expired

  fingerprint <- compute_paper_fingerprint(paper)
  cache_key <- paste0(fingerprint, "_", extraction_mode)

  if (!exists(cache_key, envir = .cauda_result_cache)) {
    return(NULL)
  }

  cached <- .cauda_result_cache[[cache_key]]
  age_seconds <- as.numeric(difftime(Sys.time(), cached$cached_at, units = "secs"))

  # Check TTL
  if (age_seconds > cached$ttl) {
    rm(list = cache_key, envir = .cauda_result_cache)
    return(NULL)
  }

  return(cached$result)
}

get_cache_stats <- function(verbose = TRUE) {
  # Get cache statistics

  cache_size <- length(ls(.cauda_result_cache))

  if (verbose) {
    cat(sprintf("\n--- CACHE STATISTICS ---\n"))
    cat(sprintf("Cached results: %d\n", cache_size))

    if (cache_size > 0) {
      cached_keys <- ls(.cauda_result_cache)
      cat("\nCached papers:\n")
      for (key in cached_keys) {
        cached <- .cauda_result_cache[[key]]
        age <- as.numeric(difftime(Sys.time(), cached$cached_at, units = "mins"))
        cat(sprintf("  %s (%.1f min old)\n", substr(cached$paper_title, 1, 40), age))
      }
    }
  }

  return(list(
    cache_size = cache_size,
    cached_keys = if (cache_size > 0) ls(.cauda_result_cache) else character()
  ))
}

clear_cache <- function(verbose = TRUE) {
  # Clear all cached results

  rm(list = ls(.cauda_result_cache), envir = .cauda_result_cache)

  if (verbose) {
    cat("✓ Cache cleared\n")
  }
}

# ============================================================================
# PART 3: MEMORY-EFFICIENT BATCH PROCESSING
# ============================================================================

process_papers_batched <- function(papers,
                                  batch_size = 50,
                                  extraction_mode = "mock_realistic",
                                  use_cache = TRUE,
                                  use_parallel = TRUE,
                                  verbose = TRUE) {
  # Process papers in memory-efficient batches
  # Useful for 1000+ papers

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║     MEMORY-EFFICIENT BATCH PROCESSING                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  total_papers <- length(papers)
  n_batches <- ceiling(total_papers / batch_size)

  all_results <- list()
  batch_stats <- data.frame()

  start_time <- Sys.time()

  for (batch_num in 1:n_batches) {
    batch_start_idx <- (batch_num - 1) * batch_size + 1
    batch_end_idx <- min(batch_num * batch_size, total_papers)
    batch_papers <- papers[batch_start_idx:batch_end_idx]

    cat(sprintf("\n[BATCH %d/%d] Processing papers %d-%d (%d papers)\n",
                batch_num, n_batches, batch_start_idx, batch_end_idx, length(batch_papers)))

    batch_start_time <- Sys.time()

    # Try cache first
    cached_results <- list()
    uncached_papers <- list()
    uncached_indices <- integer()

    for (i in seq_along(batch_papers)) {
      paper <- batch_papers[[i]]

      if (use_cache) {
        cached <- retrieve_cached_result(paper, extraction_mode)
        if (!is.null(cached)) {
          cached_results[[length(cached_results) + 1]] <- cached
          cat(".")
          next
        }
      }

      uncached_papers[[length(uncached_papers) + 1]] <- paper
      uncached_indices <- c(uncached_indices, i)
    }

    # Process uncached papers
    if (length(uncached_papers) > 0) {
      if (use_parallel && length(uncached_papers) > 1) {
        # Parallel processing
        parallel_result <- process_papers_parallel(uncached_papers,
                                                   extraction_mode,
                                                   verbose = FALSE)
        batch_results <- parallel_result$results
      } else {
        # Sequential processing
        batch_results <- lapply(uncached_papers, function(paper) {
          process_single_paper(paper, extraction_mode, verbose = FALSE)
        })
      }

      # Cache results
      if (use_cache) {
        for (i in seq_along(batch_results)) {
          paper <- uncached_papers[[i]]
          result <- batch_results[[i]]
          cache_result(paper, extraction_mode, result)
        }
      }

      cat(sprintf(" %d new", length(batch_results)))
    }

    # Combine results
    # Note: merge cached and new results in original order
    combined_results <- list()
    cached_idx <- 1
    uncached_idx <- 1

    for (i in 1:length(batch_papers)) {
      if (i %in% uncached_indices) {
        combined_results[[i]] <- batch_results[[uncached_idx]]
        uncached_idx <- uncached_idx + 1
      } else {
        combined_results[[i]] <- cached_results[[cached_idx]]
        cached_idx <- cached_idx + 1
      }
    }

    all_results <- c(all_results, combined_results)

    batch_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))

    batch_stats <- rbind(batch_stats, data.frame(
      batch = batch_num,
      papers = length(batch_papers),
      elapsed_secs = batch_elapsed,
      throughput = length(batch_papers) / batch_elapsed,
      cached = length(cached_results),
      new = length(uncached_papers)
    ))

    cat(sprintf(" (%.2fs)\n", batch_elapsed))

    # Memory cleanup every 2 batches
    if (batch_num %% 2 == 0) {
      gc()
    }
  }

  total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              BATCH PROCESSING COMPLETE                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  cat(sprintf("\nTotal papers: %d\n", total_papers))
  cat(sprintf("Total time: %.2f seconds\n", total_elapsed))
  cat(sprintf("Average throughput: %.2f papers/sec\n", total_papers / total_elapsed))

  cat("\nBatch statistics:\n")
  print(batch_stats)

  return(list(
    results = all_results,
    batch_stats = batch_stats,
    total_elapsed = total_elapsed,
    average_throughput = total_papers / total_elapsed
  ))
}

# ============================================================================
# PART 4: SCALABILITY TESTING
# ============================================================================

benchmark_scalability <- function(paper,
                                 extraction_modes = c("mock_perfect", "mock_realistic", "mock_degraded"),
                                 verbose = TRUE) {
  # Benchmark a single paper across different extraction modes
  # Useful for performance profiling

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           SCALABILITY BENCHMARK                           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  cat(sprintf("\nPaper: %s\n", paper$title))

  results <- data.frame()

  for (mode in extraction_modes) {
    start_time <- Sys.time()

    result <- process_single_paper(paper, mode, verbose = FALSE)

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    results <- rbind(results, data.frame(
      mode = mode,
      elapsed_secs = elapsed,
      f1_score = result$f1_score,
      composite_score = result$composite_score,
      claims_extracted = result$num_claims_extracted
    ))

    cat(sprintf("\n%s:\n", mode))
    cat(sprintf("  Time: %.3f seconds\n", elapsed))
    cat(sprintf("  F1-Score: %.3f\n", result$f1_score))
    cat(sprintf("  Claims: %d\n", result$num_claims_extracted))
  }

  cat("\n--- SUMMARY ---\n")
  print(results)

  return(results)
}

simulate_large_batch <- function(n_papers = 100,
                                n_cores = NULL,
                                batch_size = 50,
                                verbose = TRUE) {
  # Simulate processing a large batch of papers
  # For capacity testing

  if (verbose) {
    cat(sprintf("\n🚀 SIMULATING BATCH OF %d PAPERS\n", n_papers))
  }

  # Create mock papers
  mock_papers <- list()
  for (i in 1:n_papers) {
    mock_papers[[i]] <- list(
      title = paste0("Paper_", sprintf("%04d", i)),
      domain = "wind_energy",
      scenario = sample(c("perfect", "realistic", "degraded"), 1),
      ground_truth_edges = data.frame(
        source = c("A", "B", "C"),
        target = c("D", "E", "F"),
        stringsAsFactors = FALSE
      )
    )
  }

  # Process with timing
  start_time <- Sys.time()

  result <- process_papers_batched(mock_papers,
                                   batch_size = batch_size,
                                   use_parallel = TRUE,
                                   use_cache = TRUE,
                                   verbose = verbose)

  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat(sprintf("\n✓ Simulation complete!\n"))
  cat(sprintf("  Processed: %d papers\n", n_papers))
  cat(sprintf("  Total time: %.2f seconds\n", total_time))
  cat(sprintf("  Throughput: %.2f papers/second\n", n_papers / total_time))

  return(result)
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Performance Optimization module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. get_optimal_cores(n_papers)          - Determine optimal CPU cores\n")
cat("  2. process_papers_parallel()            - Multi-core processing\n")
cat("  3. retrieve_cached_result()             - Get cached results\n")
cat("  4. cache_result()                       - Cache paper results\n")
cat("  5. get_cache_stats()                    - Cache statistics\n")
cat("  6. clear_cache()                        - Clear all cache\n")
cat("  7. process_papers_batched()             - Memory-efficient batching\n")
cat("  8. benchmark_scalability(paper)         - Performance profiling\n")
cat("  9. simulate_large_batch(n_papers)       - Capacity testing\n")
cat("\nRun: result <- process_papers_batched(papers, batch_size=50)\n")
