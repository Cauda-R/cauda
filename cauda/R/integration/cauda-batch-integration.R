# ============================================================================
# CAUDA BATCH INTEGRATION MODULE
# ============================================================================
# Integrates production batch pipeline into main cauda ecosystem
# Adds: cauda.batch_process(), visualizations, cross-paper analysis, validation

library(ggplot2)
library(gridExtra)

# ============================================================================
# PART 1: CAUDA BATCH PROCESS - Main Integration Function
# ============================================================================

cauda.batch_process <- function(papers,
                               job_name = "cauda_batch",
                               extraction_mode = "mock_realistic",
                               ground_truth_dags = NULL,  # Optional: list of ground truth DAGs for validation
                               output_dir = "cauda_batch_results",
                               visualize = TRUE,
                               verbose = TRUE) {
  # Main entry point: process multiple papers using cauda pipeline
  # Integrated with existing cauda ecosystem

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           CAUDA BATCH PROCESS - INTEGRATED MODE           ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Run batch pipeline
  job <- run_quick_batch(papers, job_name, extraction_mode, output_dir, verbose)

  # Validation mode (if ground truth provided)
  if (!is.null(ground_truth_dags)) {
    cat("\n--- VALIDATION MODE: Comparing Against Ground Truth ---\n")
    validation_results <- validate_batch_against_ground_truth(
      job, ground_truth_dags, verbose = verbose
    )
    job$validation <- validation_results
  }

  # Cross-paper analysis
  cat("\n--- CROSS-PAPER ANALYSIS ---\n")
  cross_analysis <- analyze_cross_paper_patterns(job, verbose = verbose)
  job$cross_analysis <- cross_analysis

  # Visualizations
  if (visualize) {
    cat("\n--- GENERATING VISUALIZATIONS ---\n")
    viz_dir <- file.path(output_dir, "visualizations")
    if (!dir.exists(viz_dir)) dir.create(viz_dir, recursive = TRUE)

    generate_batch_visualizations(job, viz_dir, verbose = verbose)
    job$viz_dir <- viz_dir
  }

  cat("\n✓ Batch process complete!\n")
  return(job)
}

# ============================================================================
# PART 2: VALIDATION MODE - Compare Against Ground Truth
# ============================================================================

validate_batch_against_ground_truth <- function(job, ground_truth_dags, verbose = TRUE) {
  # Compare extracted DAGs against ground truth
  # Returns detailed accuracy metrics per paper

  validation_results <- list()

  for (i in seq_along(job$results)) {
    paper_result <- job$results[[i]]
    gt_dag <- ground_truth_dags[[i]]

    if (is.null(gt_dag)) {
      next
    }

    # Get ground truth edges
    gt_edges <- gt_dag$ground_truth_edges
    gt_edge_set <- paste(gt_edges$source, "->", gt_edges$target)

    # Get extracted edges at different confidence levels
    high_conf <- paper_result$extracted_claims[paper_result$extracted_claims$confidence == "high", ]
    high_med_conf <- paper_result$extracted_claims[paper_result$extracted_claims$confidence %in% c("high", "medium"), ]
    all_conf <- paper_result$extracted_claims

    # Calculate metrics for each confidence level
    validation_results[[i]] <- list(
      paper = paper_result$paper_title,

      high_confidence = calculate_validation_metrics(
        high_conf, gt_edge_set
      ),

      high_and_medium = calculate_validation_metrics(
        high_med_conf, gt_edge_set
      ),

      all_claims = calculate_validation_metrics(
        all_conf, gt_edge_set
      ),

      ground_truth_edges = nrow(gt_edges)
    )

    if (verbose) {
      cat(sprintf("\n%s:\n", paper_result$paper_title))
      cat(sprintf("  High-conf only:  F1=%.3f (P=%.3f, R=%.3f)\n",
                  validation_results[[i]]$high_confidence$f1,
                  validation_results[[i]]$high_confidence$precision,
                  validation_results[[i]]$high_confidence$recall))
      cat(sprintf("  High+Medium:     F1=%.3f (P=%.3f, R=%.3f)\n",
                  validation_results[[i]]$high_and_medium$f1,
                  validation_results[[i]]$high_and_medium$precision,
                  validation_results[[i]]$high_and_medium$recall))
      cat(sprintf("  All claims:      F1=%.3f (P=%.3f, R=%.3f)\n",
                  validation_results[[i]]$all_claims$f1,
                  validation_results[[i]]$all_claims$precision,
                  validation_results[[i]]$all_claims$recall))
    }
  }

  return(validation_results)
}

calculate_validation_metrics <- function(claims_df, gt_edge_set) {
  # Helper: calculate precision, recall, F1

  claims_df <- claims_df[!is.na(claims_df$source) & !is.na(claims_df$target), ]

  if (nrow(claims_df) == 0) {
    return(list(precision = 0, recall = 0, f1 = 0, tp = 0, fp = 0, fn = 0))
  }

  extracted_edges <- paste(claims_df$source, "->", claims_df$target)

  tp <- length(intersect(extracted_edges, gt_edge_set))
  fp <- length(setdiff(extracted_edges, gt_edge_set))
  fn <- length(setdiff(gt_edge_set, extracted_edges))

  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0
  f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  return(list(
    precision = precision,
    recall = recall,
    f1 = f1,
    tp = tp,
    fp = fp,
    fn = fn
  ))
}

# ============================================================================
# PART 3: CROSS-PAPER ANALYSIS
# ============================================================================

analyze_cross_paper_patterns <- function(job, verbose = TRUE) {
  # Analyze patterns across multiple papers
  # - Which claims appear in multiple papers?
  # - Which claims conflict?
  # - What's consensus?

  all_claims <- list()

  for (result in job$results) {
    claims <- result$extracted_claims
    claims$paper <- result$paper_title
    claims$confidence <- factor(claims$confidence, levels = c("high", "medium", "low"))
    all_claims[[result$paper_title]] <- claims
  }

  combined_claims <- do.call(rbind, all_claims)
  rownames(combined_claims) <- NULL

  # Find consensus claims (appear in multiple papers with high confidence)
  edge_counts <- table(paste(combined_claims$source, "->", combined_claims$target))
  consensus_edges <- names(edge_counts[edge_counts >= 2])

  # Find high-confidence claims
  high_conf_edges <- combined_claims[combined_claims$confidence == "high", ]
  high_conf_edge_set <- paste(high_conf_edges$source, "->", high_conf_edges$target)

  consensus_high_conf <- intersect(consensus_edges, high_conf_edge_set)

  if (verbose) {
    cat(sprintf("Total unique claims: %d\n", nrow(combined_claims)))
    cat(sprintf("Consensus claims (2+ papers): %d\n", length(consensus_edges)))
    cat(sprintf("Consensus + high-confidence: %d\n", length(consensus_high_conf)))

    if (length(consensus_high_conf) > 0) {
      cat("\nConsensus claims:\n")
      for (edge in consensus_high_conf[1:min(5, length(consensus_high_conf))]) {
        cat(sprintf("  %s\n", edge))
      }
      if (length(consensus_high_conf) > 5) {
        cat(sprintf("  ... and %d more\n", length(consensus_high_conf) - 5))
      }
    }
  }

  return(list(
    total_claims = nrow(combined_claims),
    unique_edges = length(unique(paste(combined_claims$source, "->", combined_claims$target))),
    consensus_edges = consensus_edges,
    consensus_high_conf = consensus_high_conf,
    all_combined_claims = combined_claims
  ))
}

# ============================================================================
# PART 4: PUBLICATION-QUALITY VISUALIZATIONS
# ============================================================================

generate_batch_visualizations <- function(job, viz_dir, verbose = TRUE) {
  # Create professional-quality plots for paper/presentation

  # 1. Performance overview
  if (verbose) cat("  ▸ Generating performance overview...\n")
  plot_batch_performance(job, file.path(viz_dir, "01_performance_overview.pdf"))

  # 2. Confidence distribution
  if (verbose) cat("  ▸ Generating confidence analysis...\n")
  plot_confidence_distribution(job, file.path(viz_dir, "02_confidence_distribution.pdf"))

  # 3. Validation comparison (if validation available)
  if (!is.null(job$validation)) {
    if (verbose) cat("  ▸ Generating validation metrics...\n")
    plot_validation_results(job, file.path(viz_dir, "03_validation_metrics.pdf"))
  }

  # 4. Cross-paper patterns
  if (!is.null(job$cross_analysis)) {
    if (verbose) cat("  ▸ Generating cross-paper analysis...\n")
    plot_cross_paper_patterns(job, file.path(viz_dir, "04_cross_paper_patterns.pdf"))
  }

  if (verbose) cat("  ✓ Visualizations saved to: ", viz_dir, "\n")
}

# Plot 1: Performance overview (F1, Composite, Claims)
plot_batch_performance <- function(job, output_file) {
  # HORIZONTAL BARS - SCALABLE for any number of papers

  results_df <- data.frame(
    paper = sapply(job$results, \(x) x$paper_title),
    f1 = sapply(job$results, \(x) x$f1_score),
    composite = sapply(job$results, \(x) x$composite_score),
    claims = sapply(job$results, \(x) x$num_claims_extracted)
  )

  n_papers <- nrow(results_df)
  plot_height <- max(6, 2 + n_papers * 0.8)  # Scale height by number of papers

  # Common CAUDA theme - HORIZONTAL
  cauda_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold", color = "#222222", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#888888", margin = margin(b = 8)),
      axis.title = element_text(face = "bold", size = 11, color = "#444444"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#333333"),
      axis.text.x = element_text(size = 10, color = "#555555"),
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 15, 12, 15)
    )

  # HORIZONTAL BARS - F1 Score
  p1 <- ggplot(results_df, aes(x = f1, y = reorder(paper, f1))) +
    geom_bar(stat = "identity", fill = "#4A90D9", color = "#2C5AA0", linewidth = 0.7, alpha = 0.88, width = 0.7) +
    geom_text(aes(label = sprintf("%.3f", f1)), hjust = -0.15, size = 4.5, fontface = "bold", color = "#222222") +
    geom_vline(xintercept = 0.75, linetype = "dashed", color = "#E84545", linewidth = 1.3, alpha = 0.8) +
    labs(title = "F1-Score by Paper", x = "F1-Score", y = "", subtitle = "Threshold: 0.75") +
    xlim(0, 1.05) +
    cauda_theme

  # HORIZONTAL BARS - Composite Score
  p2 <- ggplot(results_df, aes(x = composite, y = reorder(paper, composite))) +
    geom_bar(stat = "identity", fill = "#4A90D9", color = "#2C5AA0", linewidth = 0.7, alpha = 0.88, width = 0.7) +
    geom_text(aes(label = sprintf("%.3f", composite)), hjust = -0.15, size = 4.5, fontface = "bold", color = "#222222") +
    geom_vline(xintercept = 0.75, linetype = "dashed", color = "#E84545", linewidth = 1.3, alpha = 0.8) +
    labs(title = "Composite Score by Paper", x = "Composite Score", y = "", subtitle = "Threshold: 0.75") +
    xlim(0, 1.05) +
    cauda_theme

  # HORIZONTAL BARS - Claims Count
  p3 <- ggplot(results_df, aes(x = claims, y = reorder(paper, claims))) +
    geom_bar(stat = "identity", fill = "#4A90D9", color = "#2C5AA0", linewidth = 0.7, alpha = 0.88, width = 0.7) +
    geom_text(aes(label = sprintf("%d", claims)), hjust = -0.15, size = 4.5, fontface = "bold", color = "#222222") +
    labs(title = "Claims Extracted by Paper", x = "Number of Claims", y = "") +
    cauda_theme

  combined <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

  ggsave(output_file, combined, width = 18, height = plot_height, dpi = 300)
}

# Plot 2: Confidence distribution (CAUDA STYLED)
plot_confidence_distribution <- function(job, output_file) {
  conf_data <- data.frame()

  for (result in job$results) {
    claims <- result$extracted_claims
    conf_summary <- data.frame(
      paper = substr(result$paper_title, 1, 20),
      high = sum(claims$confidence == "high"),
      medium = sum(claims$confidence == "medium"),
      low = sum(claims$confidence == "low")
    )
    conf_data <- rbind(conf_data, conf_summary)
  }

  conf_long <- reshape2::melt(conf_data, id.vars = "paper")

  n_papers <- length(unique(conf_long$paper))
  plot_height <- max(6, 1.5 + n_papers * 0.8)

  p <- ggplot(conf_long, aes(x = value, y = reorder(paper, value), fill = variable)) +
    geom_bar(stat = "identity", position = "stack", color = "#333333", linewidth = 0.5, width = 0.75) +
    geom_text(aes(label = ifelse(value > 0, value, "")), position = position_stack(vjust = 0.5),
              size = 4.5, fontface = "bold", color = "white") +
    scale_fill_manual(
      values = c("high" = "#2ecc71", "medium" = "#f39c12", "low" = "#e74c3c"),
      name = "Confidence",
      labels = c("high" = "High", "medium" = "Medium", "low" = "Low")
    ) +
    labs(title = "Confidence Distribution by Paper", x = "Number of Claims", y = "",
         subtitle = "Breakdown by confidence level") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold", color = "#222222", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#888888", margin = margin(b = 8)),
      axis.title = element_text(face = "bold", size = 11, color = "#444444"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#333333"),
      axis.text.x = element_text(size = 10, color = "#555555"),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 15, 12, 15)
    )

  ggsave(output_file, p, width = 12, height = plot_height, dpi = 300)
}

# Plot 3: Validation metrics (if available) (CAUDA STYLED)
plot_validation_results <- function(job, output_file) {
  if (is.null(job$validation)) return(NULL)

  val_data <- data.frame()

  for (v in job$validation) {
    if (!is.null(v)) {
      val_data <- rbind(val_data, data.frame(
        paper = substr(v$paper, 1, 20),
        high_conf_f1 = v$high_confidence$f1,
        high_med_f1 = v$high_and_medium$f1,
        all_f1 = v$all_claims$f1
      ))
    }
  }

  val_long <- reshape2::melt(val_data, id.vars = "paper")

  n_papers <- length(unique(val_long$paper))
  plot_height <- max(6, 2 + n_papers * 0.8)

  p <- ggplot(val_long, aes(x = value, y = reorder(paper, value), fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", color = "#333333", linewidth = 0.5, width = 0.75) +
    geom_text(aes(label = sprintf("%.3f", value)), position = position_dodge(width = 0.75),
              hjust = -0.15, size = 4.5, fontface = "bold", color = "#222222") +
    geom_vline(xintercept = 0.75, linetype = "dashed", color = "#E84545", linewidth = 1.3, alpha = 0.8) +
    scale_fill_manual(
      values = c("high_conf_f1" = "#2ecc71", "high_med_f1" = "#f39c12", "all_f1" = "#4A90D9"),
      name = "Filter",
      labels = c("all_f1" = "All", "high_conf_f1" = "High Only", "high_med_f1" = "High+Med")
    ) +
    xlim(0, 1.05) +
    labs(title = "Validation F1-Scores by Confidence Level", x = "F1-Score", y = "",
         subtitle = "Production threshold: 0.75") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold", color = "#222222", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#888888", margin = margin(b = 8)),
      axis.title = element_text(face = "bold", size = 11, color = "#444444"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#333333"),
      axis.text.x = element_text(size = 10, color = "#555555"),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 15, 12, 15)
    )

  ggsave(output_file, p, width = 14, height = plot_height, dpi = 300)
}

# Plot 4: Cross-paper patterns (CAUDA STYLED)
plot_cross_paper_patterns <- function(job, output_file) {
  if (is.null(job$cross_analysis)) return(NULL)

  stats_df <- data.frame(
    metric = c("Total Claims", "Unique Edges", "Consensus", "Consensus + High"),
    count = c(
      job$cross_analysis$total_claims,
      job$cross_analysis$unique_edges,
      length(job$cross_analysis$consensus_edges),
      length(job$cross_analysis$consensus_high_conf)
    )
  )

  # CAUDA color palette for each metric
  metric_colors <- c(
    "Total Claims" = "#4A90D9",
    "Unique Edges" = "#4A90D9",
    "Consensus" = "#2ecc71",
    "Consensus + High" = "#f39c12"
  )

  p <- ggplot(stats_df, aes(x = count, y = reorder(metric, count), fill = metric)) +
    geom_bar(stat = "identity", color = "#333333", linewidth = 0.5, width = 0.75) +
    geom_text(aes(label = count), hjust = -0.15, size = 5, fontface = "bold", color = "#222222") +
    scale_fill_manual(values = metric_colors) +
    labs(title = "Cross-Paper Analysis Summary",
         subtitle = "Pattern extraction across papers", x = "Count", y = "") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#222222", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#888888"),
      axis.title = element_text(face = "bold", size = 13, color = "#444444"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 11, face = "bold", color = "#333333"),
      axis.text.x = element_text(size = 11, color = "#555555"),
      legend.position = "none",
      panel.grid.major.x = element_line(color = "#DDDDDD", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 15, 12, 15)
    )

  ggsave(output_file, p, width = 14, height = 7, dpi = 300)
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Cauda Batch Integration module loaded.\n")
cat("\nMain function: cauda.batch_process(papers, job_name, ...)\n")
cat("  - Integrated with cauda ecosystem\n")
cat("  - Validation mode (compare vs ground truth)\n")
cat("  - Cross-paper analysis (consensus, patterns)\n")
cat("  - Publication-quality visualizations\n")
