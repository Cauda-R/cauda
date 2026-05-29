# ============================================================================
# CONFIDENCE-AWARE DAG VISUALIZATION MODULE
# ============================================================================
# Builds and visualizes DAGs with confidence-based coloring
# Supports filtering by confidence level and comparative analysis

library(bnlearn)
library(igraph)

# ============================================================================
# PART 1: CONFIDENCE-AWARE DAG BUILDER
# ============================================================================

build_confidence_aware_dag <- function(extracted_claims,
                                      confidence_filter = "all",
                                      add_confidence_attrs = TRUE) {
  # Build a DAG that preserves confidence information
  # confidence_filter: "high_only", "high_and_medium", "all", "low_only"

  # Filter claims
  if (confidence_filter == "high_only") {
    filtered <- extracted_claims[extracted_claims$confidence == "high", ]
  } else if (confidence_filter == "high_and_medium") {
    filtered <- extracted_claims[extracted_claims$confidence %in% c("high", "medium"), ]
  } else if (confidence_filter == "all") {
    filtered <- extracted_claims
  } else if (confidence_filter == "low_only") {
    filtered <- extracted_claims[extracted_claims$confidence == "low", ]
  }

  # Remove NAs
  filtered <- filtered[!is.na(filtered$source) & !is.na(filtered$target), ]

  if (nrow(filtered) == 0) {
    cat(sprintf("⚠ No claims match filter: %s\n", confidence_filter))
    return(NULL)
  }

  # Create nodes list
  nodes <- unique(c(filtered$source, filtered$target))
  nodes <- nodes[!is.na(nodes)]

  # Build empty DAG
  dag <- bnlearn::empty.graph(nodes)

  # Add arcs with confidence attributes
  for (i in 1:nrow(filtered)) {
    from <- filtered$source[i]
    to <- filtered$target[i]
    conf <- filtered$confidence[i]

    dag <- bnlearn::set.arc(dag, from, to)

    # Store confidence as arc attribute
    if (add_confidence_attrs) {
      # Note: bnlearn doesn't directly support arc attributes,
      # so we'll return them separately
    }
  }

  # Create arc metadata dataframe
  arc_metadata <- data.frame(
    from = filtered$source,
    to = filtered$target,
    confidence = filtered$confidence,
    pathway = filtered$pathway,
    strength = filtered$strength,
    stringsAsFactors = FALSE
  )

  # Return dag with metadata
  attr(dag, "arc_metadata") <- arc_metadata
  attr(dag, "confidence_filter") <- confidence_filter
  attr(dag, "num_high") <- sum(filtered$confidence == "high")
  attr(dag, "num_medium") <- sum(filtered$confidence == "medium")
  attr(dag, "num_low") <- sum(filtered$confidence == "low")

  return(dag)
}

# ============================================================================
# PART 2: CONFIDENCE SUMMARY & STATISTICS
# ============================================================================

summarize_dag_confidence <- function(dag, verbose = TRUE) {
  # Analyze the confidence composition of a DAG

  metadata <- attr(dag, "arc_metadata")
  conf_filter <- attr(dag, "confidence_filter")

  summary <- list(
    total_arcs = nrow(metadata),
    high_count = sum(metadata$confidence == "high"),
    medium_count = sum(metadata$confidence == "medium"),
    low_count = sum(metadata$confidence == "low"),
    high_pct = sum(metadata$confidence == "high") / nrow(metadata) * 100,
    medium_pct = sum(metadata$confidence == "medium") / nrow(metadata) * 100,
    low_pct = sum(metadata$confidence == "low") / nrow(metadata) * 100,
    num_nodes = length(dag$nodes),
    confidence_filter = conf_filter
  )

  if (verbose) {
    cat("\n--- DAG CONFIDENCE SUMMARY ---\n")
    cat(sprintf("Filter: %s\n", conf_filter))
    cat(sprintf("Total arcs: %d\n", summary$total_arcs))
    cat(sprintf("Nodes: %d\n", summary$num_nodes))
    cat(sprintf("\nConfidence breakdown:\n"))
    cat(sprintf("  High:   %d (%.1f%%)\n", summary$high_count, summary$high_pct))
    cat(sprintf("  Medium: %d (%.1f%%)\n", summary$medium_count, summary$medium_pct))
    cat(sprintf("  Low:    %d (%.1f%%)\n", summary$low_count, summary$low_pct))
  }

  return(summary)
}

# ============================================================================
# PART 3: CONFIDENCE-BASED VISUALIZATION
# ============================================================================

# Color mapping for confidence levels
confidence_colors <- function(confidence_vector) {
  # Returns colors for confidence levels
  # High: Green, Medium: Yellow, Low: Red

  colors <- character(length(confidence_vector))

  colors[confidence_vector == "high"] <- "#2ecc71"    # Green
  colors[confidence_vector == "medium"] <- "#f39c12"  # Orange
  colors[confidence_vector == "low"] <- "#e74c3c"     # Red
  colors[is.na(confidence_vector)] <- "#95a5a6"       # Gray

  return(colors)
}

# Prepare DAG for visualization
prepare_dag_visualization <- function(dag) {
  # Convert DAG to igraph for visualization

  arcs_mat <- bnlearn::arcs(dag)
  metadata <- attr(dag, "arc_metadata")

  if (nrow(arcs_mat) == 0) {
    cat("⚠ DAG has no arcs to visualize\n")
    return(NULL)
  }

  # Create igraph from arcs
  g <- igraph::graph_from_edgelist(arcs_mat, directed = TRUE)

  # Add confidence colors to edges
  edge_confidence <- character(igraph::ecount(g))
  for (i in 1:nrow(arcs_mat)) {
    from_idx <- which(metadata$from == arcs_mat[i, 1] & metadata$to == arcs_mat[i, 2])
    if (length(from_idx) > 0) {
      conf <- metadata$confidence[from_idx[1]]
      edge_idx <- which(igraph::head_of(g, igraph::E(g)) == which(igraph::V(g)$name == arcs_mat[i, 2]) &
                        igraph::tail_of(g, igraph::E(g)) == which(igraph::V(g)$name == arcs_mat[i, 1]))
      if (length(edge_idx) > 0) {
        edge_confidence[edge_idx] <- conf
      }
    }
  }

  # Add edge colors based on confidence
  edge_colors <- confidence_colors(metadata$confidence)

  # Store visualization attributes
  attr(g, "edge_colors") <- edge_colors
  attr(g, "edge_confidence") <- metadata$confidence
  attr(g, "metadata") <- metadata

  return(g)
}

# Visualize DAG with confidence coloring
plot_confidence_dag <- function(dag, title = NULL, node_size = 15,
                               vertex_label_cex = 0.7, layout_method = "spring") {
  # Plot DAG with confidence-based edge coloring

  g <- prepare_dag_visualization(dag)

  if (is.null(g)) {
    return(NULL)
  }

  metadata <- attr(dag, "arc_metadata")
  edge_colors <- attr(g, "edge_colors")

  # Choose layout
  if (layout_method == "spring") {
    layout <- igraph::layout_with_fr(g)
  } else if (layout_method == "circle") {
    layout <- igraph::layout_in_circle(g)
  } else {
    layout <- igraph::layout_with_fr(g)
  }

  # Create plot
  plot(g,
       vertex.size = node_size,
       vertex.label.cex = vertex_label_cex,
       vertex.color = "#3498db",
       vertex.label.color = "white",
       edge.color = edge_colors,
       edge.width = 2,
       edge.arrow.size = 0.7,
       layout = layout,
       main = title,
       margin = 0.1)

  # Add legend
  legend("topright",
         legend = c("High confidence", "Medium confidence", "Low confidence"),
         col = c("#2ecc71", "#f39c12", "#e74c3c"),
         lty = 1,
         lwd = 2)

  return(invisible(g))
}

# ============================================================================
# PART 4: COMPARATIVE ANALYSIS - MULTIPLE CONFIDENCE LEVELS
# ============================================================================

compare_confidence_levels <- function(extracted_claims, paper_obj = NULL,
                                     verbose = TRUE) {
  # Compare DAGs at different confidence levels
  # Shows what claims you keep/lose with filtering

  filters <- c("high_only", "high_and_medium", "all")

  comparison <- list()

  for (filter in filters) {
    dag <- build_confidence_aware_dag(extracted_claims, confidence_filter = filter)

    if (!is.null(dag)) {
      summary <- summarize_dag_confidence(dag, verbose = FALSE)
      comparison[[filter]] <- summary
    }
  }

  if (verbose) {
    cat("\n╔════════════════════════════════════════════════════════════╗\n")
    cat("║         CONFIDENCE LEVEL COMPARISON                        ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")

    for (filter in filters) {
      s <- comparison[[filter]]
      if (!is.null(s)) {
        cat(sprintf("\n%s:\n", toupper(filter)))
        cat(sprintf("  Arcs: %d  |  Nodes: %d\n", s$total_arcs, s$num_nodes))
        cat(sprintf("  High: %d, Medium: %d, Low: %d\n",
                    s$high_count, s$medium_count, s$low_count))
      }
    }

    # Show impact of filtering
    all_arcs <- comparison$all$total_arcs
    high_arcs <- comparison$high_only$total_arcs
    high_med_arcs <- comparison$high_and_medium$total_arcs

    cat("\n--- FILTERING IMPACT ---\n")
    cat(sprintf("Keeping high-confidence only: %.1f%% of arcs (%d/%d)\n",
                high_arcs / all_arcs * 100, high_arcs, all_arcs))
    cat(sprintf("Keeping high+medium:         %.1f%% of arcs (%d/%d)\n",
                high_med_arcs / all_arcs * 100, high_med_arcs, all_arcs))

    # Accuracy improvement (if ground truth available)
    if (!is.null(paper_obj)) {
      gt_edges <- paper_obj$ground_truth_edges
      gt_edge_set <- paste(gt_edges$source, "->", gt_edges$target)

      for (filter in filters) {
        dag <- build_confidence_aware_dag(extracted_claims, confidence_filter = filter)
        metadata <- attr(dag, "arc_metadata")
        correct <- sum(paste(metadata$from, "->", metadata$to) %in% gt_edge_set)
        accuracy <- correct / nrow(metadata) * 100

        cat(sprintf("\n%s accuracy: %.1f%% (%d/%d correct)\n",
                    filter, accuracy, correct, nrow(metadata)))
      }
    }
  }

  return(comparison)
}

# ============================================================================
# PART 5: NODE-LEVEL CONFIDENCE ANALYSIS
# ============================================================================

analyze_node_confidence <- function(extracted_claims, verbose = TRUE) {
  # Analyze confidence at the node level
  # Which nodes are involved in high-confidence vs low-confidence claims?

  high_claims <- extracted_claims[extracted_claims$confidence == "high", ]
  low_claims <- extracted_claims[extracted_claims$confidence == "low", ]

  high_nodes <- unique(c(high_claims$source, high_claims$target))
  low_nodes <- unique(c(low_claims$source, low_claims$target))

  high_only <- setdiff(high_nodes, low_nodes)
  low_only <- setdiff(low_nodes, high_nodes)
  both <- intersect(high_nodes, low_nodes)

  if (verbose) {
    cat("\n--- NODE CONFIDENCE ANALYSIS ---\n")
    cat(sprintf("High-confidence only (%.0f nodes):\n", length(high_only)))
    if (length(high_only) > 0) {
      cat(sprintf("  %s\n", paste(high_only, collapse = ", ")))
    } else {
      cat("  (none)\n")
    }

    cat(sprintf("\nLow-confidence only (%.0f nodes):\n", length(low_only)))
    if (length(low_only) > 0) {
      cat(sprintf("  %s\n", paste(low_only, collapse = ", ")))
    } else {
      cat("  (none)\n")
    }

    cat(sprintf("\nBoth high and low (%.0f nodes - uncertain):\n", length(both)))
    if (length(both) > 0) {
      cat(sprintf("  %s\n", paste(both, collapse = ", ")))
    } else {
      cat("  (none)\n")
    }
  }

  return(list(
    high_only = high_only,
    low_only = low_only,
    both = both
  ))
}

# ============================================================================
# PART 6: COMPREHENSIVE CONFIDENCE VISUALIZATION REPORT
# ============================================================================

generate_confidence_visualization_report <- function(extracted_claims, paper_obj = NULL,
                                                    verbose = TRUE, plot = TRUE) {
  # Complete confidence-aware analysis with visualizations

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║    CONFIDENCE-AWARE DAG VISUALIZATION REPORT              ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # 1. Overall confidence distribution
  cat("\n[1/5] Overall Confidence Distribution\n")
  cat("───────────────────────────────────────\n")
  dist <- table(extracted_claims$confidence)
  for (conf in names(dist)) {
    cat(sprintf("  %s: %d claims (%.1f%%)\n",
                conf, dist[conf], dist[conf] / sum(dist) * 100))
  }

  # 2. Compare confidence levels
  cat("\n[2/5] Comparing Different Confidence Filters\n")
  cat("───────────────────────────────────────────────\n")
  comparison <- compare_confidence_levels(extracted_claims, paper_obj, verbose = TRUE)

  # 3. Node-level analysis
  cat("\n[3/5] Node-Level Confidence Analysis\n")
  cat("───────────────────────────────────────\n")
  node_conf <- analyze_node_confidence(extracted_claims, verbose = TRUE)

  # 4. Visualization
  if (plot) {
    cat("\n[4/5] Generating Visualizations\n")
    cat("──────────────────────────────────\n")

    # All claims DAG
    cat("  ▸ Plotting high-confidence DAG...\n")
    dag_high <- build_confidence_aware_dag(extracted_claims, "high_only")
    if (!is.null(dag_high)) {
      plot_confidence_dag(dag_high,
                         title = paste0(paper_obj$title, "\n(High-Confidence Claims Only)"))
    }

    # High + Medium
    cat("  ▸ Plotting high+medium-confidence DAG...\n")
    dag_high_med <- build_confidence_aware_dag(extracted_claims, "high_and_medium")
    if (!is.null(dag_high_med)) {
      plot_confidence_dag(dag_high_med,
                         title = paste0(paper_obj$title, "\n(High + Medium Confidence)"))
    }

    # All
    cat("  ▸ Plotting all claims DAG...\n")
    dag_all <- build_confidence_aware_dag(extracted_claims, "all")
    if (!is.null(dag_all)) {
      plot_confidence_dag(dag_all,
                         title = paste0(paper_obj$title, "\n(All Claims)"))
    }
  }

  # 5. Recommendations
  cat("\n[5/5] Recommendations for Analysis\n")
  cat("────────────────────────────────────\n")

  high_pct <- dist["high"] / sum(dist) * 100
  if (high_pct > 70) {
    cat("✓ High-confidence claims dominate (>70%)\n")
    cat("  → Use high-confidence DAG for main analysis\n")
  } else if (high_pct > 50) {
    cat("⚠ Moderate high-confidence (50-70%)\n")
    cat("  → Use high+medium confidence for main analysis\n")
  } else {
    cat("✗ Low proportion of high-confidence claims (<50%)\n")
    cat("  → Extraction quality may need improvement\n")
  }

  if (length(node_conf$both) > 0) {
    cat(sprintf("\n⚠ %d nodes have both high and low confidence claims\n", length(node_conf$both)))
    cat("  → These nodes need domain expert review\n")
  }

  cat("\n✓ Report complete.\n")

  return(list(
    distribution = dist,
    comparison = comparison,
    node_confidence = node_conf
  ))
}

# ============================================================================
# INITIALIZATION
# ============================================================================

cat("\n✓ Confidence-Aware DAG Visualization module loaded.\n")
cat("\nAvailable functions:\n")
cat("  1. build_confidence_aware_dag(claims, filter)    - Build filtered DAG\n")
cat("  2. summarize_dag_confidence(dag)                 - Summary statistics\n")
cat("  3. prepare_dag_visualization(dag)                - Convert to igraph\n")
cat("  4. plot_confidence_dag(dag)                      - Visualize with colors\n")
cat("  5. compare_confidence_levels(claims)             - Compare filters\n")
cat("  6. analyze_node_confidence(claims)               - Node-level analysis\n")
cat("  7. generate_confidence_visualization_report()    - Full report + plots\n")
cat("\nExample:\n")
cat("  report <- generate_confidence_visualization_report(claims, paper_obj)\n")
