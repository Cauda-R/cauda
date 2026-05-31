# =============================================================================
# cauda.R
# cauda: Causal Automated Unified Data Analysis
# A simple, high-level R toolkit for exploratory causal data analysis.
#
# Functions:
#   cauda.missing()       - summarize missing values in raw data
#   cauda.recode()        - auto-recode character variables to numeric
#   cauda.clean()         - remove uninformative/redundant columns, handle NAs
#   cauda.prep()          - recode + clean in one step
#   cauda.dag()           - learn and plot a causal DAG via bnlearn
#   cauda.add()           - add an arrow to the DAG
#   cauda.delete()        - delete an arrow from the DAG
#   cauda.flip()          - flip an arrow direction in the DAG
#   cauda.corr()          - correlation heatmap + spring network (side by side)
#   cauda.pcorr()         - partial correlation network
#   cauda.consensus()     - consensus DAG across multiple algorithms
#   cauda.pdp()           - PDP / ICE / ALE plots for a feature
#   cauda.pdp2d()         - 2D PDP showing interaction between two features
#   cauda.optimize()      - recommend optimal settings for controllable inputs
#   cauda.independence()  - conditional independence tests between variables
#   cauda.analyze()       - full pipeline in one command
#   cauda.extract()       - extract causal claims from text using LLM
#   cauda.extract_pdf()   - extract causal claims from a PDF paper
#   cauda.claims_to_dag() - convert claims dataframe into a bnlearn DAG
#   cauda.validate_dag()  - compare extracted DAG to ground truth
#   cauda.dag_theory()    - plot extracted DAG with pathway colors & edge styles
#   cauda.save()          - save any cauda plot to high-res PNG
#
# =============================================================================
# QUICK START EXAMPLES
# =============================================================================
#
# Load and prep data:
#   df <- read.csv("your_data.csv", stringsAsFactors = FALSE)
#   df_ready <- cauda.prep(df)
#
# Full analysis in one command:
#   results <- cauda.analyze(df, highlight = "YourTarget")
#
# Learn and edit a DAG:
#   dag <- cauda.dag(df_ready, highlight = "YourTarget")
#   dag <- cauda.add(dag, "VarA", "VarB")
#   dag <- cauda.delete(dag, "VarC", "VarD")
#   dag <- cauda.flip(dag, "VarE", "VarF")
#
# Correlation analysis:
#   cauda.corr(df_ready, highlight = "YourTarget")
#   cauda.pcorr(df_ready, highlight = "YourTarget")
#
# Consensus DAG across 4 algorithms:
#   cauda.consensus(df_ready, highlight = "YourTarget")
#
# PDP / ICE / ALE plots:
#   cauda.pdp(df_ready, target = "YourTarget", feature = "VarA", method = "pdp")
#   cauda.pdp(df_ready, target = "YourTarget", feature = "VarA", method = "ice")
#   cauda.pdp2d(df_ready, target = "YourTarget", feature1 = "VarA", feature2 = "VarB")
#
# Decision optimization:
#   cauda.optimize(df_ready, target = "YourTarget",
#                  controls = c("VarA", "VarB", "VarC"),
#                  actionable = c("VarA", "VarB"),
#                  maximize = FALSE)
#
# Conditional independence tests:
#   cauda.independence(df_ready, target = "YourTarget")
#
# Run unit tests:
#   source("cauda_tests.R")
# =============================================================================


# =============================================================================
# cauda.missing()
# Summarizes missing values in a raw data frame BEFORE any cleaning.
# Helps the user understand what needs to be handled and why.
#
# Arguments:
#   df     : any raw data frame
#   thresh : flag variables above this % missing (default 5%)
# =============================================================================

cauda.missing <- function(df, thresh = 5) {

  if (!is.data.frame(df)) stop("Input must be a data frame.")

  df_check          <- df
  df_check[df_check == ""] <- NA

  na_counts <- colSums(is.na(df_check))
  na_pct    <- round(colMeans(is.na(df_check)) * 100, 2)

  result <- data.frame(
    Variable        = names(df_check),
    Missing_Count   = as.integer(na_counts),
    Missing_Percent = na_pct,
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$Missing_Count), ]

  cat("==========================================\n")
  cat("  cauda: Missing Value Summary\n")
  cat("==========================================\n")
  cat("  Total rows      :", nrow(df_check), "\n")
  cat("  Total columns   :", ncol(df_check), "\n")
  cat("  Columns with NAs:", sum(na_counts > 0), "\n\n")

  cols_with_na <- result[result$Missing_Count > 0, ]
  if (nrow(cols_with_na) > 0) {
    print(cols_with_na, row.names = FALSE)
  } else {
    cat("  No missing values found.\n")
  }

  flagged <- result[result$Missing_Percent >= thresh, ]
  if (nrow(flagged) > 0) {
    cat(paste0("\n  Warning: ", nrow(flagged),
               " variable(s) above ", thresh, "% missing: ",
               paste(flagged$Variable, collapse = ", "), "\n"))
  } else {
    cat("\n  All variables below", thresh, "% missing threshold.\n")
  }
  cat("==========================================\n")

  return(invisible(result))
}


# =============================================================================
# cauda.recode()
# Converts character/factor variables to numeric automatically.
#
# Rules applied (in order):
#   Yes/No          -> 1/0
#   Male/Female     -> 1/0  (Male = 1)
#   True/False      -> 1/0
#   2-level factor  -> 1/0  (alphabetical: second level = 1)
#   Multi-level     -> integer codes
#   Already numeric -> unchanged
#   High-cardinality (> max_levels unique values) -> dropped
#
# Arguments:
#   df         : raw data frame
#   max_levels : columns with more unique values than this are dropped (default 10)
#   verbose    : print a log of what happened to each column
# =============================================================================

cauda.recode <- function(df, max_levels = 10, verbose = TRUE) {

  if (!is.data.frame(df)) stop("Input must be a data frame.")

  recode_log <- list()
  df_out     <- df

  for (col in names(df)) {

    x <- df[[col]]

    # Already numeric - leave alone
    if (is.numeric(x)) {
      recode_log[[col]] <- "kept as numeric"
      next
    }

    x_char   <- trimws(as.character(x))
    n_unique <- length(unique(x_char[!is.na(x_char)]))

    # Drop high-cardinality columns (IDs, free text)
    if (n_unique > max_levels) {
      df_out[[col]] <- NULL
      recode_log[[col]] <- paste0("DROPPED - ", n_unique, " unique values")
      if (verbose) cat("  Dropped  :", col, "(", n_unique, "unique values )\n")
      next
    }

    vals       <- unique(x_char[!is.na(x_char)])
    vals_lower <- tolower(vals)

    # Yes/No -> 1/0
    if (setequal(vals_lower, c("yes", "no"))) {
      df_out[[col]] <- ifelse(tolower(x_char) == "yes", 1L, 0L)
      recode_log[[col]] <- "Yes/No -> 1/0"
      if (verbose) cat("  Recoded  :", col, "( Yes/No -> 1/0 )\n")
      next
    }

    # Male/Female -> 1/0
    if (setequal(vals_lower, c("male", "female"))) {
      df_out[[col]] <- ifelse(tolower(x_char) == "male", 1L, 0L)
      recode_log[[col]] <- "Male/Female -> 1/0 (Male=1)"
      if (verbose) cat("  Recoded  :", col, "( Male/Female -> 1/0 )\n")
      next
    }

    # True/False -> 1/0
    if (setequal(vals_lower, c("true", "false"))) {
      df_out[[col]] <- ifelse(tolower(x_char) == "true", 1L, 0L)
      recode_log[[col]] <- "True/False -> 1/0"
      if (verbose) cat("  Recoded  :", col, "( True/False -> 1/0 )\n")
      next
    }

    # Any other 2-level variable -> 1/0 alphabetical
    if (n_unique == 2) {
      lvls <- sort(unique(x_char))
      df_out[[col]] <- ifelse(x_char == lvls[2], 1L, 0L)
      recode_log[[col]] <- paste0("2-level -> 1/0 (", lvls[1], "=0, ", lvls[2], "=1)")
      if (verbose) cat("  Recoded  :", col, "(", lvls[1], "=0,", lvls[2], "=1 )\n")
      next
    }

    # Multi-level -> integer codes
    f             <- factor(x_char)
    df_out[[col]] <- as.integer(f)
    level_map     <- paste(seq_along(levels(f)), levels(f), sep = "=", collapse = ", ")
    recode_log[[col]] <- paste0("multi-level -> integer ( ", level_map, " )")
    if (verbose) cat("  Recoded  :", col, "( multi-level:", level_map, ")\n")
  }

  attr(df_out, "recode_log") <- recode_log

  if (verbose) {
    dropped <- sum(sapply(recode_log, function(x) grepl("DROPPED", x)))
    cat("\ncauda.recode() complete.\n")
    cat("  Original columns :", ncol(df), "\n")
    cat("  Output columns   :", ncol(df_out), "\n")
    cat("  Columns dropped  :", dropped, "\n")
  }

  return(df_out)
}


# =============================================================================
# cauda.clean()
# Removes uninformative and redundant columns, handles missing values.
#
# Arguments:
#   df         : recoded data frame (output of cauda.recode())
#   nzv_thresh : variance threshold — columns below this are dropped (default 0.01)
#   cor_thresh : correlation threshold — redundant columns above this dropped (default 0.95)
#   na_action  : "omit" (drop rows with NAs) or "impute" (fill with column medians)
#   verbose    : print a summary of what was removed
# =============================================================================

cauda.clean <- function(df,
                        nzv_thresh = 0.01,
                        cor_thresh = 0.95,
                        na_action  = "omit",
                        verbose    = TRUE) {

  if (!is.data.frame(df)) stop("Input must be a data frame.")
  if (!na_action %in% c("omit", "impute")) stop("na_action must be 'omit' or 'impute'.")

  n_start <- ncol(df)
  dropped <- c()

  # Step 1 - Near-zero variance
  variances <- sapply(df, function(x) var(as.numeric(x), na.rm = TRUE))
  nzv_cols  <- names(variances[!is.na(variances) & variances < nzv_thresh])

  if (length(nzv_cols) > 0) {
    df      <- df[, !names(df) %in% nzv_cols, drop = FALSE]
    dropped <- c(dropped, nzv_cols)
    if (verbose) cat("  Dropped (near-zero variance):", paste(nzv_cols, collapse = ", "), "\n")
  }

  # Step 2 - Highly correlated / redundant columns
  num_cols <- names(df)[sapply(df, is.numeric)]

  if (length(num_cols) > 1) {
    cor_mat  <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
    cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA

    high_cor  <- which(abs(cor_mat) >= cor_thresh, arr.ind = TRUE)
    redundant <- c()

    if (nrow(high_cor) > 0) {
      for (i in seq_len(nrow(high_cor))) {
        col_to_drop <- rownames(cor_mat)[high_cor[i, 1]]
        if (!col_to_drop %in% redundant) {
          redundant <- c(redundant, col_to_drop)
          if (verbose) {
            col_kept <- colnames(cor_mat)[high_cor[i, 2]]
            cat("  Dropped (redundant with", col_kept, "):", col_to_drop, "\n")
          }
        }
      }
      df      <- df[, !names(df) %in% redundant, drop = FALSE]
      dropped <- c(dropped, redundant)
    }
  }

  # Step 3 - Missing values
  na_count <- sum(is.na(df))
  if (na_count > 0) {
    if (na_action == "omit") {
      n_before <- nrow(df)
      df       <- na.omit(df)
      if (verbose) cat("  Removed", n_before - nrow(df), "row(s) with missing values.\n")
    } else if (na_action == "impute") {
      if (verbose) cat("  Imputing", na_count, "missing value(s) with column medians.\n")
      for (col in names(df)) {
        if (any(is.na(df[[col]]))) {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        }
      }
    }
  } else {
    if (verbose) cat("  No missing values found.\n")
  }

  if (verbose) {
    cat("\ncauda.clean() complete.\n")
    cat("  Columns before :", n_start, "\n")
    cat("  Columns after  :", ncol(df), "\n")
    cat("  Columns dropped:", length(dropped), "\n")
    cat("  Rows remaining :", nrow(df), "\n")
  }

  return(df)
}


# =============================================================================
# cauda.prep()
# Convenience wrapper: runs cauda.recode() then cauda.clean() in one step.
# This is what the user calls to get a dataset ready for analysis.
#
# Usage:
#   df_ready <- cauda.prep(df)
# =============================================================================

cauda.prep <- function(df,
                       max_levels = 10,
                       nzv_thresh = 0.01,
                       cor_thresh = 0.95,
                       na_action  = "omit",
                       verbose    = TRUE) {

  cat("=== cauda.prep: Step 1 - Recoding ===\n")
  df_recoded <- cauda.recode(df, max_levels = max_levels, verbose = verbose)

  cat("\n=== cauda.prep: Step 2 - Cleaning ===\n")
  df_clean   <- cauda.clean(df_recoded,
                            nzv_thresh = nzv_thresh,
                            cor_thresh = cor_thresh,
                            na_action  = na_action,
                            verbose    = verbose)
  return(df_clean)
}


# =============================================================================
# cauda.dag()
# Learns a Bayesian network (DAG) from a cleaned data frame and plots it.
# Uses bnlearn's Hill-Climbing algorithm to discover dependency structure.
#
# Arguments:
#   df        : cleaned numeric data frame (output of cauda.prep())
#   highlight : column name to highlight in red (e.g. "Churn")
#   verbose   : print a summary of nodes and edges
# =============================================================================

cauda.dag <- function(df, highlight = NULL, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) stop("Please install bnlearn: install.packages('bnlearn')")
  if (!requireNamespace("igraph",  quietly = TRUE)) stop("Please install igraph: install.packages('igraph')")


  df_bn <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_bn <- na.omit(df_bn)

  if (verbose) cat("Learning DAG from", ncol(df_bn), "variables and", nrow(df_bn), "rows...\n")

  set.seed(42)
  dag <- bnlearn::hc(df_bn)

  arcs       <- bnlearn::arcs(dag)
  node_names <- bnlearn::nodes(dag)

  g <- igraph::make_empty_graph(n = length(node_names), directed = TRUE)
  g <- igraph::set_vertex_attr(g, "name", value = node_names)

  if (nrow(arcs) > 0) {
    edges <- as.vector(t(arcs))
    g     <- igraph::add_edges(g, match(edges, node_names))
  }

  node_colors <- rep("#4A90D9", length(node_names))
  if (!is.null(highlight) && highlight %in% node_names) {
    node_colors[node_names == highlight] <- "#E84545"
  }

  # Wrap long labels at camelCase boundaries
  wrap_label <- function(x, max_chars = 10) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(node_names, wrap_label)

  # Use graphopt layout which naturally spaces nodes based on connection weight
  set.seed(42)
  layout_coords <- igraph::layout_with_graphopt(g, niter = 2000, 
                                                charge = 0.08,
                                                spring.length = 0,
                                                spring.constant = 0.01)
  layout_coords <- igraph::norm_coords(layout_coords, 
                                       xmin = -1, xmax = 1, 
                                       ymin = -1, ymax = 1)

  par(mar = c(1, 1, 3, 1), bg = "white")

  plot(
    g,
    layout             = layout_coords,
    vertex.label       = wrapped_labels,
    vertex.size        = 20,
    vertex.color       = adjustcolor(node_colors, alpha.f = 0.88),
    vertex.label.cex   = 0.44,
    vertex.label.color = "black",
    vertex.label.font  = 2,
    vertex.frame.color = "black",
    vertex.frame.width = 1.5,
    edge.arrow.size    = 0.4,
    edge.color         = adjustcolor("#444444", alpha.f = 0.6),
    edge.curved        = 0.2,
    edge.width         = 0.5,
    rescale            = FALSE,
    asp                = 0,
    xlim               = c(-1.2, 1.2),
    ylim               = c(-1.2, 1.2),
    main               = "cauda: Learned Causal Network (DAG)"
  )

  if (verbose) {
    cat("\nDAG learned successfully.\n")
    cat("  Nodes:", length(node_names), "\n")
    cat("  Edges:", nrow(arcs), "\n")
    if (nrow(arcs) > 0) {
      cat("\nDependency arrows:\n")
      for (i in seq_len(nrow(arcs))) {
        cat("  ", arcs[i, 1], "->", arcs[i, 2], "\n")
      }
    }
  }

  return(invisible(dag))
}


# =============================================================================
# cauda.corr()
# Produces two correlation visualizations:
#   (1) A corrplot heatmap showing pairwise correlations
#   (2) A qgraph spring-layout correlation network
#
# Arguments:
#   df        : cleaned numeric data frame (output of cauda.prep())
#   threshold : minimum absolute correlation to show in network (default 0.15)
#   highlight : column to highlight in red (e.g. "Churn")
#   verbose   : print top correlations to console
# =============================================================================

cauda.corr <- function(df, threshold = 0.15, highlight = NULL, verbose = TRUE) {

  if (!requireNamespace("corrplot", quietly = TRUE)) stop("Please install corrplot: install.packages('corrplot')")
  if (!requireNamespace("qgraph",   quietly = TRUE)) stop("Please install qgraph: install.packages('qgraph')")


  df_num  <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  cor_mat <- cor(df_num, use = "pairwise.complete.obs")

  # Plot 1 - corrplot heatmap (full window)
  par(mfrow = c(1, 1))
  corrplot(
    cor_mat,
    method      = "color",
    type        = "upper",
    order       = "hclust",
    addCoef.col = "black",
    number.cex  = 0.32,
    tl.cex      = 0.45,
    tl.col      = "black",
    tl.srt      = 45,
    col         = colorRampPalette(c("#E84545", "white", "#4A90D9"))(200),
    title       = "cauda: Correlation Heatmap",
    mar         = c(0, 0, 2, 0)
  )

  # Plot 2 - correlation network with same cosmetics as DAG
  node_colors <- rep("#4A90D9", ncol(cor_mat))
  names(node_colors) <- colnames(cor_mat)
  if (!is.null(highlight) && highlight %in% colnames(cor_mat)) {
    node_colors[highlight] <- "#E84545"
  }

  # Wrap long labels same as DAG
  wrap_label <- function(x, max_chars = 10) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(colnames(cor_mat), wrap_label)

  par(mar = c(2, 1, 4, 1))
  qgraph(
    cor_mat,
    layout        = "spring",
    minimum       = threshold,
    cut           = 0.3,
    labels        = wrapped_labels,
    label.scale   = FALSE,
    vsize         = 7,
    label.cex     = 0.52,
    color         = adjustcolor(node_colors, alpha.f = 0.88),
    posCol        = "#4A90D9",
    negCol        = "#E84545",
    border.color  = "black",
    border.width  = 1.5,
    label.color   = "black",
    label.font    = 2,
    diag          = FALSE,
    repulsion     = 0.8
  )
  title("cauda: Correlation Network", adj = 0.5, font.main = 2, cex.main = 1.3, line = 2)

  if (verbose) {
    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "Correlation")
    cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]
    cor_df <- cor_df[!duplicated(apply(cor_df[, 1:2], 1, function(x) paste(sort(x), collapse = "-"))), ]
    cor_df$AbsCorr <- abs(cor_df$Correlation)
    cor_df <- cor_df[order(-cor_df$AbsCorr), ]

    cat("\nTop 10 strongest correlations:\n")
    print(head(cor_df[, c("Var1", "Var2", "Correlation")], 10), row.names = FALSE)

    if (!is.null(highlight) && highlight %in% colnames(cor_mat)) {
      cat(paste0("\nCorrelations with ", highlight, " (sorted):\n"))
      hi_cor <- cor_mat[highlight, ]
      hi_cor <- sort(hi_cor[names(hi_cor) != highlight], decreasing = TRUE)
      print(round(hi_cor, 3))
    }
  }

  return(invisible(cor_mat))
}


# =============================================================================
# cauda.pcorr()
# Computes and visualizes partial correlations.
# Partial correlations show relationships between variables AFTER removing
# the influence of all other variables — much closer to actual causal signal.
#
# Arguments:
#   df        : cleaned numeric data frame (output of cauda.prep())
#   threshold : minimum absolute partial correlation to show (default 0.05)
#   highlight : column to highlight in red (e.g. "Churn")
#   verbose   : print top partial correlations to console
# =============================================================================

cauda.pcorr <- function(df, threshold = 0.05, highlight = NULL, verbose = TRUE) {

  if (!requireNamespace("ppcor",  quietly = TRUE)) stop("Please install ppcor: install.packages('ppcor')")
  if (!requireNamespace("qgraph", quietly = TRUE)) stop("Please install qgraph: install.packages('qgraph')")


  df_num    <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_num    <- na.omit(df_num)

  # Remove near-constant columns that cause collinearity issues
  variances <- sapply(df_num, var, na.rm = TRUE)
  df_num    <- df_num[, variances > 1e-6, drop = FALSE]
  col_names <- colnames(df_num)

  pcor_result <- suppressWarnings(ppcor::pcor(df_num))
  pcor_mat    <- pcor_result$estimate

  # Restore names (ppcor sometimes drops them)
  rownames(pcor_mat) <- col_names
  colnames(pcor_mat) <- col_names

  # Node colors
  node_colors <- rep("#4A90D9", ncol(pcor_mat))
  names(node_colors) <- col_names
  if (!is.null(highlight) && highlight %in% col_names) {
    node_colors[highlight] <- "#E84545"
  }

  # Wrap long labels same as DAG
  wrap_label <- function(x, max_chars = 10) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(col_names, wrap_label)

  par(oma = c(0, 0, 2.5, 0), mar = c(1, 1, 0, 1))
  qgraph(
    pcor_mat,
    layout        = "spring",
    minimum       = 0.08,
    cut           = 0.15,
    labels        = wrapped_labels,
    label.scale   = FALSE,
    vsize         = 7,
    label.cex     = 0.52,
    color         = adjustcolor(node_colors, alpha.f = 0.88),
    posCol        = "#4A90D9",
    negCol        = "#E84545",
    border.color  = "black",
    border.width  = 1.5,
    label.color   = "black",
    label.font    = 2,
    diag          = FALSE,
    repulsion     = 0.9,
    maximum       = 0.4,
    esize         = 6
  )
  mtext("cauda: Partial Correlation Network", side = 3, line = 1, 
        outer = TRUE, font = 2, cex = 1.3)
  par(oma = c(0,0,0,0))

  if (verbose) {
    pc_df <- as.data.frame(as.table(pcor_mat))
    names(pc_df) <- c("Var1", "Var2", "Partial_Cor")
    pc_df <- pc_df[pc_df$Var1 != pc_df$Var2, ]
    pc_df <- pc_df[!duplicated(apply(pc_df[, 1:2], 1, function(x) paste(sort(x), collapse = "-"))), ]
    pc_df$AbsCor <- abs(pc_df$Partial_Cor)
    pc_df <- pc_df[order(-pc_df$AbsCor), ]

    cat("\nTop 10 strongest partial correlations:\n")
    print(head(pc_df[, c("Var1", "Var2", "Partial_Cor")], 10), row.names = FALSE)

    if (!is.null(highlight) && highlight %in% col_names) {
      cat(paste0("\nPartial correlations with ", highlight, " (sorted):\n"))
      hi_cor <- pcor_mat[highlight, ]
      hi_cor <- sort(hi_cor[names(hi_cor) != highlight], decreasing = TRUE)
      print(round(hi_cor, 3))
    }
  }

  return(invisible(pcor_mat))
}


# =============================================================================
# cauda.analyze()
# Main entry point. Runs the full pipeline in one command:
#   1. Missing value summary
#   2. Recode + clean
#   3. Causal DAG
#   4. Correlation heatmap + network
#   5. Partial correlation network
#
# Usage:
#   results <- cauda.analyze(df, highlight = "Churn")
#
# Arguments:
#   df        : raw data frame (recoded and cleaned automatically)
#   highlight : column to highlight across all outputs (e.g. "Churn")
#   verbose   : print summaries throughout
# =============================================================================

cauda.analyze <- function(df, highlight = NULL, verbose = TRUE) {

  cat("=============================================\n")
  cat("  cauda: Causal Automated Data Analysis\n")
  cat("=============================================\n\n")

  # Step 1 - Missing value summary
  cat("--- Step 1: Missing Value Summary ---\n")
  cauda.missing(df)

  # Step 2 - Prep
  cat("\n--- Step 2: Recoding and Cleaning ---\n")
  df_ready <- cauda.prep(df, verbose = verbose)

  # Step 3 - DAG
  cat("\n--- Step 3: Learning Causal Network (DAG) ---\n")
  dag <- cauda.dag(df_ready, highlight = highlight, verbose = verbose)
  readline(prompt = "\nPress [Enter] to continue to correlation heatmap...")

  # Step 4 - Correlations
  cat("\n--- Step 4: Correlation Heatmap and Network ---\n")
  cor_mat <- cauda.corr(df_ready, highlight = highlight, verbose = verbose)
  readline(prompt = "\nPress [Enter] to continue to partial correlation network...")

  # Step 5 - Partial Correlations
  cat("\n--- Step 5: Partial Correlation Network ---\n")
  pcor_mat <- cauda.pcorr(df_ready, highlight = highlight, verbose = verbose)

  # Final summary
  cat("\n=============================================\n")
  cat("  cauda.analyze() complete.\n")
  cat("  Dataset  :", nrow(df_ready), "rows,", ncol(df_ready), "variables\n")
  if (!is.null(highlight)) cat("  Target   :", highlight, "\n")
  cat("  Outputs  : missing value summary, DAG,\n")
  cat("             correlation heatmap + network,\n")
  cat("             partial correlation network\n")
  cat("=============================================\n")

  return(invisible(list(
    data     = df_ready,
    dag      = dag,
    cor_mat  = cor_mat,
    pcor_mat = pcor_mat
  )))
}


# =============================================================================
# DAG EDITING FUNCTIONS
# After learning a DAG with cauda.dag(), the user can manually refine it.
# All three functions return the updated DAG and replot it automatically.
#
# Usage:
#   dag <- cauda.dag(df_ready, highlight = "Churn")
#   dag <- cauda.add(dag, "tenure", "Churn")
#   dag <- cauda.delete(dag, "gender", "Churn")
#   dag <- cauda.flip(dag, "TotalRevenue", "Churn")
# =============================================================================

# Helper: replot a DAG after editing
.cauda.plot_dag <- function(dag, highlight = NULL) {


  arcs       <- bnlearn::arcs(dag)
  node_names <- bnlearn::nodes(dag)

  g <- igraph::make_empty_graph(n = length(node_names), directed = TRUE)
  g <- igraph::set_vertex_attr(g, "name", value = node_names)

  if (nrow(arcs) > 0) {
    edges <- as.vector(t(arcs))
    g     <- igraph::add_edges(g, match(edges, node_names))
  }

  node_colors <- rep("#4A90D9", length(node_names))
  if (!is.null(highlight) && highlight %in% node_names) {
    node_colors[node_names == highlight] <- "#E84545"
  }

  wrap_label <- function(x, max_chars = 10) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(node_names, wrap_label)

  set.seed(42)
  if (igraph::ecount(g) > 0) {
    layout_coords <- igraph::layout_with_graphopt(g, niter = 2000,
                                                  charge = 0.08,
                                                  spring.length = 0,
                                                  spring.constant = 0.01)
  } else {
    layout_coords <- igraph::layout_with_fr(g, niter = 2000)
  }
  layout_coords <- igraph::norm_coords(layout_coords,
                                       xmin = -1, xmax = 1,
                                       ymin = -1, ymax = 1)

  par(mar = c(1, 1, 3, 1), bg = "white")

  plot(
    g,
    layout             = layout_coords,
    vertex.label       = wrapped_labels,
    vertex.size        = 20,
    vertex.color       = adjustcolor(node_colors, alpha.f = 0.88),
    vertex.label.cex   = 0.44,
    vertex.label.color = "black",
    vertex.label.font  = 2,
    vertex.frame.color = "black",
    vertex.frame.width = 1.5,
    edge.arrow.size    = 0.4,
    edge.color         = adjustcolor("#444444", alpha.f = 0.6),
    edge.curved        = 0.2,
    edge.width         = 0.5,
    rescale            = FALSE,
    asp                = 0,
    xlim               = c(-1.2, 1.2),
    ylim               = c(-1.2, 1.2),
    main               = "cauda: Edited Causal Network (DAG)"
  )
}

# ------------------------------------------------------------------------------
# cauda.add(dag, x, y)
# Adds an arrow from x to y in the DAG and replots it.
# ------------------------------------------------------------------------------
cauda.add <- function(dag, x, y, highlight = NULL) {


  if (!x %in% bnlearn::nodes(dag)) stop(paste("Node not found:", x))
  if (!y %in% bnlearn::nodes(dag)) stop(paste("Node not found:", y))

  # Check if arc already exists
  existing <- bnlearn::arcs(dag)
  if (any(existing[, 1] == x & existing[, 2] == y)) {
    cat("Arrow", x, "->", y, "already exists.\n")
    return(invisible(dag))
  }

  dag_new <- bnlearn::set.arc(dag, from = x, to = y)
  cat("Added arrow:", x, "->", y, "\n")
  cat("Total edges:", nrow(bnlearn::arcs(dag_new)), "\n")

  .cauda.plot_dag(dag_new, highlight = highlight)
  return(invisible(dag_new))
}

# ------------------------------------------------------------------------------
# cauda.delete(dag, x, y)
# Removes the arrow from x to y in the DAG and replots it.
# ------------------------------------------------------------------------------
cauda.delete <- function(dag, x, y, highlight = NULL) {


  if (!x %in% bnlearn::nodes(dag)) stop(paste("Node not found:", x))
  if (!y %in% bnlearn::nodes(dag)) stop(paste("Node not found:", y))

  existing <- bnlearn::arcs(dag)
  if (!any(existing[, 1] == x & existing[, 2] == y)) {
    cat("No arrow from", x, "to", y, "found in DAG.\n")
    return(invisible(dag))
  }

  dag_new <- bnlearn::drop.arc(dag, from = x, to = y)
  cat("Deleted arrow:", x, "->", y, "\n")
  cat("Total edges:", nrow(bnlearn::arcs(dag_new)), "\n")

  .cauda.plot_dag(dag_new, highlight = highlight)
  return(invisible(dag_new))
}

# ------------------------------------------------------------------------------
# cauda.flip(dag, x, y)
# Reverses the direction of the arrow between x and y and replots it.
# ------------------------------------------------------------------------------
cauda.flip <- function(dag, x, y, highlight = NULL) {


  if (!x %in% bnlearn::nodes(dag)) stop(paste("Node not found:", x))
  if (!y %in% bnlearn::nodes(dag)) stop(paste("Node not found:", y))

  existing <- bnlearn::arcs(dag)

  # Check which direction the arc exists in
  if (any(existing[, 1] == x & existing[, 2] == y)) {
    dag_new <- bnlearn::drop.arc(dag, from = x, to = y)
    dag_new <- bnlearn::set.arc(dag_new, from = y, to = x)
    cat("Flipped arrow:", x, "->", y, "is now", y, "->", x, "\n")
  } else if (any(existing[, 1] == y & existing[, 2] == x)) {
    dag_new <- bnlearn::drop.arc(dag, from = y, to = x)
    dag_new <- bnlearn::set.arc(dag_new, from = x, to = y)
    cat("Flipped arrow:", y, "->", x, "is now", x, "->", y, "\n")
  } else {
    cat("No arrow found between", x, "and", y, "\n")
    return(invisible(dag))
  }

  cat("Total edges:", nrow(bnlearn::arcs(dag_new)), "\n")
  .cauda.plot_dag(dag_new, highlight = highlight)
  return(invisible(dag_new))
}


# =============================================================================
# cauda.pdp()
# Generates PDP, ICE, and ALE plots showing how a feature affects an outcome.
# Fits a random forest model internally and uses iml for visualization.
#
# Arguments:
#   df      : cleaned numeric data frame (output of cauda.prep())
#   target  : column name of the outcome variable (e.g. "Churn")
#   feature : column name of the feature to inspect (e.g. "tenure")
#   method  : "pdp", "ice", or "ale" (default "pdp")
# =============================================================================

cauda.pdp <- function(df, target, feature, method = "pdp") {

  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Please install randomForest: install.packages('randomForest')")
  if (!requireNamespace("iml",          quietly = TRUE)) stop("Please install iml: install.packages('iml')")


  if (!target  %in% names(df)) stop(paste("Target not found:", target))
  if (!feature %in% names(df)) stop(paste("Feature not found:", feature))
  if (!method  %in% c("pdp", "ice", "ale")) stop("method must be 'pdp', 'ice', or 'ale'")

  df_model <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_model <- na.omit(df_model)

  X <- df_model[, setdiff(names(df_model), target), drop = FALSE]
  y <- df_model[[target]]

  cat("Fitting random forest for", target, "~", "...\n")
  set.seed(42)
  rf <- suppressWarnings(randomForest::randomForest(x = X, y = y, ntree = 300))

  predictor <- iml::Predictor$new(model = rf, data = X, y = y)

  cat("Generating", toupper(method), "plot for feature:", feature, "\n")
  eff <- suppressWarnings(iml::FeatureEffect$new(predictor, feature = feature, method = method))

  p <- suppressWarnings(plot(eff)) +
    ggplot2::labs(
      title    = paste0("cauda: ", toupper(method), " Plot"),
      subtitle = paste0("Feature: ", feature, "  |  Target: ", target),
      x        = feature,
      y        = paste("Predicted", target)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0.5),
      axis.title    = ggplot2::element_text(face = "bold"),
      axis.text     = ggplot2::element_text(face = "bold"),
      plot.margin   = ggplot2::margin(20, 40, 20, 40)
    )

  print(p)
  return(invisible(eff))
}


# =============================================================================
# cauda.consensus()
# Runs multiple bnlearn causal discovery algorithms and plots a consensus DAG.
# Arrows that more algorithms agree on are drawn thicker and darker,
# giving the user a sense of how robust each dependency is.
#
# Algorithms used: hc, tabu, iamb, fast.iamb
#
# Arguments:
#   df        : cleaned numeric data frame (output of cauda.prep())
#   highlight : column to highlight in red (e.g. "Churn")
#   verbose   : print agreement summary
# =============================================================================

cauda.consensus <- function(df, highlight = NULL, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) stop("Please install bnlearn: install.packages('bnlearn')")
  if (!requireNamespace("igraph",  quietly = TRUE)) stop("Please install igraph: install.packages('igraph')")


  df_bn <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_bn <- na.omit(df_bn)

  # Convert to discrete for constraint-based methods
  df_disc <- as.data.frame(lapply(df_bn, function(x) {
    cut(x, breaks = 3, labels = FALSE)
  }))
  df_disc <- as.data.frame(lapply(df_disc, as.factor))

  algorithms <- list(
    hc        = function(d) bnlearn::hc(df_bn),
    tabu      = function(d) bnlearn::tabu(df_bn),
    iamb      = function(d) bnlearn::iamb(df_disc),
    fast.iamb = function(d) bnlearn::fast.iamb(df_disc)
  )

  cat("Running", length(algorithms), "causal discovery algorithms...\n")

  dag_list <- list()
  for (algo_name in names(algorithms)) {
    cat(" Running:", algo_name, "...\n")
    tryCatch({
      dag_list[[algo_name]] <- suppressWarnings(algorithms[[algo_name]](df_bn))
    }, error = function(e) {
      cat("  Skipped", algo_name, "(error).\n")
    })
  }

  cat("Algorithms completed:", length(dag_list), "/", length(algorithms), "\n\n")

  max_count <- length(dag_list)

  # Count agreement across algorithms for each directed arc
  node_names <- bnlearn::nodes(dag_list[[1]])
  arc_counts  <- list()

  for (dag in dag_list) {
    arcs <- bnlearn::arcs(dag)
    if (nrow(arcs) == 0) next
    for (i in seq_len(nrow(arcs))) {
      key <- paste(arcs[i, 1], "->", arcs[i, 2])
      arc_counts[[key]] <- (arc_counts[[key]] %||% 0) + 1
    }
  }

  # Build igraph with edge weights = number of algorithms agreeing
  g <- igraph::make_empty_graph(n = length(node_names), directed = TRUE)
  g <- igraph::set_vertex_attr(g, "name", value = node_names)

  edge_weights <- c()
  for (key in names(arc_counts)) {
    parts <- strsplit(key, " -> ")[[1]]
    from  <- parts[1]
    to    <- parts[2]
    g     <- igraph::add_edges(g, match(c(from, to), node_names))
    edge_weights <- c(edge_weights, arc_counts[[key]])
  }

  # Node colors
  node_colors <- rep("#4A90D9", length(node_names))
  if (!is.null(highlight) && highlight %in% node_names) {
    node_colors[node_names == highlight] <- "#E84545"
  }

  # Edge width and color by agreement count
  edge_widths <- (edge_weights / max_count) * 3 + 0.3
  edge_colors <- colorRampPalette(c("#CCCCCC", "#222222"))(max_count)[edge_weights]

  # Wrap labels same as main DAG
  wrap_label <- function(x, max_chars = 10) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(node_names, wrap_label)

  set.seed(42)
  layout_coords <- igraph::layout_with_graphopt(g, niter = 2000,
                                                charge = 0.08,
                                                spring.length = 0,
                                                spring.constant = 0.01)
  layout_coords <- igraph::norm_coords(layout_coords,
                                       xmin = -1, xmax = 1,
                                       ymin = -1, ymax = 1)

  par(mar = c(3, 1, 3, 1), bg = "white")

  plot(
    g,
    layout             = layout_coords,
    vertex.label       = wrapped_labels,
    vertex.size        = 20,
    vertex.color       = adjustcolor(node_colors, alpha.f = 0.88),
    vertex.label.cex   = 0.44,
    vertex.label.color = "black",
    vertex.label.font  = 2,
    vertex.frame.color = "black",
    vertex.frame.width = 1.5,
    edge.arrow.size    = 0.4,
    edge.width         = edge_widths,
    edge.color         = adjustcolor(edge_colors, alpha.f = 0.7),
    edge.curved        = 0.2,
    rescale            = FALSE,
    asp                = 0,
    xlim               = c(-1.2, 1.2),
    ylim               = c(-1.2, 1.2),
    main               = "cauda: Consensus Causal Network (DAG)"
  )

  legend("bottomleft",
    legend = paste(1:max_count, "algorithm(s) agree"),
    lwd    = (1:max_count / max_count) * 3 + 0.3,
    col    = colorRampPalette(c("#CCCCCC", "#222222"))(max_count),
    bty    = "n",
    cex    = 0.7
  )

  if (verbose) {
    cat("Top edges by algorithm agreement:\n")
    arc_df <- data.frame(
      Arrow     = names(arc_counts),
      Algorithms = unlist(arc_counts),
      stringsAsFactors = FALSE
    )
    arc_df <- arc_df[order(-arc_df$Algorithms), ]
    print(head(arc_df, 15), row.names = FALSE)
  }

  return(invisible(list(graph = g, arc_counts = arc_counts)))
}

# Null coalescing helper used in cauda.consensus
`%||%` <- function(a, b) if (!is.null(a)) a else b


# =============================================================================
# cauda.pdp2d()
# Generates a 2D partial dependence plot showing how TWO features interact
# to jointly affect the outcome. Output is a heatmap — the color at each
# point shows the predicted outcome when feature1 and feature2 take those values.
#
# Arguments:
#   df       : cleaned numeric data frame (output of cauda.prep())
#   target   : column name of the outcome variable (e.g. "Churn")
#   feature1 : first feature (x axis)
#   feature2 : second feature (y axis / color interaction)
# =============================================================================

cauda.pdp2d <- function(df, target, feature1, feature2) {

  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Please install randomForest")
  if (!requireNamespace("iml",          quietly = TRUE)) stop("Please install iml")


  if (!target   %in% names(df)) stop(paste("Target not found:", target))
  if (!feature1 %in% names(df)) stop(paste("Feature not found:", feature1))
  if (!feature2 %in% names(df)) stop(paste("Feature not found:", feature2))

  df_model <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_model <- na.omit(df_model)

  X <- df_model[, setdiff(names(df_model), target), drop = FALSE]
  y <- df_model[[target]]

  cat("Fitting random forest for", target, "...\n")
  set.seed(42)
  rf <- suppressWarnings(randomForest::randomForest(x = X, y = y, ntree = 300))

  predictor <- iml::Predictor$new(model = rf, data = X, y = y)

  cat("Generating 2D PDP for:", feature1, "x", feature2, "\n")
  eff2d <- suppressWarnings(iml::FeatureEffect$new(
    predictor,
    feature = c(feature1, feature2),
    method  = "pdp"
  ))

  p <- suppressWarnings(plot(eff2d)) +
    ggplot2::labs(
      title    = "cauda: 2D Partial Dependence Plot",
      subtitle = paste0(feature1, " x ", feature2, "  |  Target: ", target)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0.5)
    ) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title  = ggplot2::element_text(face = "bold"),
      axis.text   = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(20, 40, 20, 40)
    )

  print(p)
  return(invisible(eff2d))
}


# =============================================================================
# cauda.optimize()
# Given a causal DAG and a dataset, recommends optimal values for controllable
# input variables to maximize (or minimize) a target outcome.
# Uses a random forest to predict the outcome and a grid search over
# controllable inputs to find the best combination.
#
# Arguments:
#   df          : cleaned numeric data frame (output of cauda.prep())
#   target      : column name of the outcome to optimize (e.g. "Churn")
#   controls    : character vector of controllable input variables
#   maximize    : TRUE to maximize target, FALSE to minimize (default FALSE)
#   n_grid      : number of grid points per variable (default 10)
# =============================================================================

cauda.optimize <- function(df, target, controls, maximize = FALSE, n_grid = 10, actionable = NULL) {

  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Please install randomForest")


  if (!target %in% names(df)) stop(paste("Target not found:", target))
  for (ctrl in controls) {
    if (!ctrl %in% names(df)) stop(paste("Control variable not found:", ctrl))
  }

  # Warn if user tries to optimize non-actionable variables
  non_actionable <- c("tenure", "age", "gender", "SeniorCitizen",
                      "MaritalStatus", "Dependents")

  if (is.null(actionable)) {
    flagged <- controls[controls %in% non_actionable]
    if (length(flagged) > 0) {
      cat("Note: The following variables may not be actionable in practice:\n")
      cat(" ", paste(flagged, collapse = ", "), "\n")
      cat("Consider using actionable = c('var1', 'var2') to specify truly controllable inputs.\n\n")
    }
  } else {
    not_in_controls <- actionable[!actionable %in% controls]
    if (length(not_in_controls) > 0) {
      stop(paste("These actionable variables are not in controls:",
                 paste(not_in_controls, collapse = ", ")))
    }
    controls <- actionable
    cat("Optimizing over actionable variables only:", paste(controls, collapse = ", "), "\n\n")
  }

  df_model <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_model <- na.omit(df_model)

  X <- df_model[, setdiff(names(df_model), target), drop = FALSE]
  y <- df_model[[target]]

  cat("Fitting random forest for", target, "...\n")
  set.seed(42)
  rf <- randomForest::randomForest(x = X, y = y, ntree = 300)

  # Build grid of candidate values for each control variable
  cat("Searching over", length(controls), "control variable(s)...\n")
  grid_list <- lapply(controls, function(ctrl) {
    vals <- df_model[[ctrl]]
    seq(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE), length.out = n_grid)
  })
  names(grid_list) <- controls

  grid <- expand.grid(grid_list)

  # Fill non-control variables with their median values
  baseline <- as.data.frame(lapply(X, median, na.rm = TRUE))
  pred_data <- baseline[rep(1, nrow(grid)), , drop = FALSE]
  rownames(pred_data) <- NULL

  for (ctrl in controls) {
    pred_data[[ctrl]] <- grid[[ctrl]]
  }

  # Predict outcome for each grid point
  preds <- predict(rf, newdata = pred_data)
  grid$predicted <- preds

  # Find optimal combination
  if (maximize) {
    best_idx <- which.max(grid$predicted)
  } else {
    best_idx <- which.min(grid$predicted)
  }

  best <- grid[best_idx, ]

  cat("\n==========================================\n")
  cat("  cauda: Decision Optimization Results\n")
  cat("==========================================\n")
  cat("  Target   :", target, "\n")
  cat("  Goal     :", ifelse(maximize, "Maximize", "Minimize"), "\n")
  cat("  Controls :", paste(controls, collapse = ", "), "\n\n")
  cat("  Optimal settings:\n")
  for (ctrl in controls) {
    cat("   ", ctrl, "=", round(best[[ctrl]], 4), "\n")
  }
  cat("\n  Predicted", target, "at optimum:",
      round(best$predicted, 4), "\n")
  cat("  Baseline", target, "(median inputs):",
      round(predict(rf, newdata = baseline), 4), "\n")
  cat("==========================================\n")

  return(invisible(best))
}


# =============================================================================
# cauda.independence()
# Tests whether pairs of nodes are conditionally independent of each other
# given the values of other variables in the dataset.
# Uses bnlearn's conditional independence tests.
#
# A low p-value means the two variables are NOT independent — they are
# genuinely associated even after controlling for other variables.
#
# Arguments:
#   df        : cleaned numeric data frame (output of cauda.prep())
#   target    : if provided, only tests pairs involving this variable
#   threshold : p-value threshold for flagging significant dependencies (default 0.05)
#   verbose   : print full results table
# =============================================================================

cauda.independence <- function(df, target = NULL, threshold = 0.05, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) stop("Please install bnlearn")


  df_num <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  df_num <- na.omit(df_num)
  vars   <- colnames(df_num)

  # Determine which pairs to test
  if (!is.null(target)) {
    if (!target %in% vars) stop(paste("Target not found:", target))
    pairs <- lapply(setdiff(vars, target), function(v) c(target, v))
  } else {
    pairs <- combn(vars, 2, simplify = FALSE)
  }

  cat("Running conditional independence tests on", length(pairs), "pairs...\n\n")

  results <- data.frame(
    Var1    = character(),
    Var2    = character(),
    p_value = numeric(),
    Result  = character(),
    stringsAsFactors = FALSE
  )

  for (pair in pairs) {
    x    <- pair[1]
    y    <- pair[2]
    cond <- setdiff(vars, c(x, y))

    tryCatch({
      test   <- bnlearn::ci.test(x, y, cond, data = df_num, test = "cor")
      pval   <- test$p.value
      result <- ifelse(pval < threshold, "DEPENDENT", "independent")

      results <- rbind(results, data.frame(
        Var1    = x,
        Var2    = y,
        p_value = round(pval, 4),
        Result  = result,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) NULL)
  }

  results <- results[order(results$p_value), ]

  # Print results
  cat("==========================================\n")
  cat("  cauda: Conditional Independence Tests\n")
  cat("==========================================\n")
  cat("  Pairs tested   :", nrow(results), "\n")
  cat("  Dependent pairs:", sum(results$Result == "DEPENDENT"), "\n")
  cat("  Threshold      : p <", threshold, "\n\n")

  if (verbose) {
    if (!is.null(target)) {
      cat(paste0("Results for pairs involving ", target, ":\n"))
    } else {
      cat("Top 20 most dependent pairs:\n")
    }
    print(head(results, 20), row.names = FALSE)
  }

  cat("==========================================\n")
  return(invisible(results))
}


# =============================================================================
# CAUSAL CLAIM EXTRACTION FROM TEXT & PAPERS
# =============================================================================
# Functions for extracting causal DAGs from scientific literature via LLM APIs
#
#   cauda.extract()        - extract causal claims from text using LLM
#   cauda.extract_pdf()    - extract causal claims from a PDF paper
#   cauda.claims_to_dag()  - convert claims dataframe into a bnlearn DAG
#   cauda.validate_dag()   - compare extracted DAG to ground truth
#
# =============================================================================


# =============================================================================
# cauda.extract()
# Extract causal claims from text using OpenAI API
#
# Arguments:
#   text           : the text to analyze (character string)
#   domain         : domain/field for context (e.g. "wind energy", "opioid")
#   model          : OpenAI model to use (default "gpt-4o-mini")
#   api_key        : OpenAI API key (reads from OPENAI_API_KEY env var if NULL)
#   verbose        : print progress and API info
#
# Returns:
#   Data frame with columns:
#   - claim_type, source, target, pathway, direction, strength, confidence, 
#     established, quote
#
# =============================================================================

cauda.extract <- function(text, 
                          domain = "general", 
                          model = "gpt-4o-mini",
                          api_key = NULL, 
                          verbose = TRUE) {

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 required. Install with: install.packages('httr2')")
  }

  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }
  if (api_key == "") {
    stop("OPENAI_API_KEY not found. Set with: Sys.setenv(OPENAI_API_KEY='your-key')")
  }

  # Truncate text to 20K chars to avoid token limits
  if (nchar(text) > 20000) {
    if (verbose) cat("Text truncated to 20,000 characters.\n")
    text <- substr(text, 1, 20000)
  }

  system_prompt <- paste0(
    "You are a causal inference expert. Extract causal claims from the text.\n",
    "Return a JSON array of objects with exactly these fields:\n",
    "- claim_type: 'causal_effect' (only this type)\n",
    "- source: variable/node that causes\n",
    "- target: variable/node that is caused\n",
    "- pathway: brief mechanism name (e.g. 'wake_deficit', 'transfer_learning')\n",
    "- direction: 'positive' or 'negative'\n",
    "- strength: 'low', 'medium', or 'high'\n",
    "- confidence: 'low', 'medium', or 'high'\n",
    "- established: true if well-established, false if speculative\n",
    "- quote: exact quote from text supporting the claim\n",
    "Domain: ", domain, "\n",
    "Be thorough but only extract REAL causal claims, not correlations."
  )

  user_message <- paste0(
    "Extract all causal claims from this text:\n\n", text
  )

  if (verbose) {
    cat("Sending request to OpenAI...\n")
    cat("  Model:", model, "\n")
    cat("  Text length:", nchar(text), "chars\n")
  }

  tryCatch({
    response <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      httr2::req_body_json(list(
        model = model,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_message)
        ),
        temperature = 0.3,
        max_tokens = 2000
      )) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(response)
    content <- body$choices[[1]]$message$content

    if (verbose) cat("Response received. Parsing JSON...\n")

    # Parse JSON response
    claims_list <- tryCatch({
      jsonlite::fromJSON(content)
    }, error = function(e) {
      if (verbose) cat("Warning: Could not parse JSON. Returning empty frame.\n")
      return(data.frame())
    })

    if (length(claims_list) == 0) {
      if (verbose) cat("No claims extracted.\n")
      return(data.frame(
        claim_type = character(),
        source = character(),
        target = character(),
        pathway = character(),
        direction = character(),
        strength = character(),
        confidence = character(),
        established = logical(),
        quote = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Convert to data frame
    claims_df <- as.data.frame(do.call(rbind, claims_list), stringsAsFactors = FALSE)

    if (verbose) {
      cat("Extracted", nrow(claims_df), "claims\n")
    }

    return(claims_df)

  }, error = function(e) {
    cat("Error calling OpenAI API:\n")
    cat(conditionMessage(e), "\n")
    return(data.frame())
  })
}


# =============================================================================
# cauda.extract_pdf()
# Extract causal claims from a PDF paper
#
# Arguments:
#   pdf_path : path to PDF file
#   ...      : additional arguments passed to cauda.extract()
#
# Returns:
#   Data frame of extracted claims (same format as cauda.extract)
#
# =============================================================================

cauda.extract_pdf <- function(pdf_path, ...) {

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("pdftools required. Install with: install.packages('pdftools')")
  }

  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  cat("Loading PDF:", pdf_path, "\n")
  text <- pdftools::pdf_text(pdf_path)
  text <- paste(text, collapse = "\n")

  cat("Extracted", nchar(text), "characters from PDF.\n\n")

  cauda.extract(text, ...)
}


# =============================================================================
# cauda.claims_to_dag()
# Convert extracted claims into a bnlearn DAG object
#
# Arguments:
#   claims     : output from cauda.extract() or cauda.extract_pdf()
#   confidence_threshold : minimum confidence to include ("low", "medium", "high")
#   include_speculative : include claims marked established=FALSE
#   verbose    : print summary
#
# Returns:
#   A bnlearn bn object (directed acyclic graph) with metadata:
#   - attr(dag, "edge_metadata") : tibble of edges with pathway & evidence info
#   - attr(dag, "pathway_colors") : color mapping for visualization
#
# =============================================================================

cauda.claims_to_dag <- function(claims,
                                confidence_threshold = "low",
                                include_speculative = TRUE,
                                verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }

  if (nrow(claims) == 0) {
    stop("No claims provided.")
  }

  # Filter by confidence
  confidence_order <- c("low" = 1, "medium" = 2, "high" = 3)
  threshold_num <- confidence_order[confidence_threshold]

  claims_filtered <- claims[
    which((confidence_order[claims$confidence] >= threshold_num) &
          (include_speculative | claims$established == TRUE | is.na(claims$established))),
    , drop = FALSE
  ]

  if (nrow(claims_filtered) == 0) {
    stop("No claims meet filtering criteria.")
  }

  if (verbose) {
    cat("Filtered to", nrow(claims_filtered), "claims\n")
    cat("  - confidence >=", confidence_threshold, "\n")
    cat("  - include speculative:", include_speculative, "\n\n")
  }

  # Extract unique nodes
  sources <- unique(claims_filtered$source[!is.na(claims_filtered$source) & claims_filtered$source != ""])
  targets <- unique(claims_filtered$target[!is.na(claims_filtered$target) & claims_filtered$target != ""])
  all_nodes <- unique(c(sources, targets))

  if (verbose) {
    cat("  Nodes:", length(all_nodes), "\n")
    print(all_nodes)
    cat("\n")
  }

  # Create empty DAG
  dag <- bnlearn::empty.graph(all_nodes)

  # Add edges from causal_effect claims
  edge_metadata <- data.frame(
    from = character(),
    to = character(),
    pathway = character(),
    established = logical(),
    stringsAsFactors = FALSE
  )

  causal_claims <- claims_filtered[claims_filtered$claim_type == "causal_effect", ]

  for (i in seq_len(nrow(causal_claims))) {
    from <- causal_claims$source[i]
    to <- causal_claims$target[i]
    pathway <- causal_claims$pathway[i]
    established <- causal_claims$established[i]

    if (!is.na(from) && !is.na(to) && from != "" && to != "") {
      tryCatch({
        dag <- bnlearn::set.arc(dag, from = from, to = to)
        edge_metadata <- rbind(edge_metadata, data.frame(
          from = from,
          to = to,
          pathway = if (is.na(pathway)) "unknown" else pathway,
          established = if (is.na(established)) TRUE else established,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        if (verbose) cat("Note: Could not add edge", from, "->", to, "\n")
      })
    }
  }

  # Store metadata
  attr(dag, "edge_metadata") <- edge_metadata
  attr(dag, "pathway_colors") <- c(
    gateway = "#E84545",
    common_liability = "chartreuse4",
    structural = "royalblue3",
    behavioral = "#F2A623",
    unknown = "#888888"
  )

  if (verbose) {
    cat("=== DAG Created ===\n")
    cat("  Nodes:", length(all_nodes), "\n")
    cat("  Edges:", nrow(edge_metadata), "\n")
    cat("  Pathways:\n")
    pathway_counts <- table(edge_metadata$pathway)
    for (pw in names(pathway_counts)) {
      cat("    ", pw, ":", pathway_counts[pw], "\n")
    }
    cat("  Established:", sum(edge_metadata$established), "\n")
    cat("  Speculative:", sum(!edge_metadata$established), "\n")
    cat("================\n\n")
  }

  return(dag)
}


# =============================================================================
# cauda.validate_dag()
# Compare an extracted DAG against a ground truth DAG
#
# Arguments:
#   extracted_dag : DAG from cauda.claims_to_dag()
#   ground_truth_dag : reference DAG (also a bnlearn bn object)
#   verbose : print metrics
#
# Returns:
#   List with: precision, recall, F1, TP, FP, FN, direction_errors
#
# =============================================================================

cauda.validate_dag <- function(extracted_dag, ground_truth_dag, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }

  # Extract edges
  extracted_arcs <- bnlearn::arcs(extracted_dag)
  ground_truth_arcs <- bnlearn::arcs(ground_truth_dag)

  if (nrow(extracted_arcs) == 0) {
    if (verbose) cat("Extracted DAG has no edges.\n")
    return(list(precision = 0, recall = 0, F1 = 0, TP = 0, FP = nrow(extracted_arcs), FN = nrow(ground_truth_arcs)))
  }

  # Build edge set (from -> to)
  extracted_edges <- paste0(extracted_arcs[, "from"], " -> ", extracted_arcs[, "to"])
  ground_truth_edges <- paste0(ground_truth_arcs[, "from"], " -> ", ground_truth_arcs[, "to"])

  # True positives, false positives, false negatives
  TP <- sum(extracted_edges %in% ground_truth_edges)
  FP <- sum(!extracted_edges %in% ground_truth_edges)
  FN <- sum(!ground_truth_edges %in% extracted_edges)

  # Metrics
  precision <- if (TP + FP > 0) TP / (TP + FP) else 0
  recall <- if (TP + FN > 0) TP / (TP + FN) else 0
  F1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  if (verbose) {
    cat("=== DAG Validation ===\n")
    cat("Precision:", round(precision, 3), "(", TP, "/", TP + FP, ")\n")
    cat("Recall:", round(recall, 3), "(", TP, "/", TP + FN, ")\n")
    cat("F1:", round(F1, 3), "\n")
    cat("True Positives:", TP, "\n")
    cat("False Positives:", FP, "\n")
    cat("False Negatives:", FN, "\n")
    cat("==================\n\n")
  }

  return(list(
    precision = precision,
    recall = recall,
    F1 = F1,
    TP = TP,
    FP = FP,
    FN = FN,
    extracted_edges = extracted_edges,
    ground_truth_edges = ground_truth_edges
  ))
}


# =============================================================================
# cauda.dag_theory()
# Plot an extracted theory DAG with pathway-colored edges
# Distinguishes between established (solid) and speculative (dashed) claims
#
# Arguments:
#   dag       : bnlearn bn object from cauda.claims_to_dag()
#   highlight : node to highlight in red
#   verbose   : print summary
#
# Returns:
#   Invisible NULL (plots to screen)
#
# =============================================================================

cauda.dag_theory <- function(dag, highlight = NULL, verbose = TRUE) {

  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph required. Install with: install.packages('igraph')")
  }

  # Extract edges and metadata
  arcs <- bnlearn::arcs(dag)
  node_names <- bnlearn::nodes(dag)
  
  edge_metadata <- attr(dag, "edge_metadata")
  pathway_colors <- attr(dag, "pathway_colors")
  
  if (is.null(edge_metadata) || nrow(edge_metadata) == 0) {
    if (verbose) cat("Note: No pathway metadata found. Falling back to standard DAG plot.\n")
    return(invisible(cauda.dag(as.data.frame(matrix(0, 1, length(node_names))),
                               highlight = highlight, verbose = verbose)))
  }

  # Build igraph object
  g <- igraph::make_empty_graph(n = length(node_names), directed = TRUE)
  g <- igraph::set_vertex_attr(g, "name", value = node_names)
  
  if (nrow(arcs) > 0) {
    edges <- as.vector(t(arcs))
    g <- igraph::add_edges(g, match(edges, node_names))
  }

  # Node colors: blue default, red for highlight
  node_colors <- rep("#4A90D9", length(node_names))
  if (!is.null(highlight) && highlight %in% node_names) {
    node_colors[node_names == highlight] <- "#E84545"
  }

  # Edge colors and styles based on pathway
  edge_colors <- rep("#888888", nrow(arcs))  # default gray
  edge_styles <- rep(1, nrow(arcs))          # 1 = solid, 2 = dashed, 3 = dotted
  edge_widths <- rep(1, nrow(arcs))
  
  for (i in seq_len(nrow(arcs))) {
    from <- arcs[i, 1]
    to <- arcs[i, 2]
    
    # Find matching edge in metadata
    meta_match <- which(
      edge_metadata$from == from & edge_metadata$to == to
    )
    
    if (length(meta_match) > 0) {
      pathway <- edge_metadata$pathway[meta_match[1]]
      established <- edge_metadata$established[meta_match[1]]
      
      # Color by pathway
      if (!is.na(pathway) && pathway %in% names(pathway_colors)) {
        edge_colors[i] <- pathway_colors[[pathway]]
      }
      
      # Line type: solid if established, dashed if speculative
      if (!is.na(established)) {
        edge_styles[i] <- if (established) 1 else 2
      }
      
      # Width: established edges are thicker
      edge_widths[i] <- if (established) 1.2 else 0.6
    }
  }

  # Label wrapping
  wrap_label <- function(x, max_chars = 12) {
    if (nchar(x) <= max_chars) return(x)
    splits <- gregexpr("[A-Z]", x)[[1]]
    splits <- splits[splits > 1]
    if (length(splits) == 0) return(x)
    mid <- splits[ceiling(length(splits) / 2)]
    paste0(substr(x, 1, mid - 1), "\n", substr(x, mid, nchar(x)))
  }
  wrapped_labels <- sapply(node_names, wrap_label)

  # Layout with graphopt
  set.seed(42)
  layout_coords <- igraph::layout_with_graphopt(
    g, niter = 2000,
    charge = 0.08,
    spring.length = 0,
    spring.constant = 0.01
  )
  layout_coords <- igraph::norm_coords(
    layout_coords,
    xmin = -1, xmax = 1,
    ymin = -1, ymax = 1
  )

  par(mar = c(1, 1, 3, 1), bg = "white")

  # Plot with pathway colors and edge styles
  plot(
    g,
    layout             = layout_coords,
    vertex.label       = wrapped_labels,
    vertex.size        = 22,
    vertex.color       = adjustcolor(node_colors, alpha.f = 0.85),
    vertex.label.cex   = 0.48,
    vertex.label.color = "black",
    vertex.label.font  = 2,
    vertex.frame.color = "black",
    vertex.frame.width = 1.5,
    edge.arrow.size    = 0.5,
    edge.color         = adjustcolor(edge_colors, alpha.f = 0.75),
    edge.lty           = edge_styles,
    edge.width         = edge_widths,
    edge.curved        = 0.25,
    rescale            = FALSE,
    asp                = 0,
    xlim               = c(-1.25, 1.25),
    ylim               = c(-1.25, 1.25),
    main               = "Theory DAG: Causal Claims from Scientific Papers"
  )

  # Add legend
  legend_text <- c("Established pathway", "Speculative claim")
  legend_lty <- c(1, 2)
  legend_lwd <- c(1.2, 0.6)
  
  legend(
    "topright",
    legend = legend_text,
    lty = legend_lty,
    lwd = legend_lwd,
    col = "#444444",
    bty = "o",
    bg = adjustcolor("white", alpha.f = 0.9),
    cex = 0.9
  )

  if (verbose) {
    cat("\n=== Theory DAG (Pathway-Colored) ===\n")
    cat("Nodes:", length(node_names), "\n")
    cat("Edges:", nrow(arcs), "\n")
    
    if (!is.null(edge_metadata) && nrow(edge_metadata) > 0) {
      cat("\nPathway breakdown:\n")
      pathway_counts <- table(edge_metadata$pathway)
      for (pathway in names(pathway_counts)) {
        color <- if (pathway %in% names(pathway_colors)) pathway_colors[[pathway]] else "unknown"
        cat("  ", pathway, ":", pathway_counts[[pathway]], "\n")
      }
      cat("\nEstablished:", sum(edge_metadata$established), " | Speculative:", sum(!edge_metadata$established), "\n")
    }
    cat("================\n\n")
  }

  return(invisible(NULL))
}


# =============================================================================
# cauda.save()
# Saves any cauda plot as a high-resolution PNG file.
# Reruns the plot function inside a PNG device so the output is
# publication-quality regardless of your RStudio window size.
#
# Arguments:
#   filename : output filename (e.g. "dag.png"). Saved to working directory.
#   expr     : the cauda plot expression to run (unquoted)
#   width    : image width in pixels (default 3200)
#   height   : image height in pixels (default 2400)
#   res      : resolution in DPI (default 220)
# =============================================================================

cauda.save <- function(filename, expr, width = 3200, height = 2400, res = 220) {

  if (!grepl("\\.png$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".png")
  }

  grDevices::png(filename, width = width, height = height, res = res)

  tryCatch({
    force(expr)
  }, finally = {
    grDevices::dev.off()
  })

  cat("Saved high-resolution plot to:", filename, "\n")
  cat("  Size:", width, "x", height, "px at", res, "DPI\n")

  return(invisible(filename))
}
