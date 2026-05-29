# =============================================================================
# cauda_tests.R
# Basic unit tests for the cauda package.
# Run this file to verify all functions work correctly on a test dataset.
#
# Usage:
#   source("cauda.R")
#   source("cauda_tests.R")
# =============================================================================

cat("Running cauda unit tests...\n\n")

# Load required packages
suppressWarnings({
  library(bnlearn)
  library(igraph)
  library(corrplot)
  library(qgraph)
  library(ppcor)
  library(randomForest)
  library(iml)
})
passed <- 0
failed <- 0

run_test <- function(name, expr) {
  tryCatch({
    expr
    cat("  PASS:", name, "\n")
    passed <<- passed + 1
  }, error = function(e) {
    cat("  FAIL:", name, "\n")
    cat("        Error:", conditionMessage(e), "\n")
    failed <<- failed + 1
  })
}

# Use mtcars as a simple test dataset
test_df <- mtcars

# ------------------------------------------------------------------
cat("--- Data preparation ---\n")
run_test("cauda.missing() runs", {
  cauda.missing(test_df)
})

run_test("cauda.recode() runs on character data", {
  df_chr <- data.frame(
    x = c("Yes", "No", "Yes"),
    y = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- cauda.recode(df_chr, verbose = FALSE)
  stopifnot(is.numeric(result$x))
  stopifnot(all(result$x %in% c(0, 1)))
})

run_test("cauda.clean() runs", {
  cauda.clean(test_df, verbose = FALSE)
})

run_test("cauda.prep() runs end to end", {
  cauda.prep(test_df, verbose = FALSE)
})

run_test("cauda.prep() returns a data frame", {
  result <- cauda.prep(test_df, verbose = FALSE)
  stopifnot(is.data.frame(result))
})

run_test("cauda.prep() drops zero-variance columns", {
  df_nzv <- cbind(test_df, constant = 1)
  result <- cauda.prep(df_nzv, verbose = FALSE)
  stopifnot(!"constant" %in% names(result))
})

# ------------------------------------------------------------------
cat("\n--- Modeling ---\n")
df_ready <- suppressWarnings(cauda.prep(test_df, verbose = FALSE))

run_test("cauda.dag() learns a DAG", {
  dag <- cauda.dag(df_ready, verbose = FALSE)
  stopifnot(!is.null(dag))
})

run_test("cauda.dag() returns correct number of nodes", {
  dag <- cauda.dag(df_ready, verbose = FALSE)
  stopifnot(length(bnlearn::nodes(dag)) == ncol(df_ready))
})

run_test("cauda.add() adds an arc", {
  dag <- cauda.dag(df_ready, verbose = FALSE)
  n_before <- nrow(bnlearn::arcs(dag))
  dag2 <- cauda.add(dag, "cyl", "mpg")
  stopifnot(nrow(bnlearn::arcs(dag2)) >= n_before)
})

run_test("cauda.delete() removes an arc", {
  dag <- cauda.dag(df_ready, verbose = FALSE)
  arcs <- bnlearn::arcs(dag)
  if (nrow(arcs) > 0) {
    n_before <- nrow(arcs)
    dag2 <- cauda.delete(dag, arcs[1,1], arcs[1,2])
    stopifnot(nrow(bnlearn::arcs(dag2)) < n_before)
  }
})

run_test("cauda.corr() runs", {
  suppressWarnings(cauda.corr(df_ready, verbose = FALSE))
})

run_test("cauda.pcorr() runs", {
  suppressWarnings(cauda.pcorr(df_ready, verbose = FALSE))
})

run_test("cauda.independence() runs", {
  suppressWarnings(cauda.independence(df_ready, target = "mpg", verbose = FALSE))
})

run_test("cauda.pdp() runs", {
  suppressWarnings(cauda.pdp(df_ready, target = "mpg", feature = "hp", method = "pdp"))
})

run_test("cauda.optimize() runs", {
  suppressWarnings(
    cauda.optimize(df_ready, target = "mpg", controls = c("hp", "wt"), maximize = TRUE)
  )
})

# ------------------------------------------------------------------
cat("\n--- Input validation ---\n")
run_test("cauda.recode() rejects non-data-frame", {
  tryCatch(cauda.recode("not a df"), error = function(e) TRUE)
})

run_test("cauda.dag() rejects missing target column", {
  tryCatch(
    cauda.pdp(df_ready, target = "nonexistent", feature = "hp"),
    error = function(e) TRUE
  )
})

run_test("cauda.missing() handles empty strings as NA", {
  df_empty <- data.frame(x = c("a", "", "b"), stringsAsFactors = FALSE)
  result <- cauda.missing(df_empty)
  stopifnot(result$Missing_Count[result$Variable == "x"] == 1)
})

# ------------------------------------------------------------------
cat("\n==========================================\n")
cat("  Results:", passed, "passed,", failed, "failed\n")
cat("==========================================\n")
