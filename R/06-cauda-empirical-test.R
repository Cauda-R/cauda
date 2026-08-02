#' Test a Theory's Implications Against Real Data
#'
#' The empirical half of Cox's proposed CAUDA workflow: given a DAG's testable
#' implications (from [cauda.dagitty()]) and a real dataset, checks each
#' implication that can actually be evaluated against that data, and reports
#' whether the data is consistent with or contradicts the theory's claim.
#'
#' A DAG built from claim-extraction typically has abstract, paper-specific
#' node labels ("Purdue aggressive marketing", "opioid prescribing rates")
#' that don't literally match column names in any real dataset. `var_map`
#' bridges that gap: it's a named character vector where names are DAG
#' display labels and values are the data column that operationalizes them.
#' Only implications where every variable involved (X, Y, and everything in
#' the conditioning set Z) has a mapped data column can actually be tested;
#' the rest are reported separately as untestable, not silently dropped.
#'
#' Per Cox's explicit framing, this function performs empirical CRITICISM of
#' a theory, not confirmation. A "consistent" result means the data failed to
#' contradict that implication — it does not prove the theory correct
#' (observational tests can't do that). A "VIOLATED" result is real evidence
#' against that specific causal claim as stated.
#'
#' @param dag A bnlearn bn object with `attr(dag, "display_lookup")` set, as
#'   produced by [cauda.claims_to_dag()] or the Shiny app's DAG editor.
#' @param data A data frame containing the real-world measurements. Must
#'   include all columns referenced as values in `var_map`.
#' @param var_map Named character vector. Names are DAG display labels
#'   (matching `cauda.dagitty()`'s `use_display_names = TRUE` output); values
#'   are the corresponding column name in `data`. Example:
#'   `c("Overdose deaths" = "overdose_deaths_total", "Pill volume" = "arcos_total_pills_2006_2014")`.
#' @param type Type of implied-CI set, passed to
#'   `dagitty::impliedConditionalIndependencies()`. Default `"basis.set"`.
#' @param alpha Significance threshold for flagging an implication as
#'   violated. Default 0.05.
#' @param verbose Logical. Print a readable summary. Default `TRUE`.
#'
#' @return A list with:
#'   * `tested`: data frame of implications that had full data coverage, with
#'     columns `X`, `Y`, `Z` (display names), `data_X`/`data_Y`/`data_Z`
#'     (actual columns tested), `estimate`, `p_value`, `ci_lower`, `ci_upper`,
#'     `conclusion`.
#'   * `untestable`: data frame of implications that could NOT be tested
#'     because one or more variables have no entry in `var_map`, with a
#'     `reason` column naming the missing variable(s).
#'   * `n_tested`, `n_untestable`, `n_violated`: summary counts.
#'
#' @examples
#' \dontrun{
#'   dag <- cauda.claims_to_dag(claims)
#'   real_data <- read.csv(system.file("extdata", "opioid_real_data.csv", package = "cauda"))
#'   map <- c("Overdose deaths" = "overdose_deaths_total",
#'            "Heroin deaths"   = "heroin_deaths")
#'   result <- cauda.test_implications(dag, real_data, map)
#'   result$tested
#' }
#'
#' @export
#' @importFrom bnlearn arcs nodes
cauda.test_implications <- function(dag, data, var_map, type = "basis.set",
                                     alpha = 0.05, verbose = TRUE) {

  if (!requireNamespace("dagitty", quietly = TRUE)) {
    stop("dagitty required. Install with: install.packages('dagitty')")
  }
  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (is.null(names(var_map)) || any(names(var_map) == "") || any(is.na(names(var_map)))) {
    stop("var_map must be a named character vector: names = DAG display labels, values = data column names.")
  }

  arcs_mat       <- bnlearn::arcs(dag)
  node_names     <- bnlearn::nodes(dag)
  display_lookup <- attr(dag, "display_lookup")

  if (length(node_names) == 0) stop("DAG has no nodes.")

  # Rebuild the same dagitty model cauda.dagitty() would (sanitized names —
  # display_lookup is only used below to translate implications for matching
  # against var_map, same pattern as cauda.dagitty()).
  arc_lines <- if (nrow(arcs_mat) > 0) {
    sprintf('"%s" -> "%s"', arcs_mat[, "from"], arcs_mat[, "to"])
  } else character(0)

  connected      <- unique(c(arcs_mat[, "from"], arcs_mat[, "to"]))
  isolated_nodes <- setdiff(node_names, connected)
  isolated_lines <- if (length(isolated_nodes) > 0) sprintf('"%s"', isolated_nodes) else character(0)

  dagitty_string <- paste0(
    "dag {\n  ",
    paste(c(arc_lines, isolated_lines), collapse = "\n  "),
    "\n}"
  )
  dg <- dagitty::dagitty(dagitty_string)

  implied <- tryCatch(
    dagitty::impliedConditionalIndependencies(dg, type = type),
    error = function(e) stop(sprintf("dagitty could not derive implications: %s", conditionMessage(e)))
  )

  if (length(implied) == 0) {
    if (verbose) cat("No testable implications: the DAG is saturated or has too few nodes.\n")
    empty <- data.frame(X = character(), Y = character(), Z = character(), stringsAsFactors = FALSE)
    return(list(tested = empty, untestable = empty, n_tested = 0, n_untestable = 0, n_violated = 0))
  }

  to_display <- function(x) {
    if (!is.null(display_lookup)) {
      out <- unname(display_lookup[x])
      ifelse(is.na(out), x, out)
    } else x
  }

  testable_ci <- list()
  untestable_rows <- list()

  for (ci in implied) {
    x_disp <- to_display(ci$X)
    y_disp <- to_display(ci$Y)
    z_disp <- if (length(ci$Z) > 0) to_display(ci$Z) else character(0)

    all_disp <- c(x_disp, y_disp, z_disp)
    is_mapped <- all_disp %in% names(var_map)

    if (all(is_mapped)) {
      ci_mapped   <- ci
      ci_mapped$X <- unname(var_map[x_disp])
      ci_mapped$Y <- unname(var_map[y_disp])
      ci_mapped$Z <- if (length(z_disp) > 0) unname(var_map[z_disp]) else character(0)
      testable_ci[[length(testable_ci) + 1]] <- list(
        ci = ci_mapped, x_disp = x_disp, y_disp = y_disp, z_disp = z_disp
      )
    } else {
      untestable_rows[[length(untestable_rows) + 1]] <- data.frame(
        X = x_disp, Y = y_disp,
        Z = if (length(z_disp) > 0) paste(z_disp, collapse = ", ") else "(nothing)",
        reason = paste0("no data mapped for: ", paste(all_disp[!is_mapped], collapse = ", ")),
        stringsAsFactors = FALSE
      )
    }
  }

  untestable_df <- if (length(untestable_rows) > 0) {
    do.call(rbind, untestable_rows)
  } else {
    data.frame(X = character(), Y = character(), Z = character(), reason = character(), stringsAsFactors = FALSE)
  }
  rownames(untestable_df) <- NULL

  if (length(testable_ci) == 0) {
    if (verbose) {
      cat("=== Empirical Test of Theory's Implications ===\n")
      cat("0 of", length(implied), "implications are testable against the supplied data.\n")
      cat("Every implication needs at least one variable that has no entry in var_map.\n")
    }
    return(list(tested = data.frame(), untestable = untestable_df,
                n_tested = 0, n_untestable = nrow(untestable_df), n_violated = 0))
  }

  test_list   <- lapply(testable_ci, function(r) r$ci)
  needed_cols <- unique(unlist(lapply(test_list, function(ci) c(ci$X, ci$Y, ci$Z))))
  missing_cols <- setdiff(needed_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("var_map points to column(s) not present in data:", paste(missing_cols, collapse = ", ")))
  }

  # Real public panels (e.g. CDC VSRR) commonly have NA cells for some
  # state-years (a category wasn't reported that year). dagitty::localTests()
  # computes a single sample covariance matrix across all needed columns and
  # doesn't handle NAs itself (they propagate to NaN and break the internal
  # svd()), so drop incomplete rows here rather than let that happen deep
  # inside dagitty with a cryptic "infinite or missing values" error.
  test_data <- data[, needed_cols, drop = FALSE]
  complete  <- stats::complete.cases(test_data)
  n_dropped <- sum(!complete)
  test_data <- test_data[complete, , drop = FALSE]

  if (nrow(test_data) < 10) {
    stop(sprintf(
      "Only %d complete rows remain after dropping %d row(s) with missing values across columns [%s] -- too few to test reliably.",
      nrow(test_data), n_dropped, paste(needed_cols, collapse = ", ")
    ))
  }
  if (verbose && n_dropped > 0) {
    cat("Dropped", n_dropped, "row(s) with missing values in", paste(needed_cols, collapse = ", "),
        "-- testing on", nrow(test_data), "complete rows.\n")
  }

  lt <- dagitty::localTests(dg, test_data, type = "cis", tests = test_list)

  tested_df <- data.frame(
    X        = sapply(testable_ci, function(r) r$x_disp),
    Y        = sapply(testable_ci, function(r) r$y_disp),
    Z        = sapply(testable_ci, function(r) if (length(r$z_disp) > 0) paste(r$z_disp, collapse = ", ") else "(nothing)"),
    data_X   = sapply(test_list, function(ci) ci$X),
    data_Y   = sapply(test_list, function(ci) ci$Y),
    data_Z   = sapply(test_list, function(ci) if (length(ci$Z) > 0) paste(ci$Z, collapse = ", ") else "(nothing)"),
    estimate = lt[["estimate"]],
    p_value  = lt[["p.value"]],
    ci_lower = lt[[3]],
    ci_upper = lt[[4]],
    stringsAsFactors = FALSE
  )
  tested_df$conclusion <- ifelse(
    is.na(tested_df$p_value), "could not be computed (insufficient data)",
    ifelse(tested_df$p_value < alpha,
           "VIOLATED — data contradicts this implication",
           "consistent with data (not rejected)")
  )
  rownames(tested_df) <- NULL
  tested_df <- tested_df[order(tested_df$p_value), ]
  rownames(tested_df) <- NULL

  n_violated <- sum(tested_df$p_value < alpha, na.rm = TRUE)

  if (verbose) {
    cat("=== Empirical Test of Theory's Implications ===\n")
    cat("Tested:", nrow(tested_df), " | Untestable (no data mapped):", nrow(untestable_df), "\n")
    cat("Violated at alpha =", alpha, ":", n_violated, "\n\n")
    print(tested_df[, c("X", "Y", "Z", "estimate", "p_value", "conclusion")], row.names = FALSE)
    if (nrow(untestable_df) > 0) {
      cat("\nImplications with no available data (skipped):\n")
      print(untestable_df[, c("X", "Y", "Z", "reason")], row.names = FALSE)
    }
    cat("\nNOTE (per Cox's framing): this is empirical CRITICISM of the theory, not\n")
    cat("confirmation. 'Consistent' means the data failed to contradict this\n")
    cat("implication -- it does not prove the theory correct. 'VIOLATED' is real\n")
    cat("evidence against that specific causal claim as stated.\n")
  }

  list(
    tested       = tested_df,
    untestable   = untestable_df,
    n_tested     = nrow(tested_df),
    n_untestable = nrow(untestable_df),
    n_violated   = n_violated
  )
}


#' Test and Compare Multiple Theories Against the Same Real Data
#'
#' Runs [cauda.test_implications()] for each theory in a named list of DAGs
#' against the same dataset, then summarizes how many of each theory's
#' testable implications survived empirical scrutiny. This is the top-level
#' entry point for Cox's step 5: "test [testable] implications and compare
#' how well the competing theories survive empirical scrutiny."
#'
#' @param dag_list Named list of bnlearn DAGs, one per theory.
#' @param data A data frame containing the real-world measurements.
#' @param var_map Named character vector shared across all theories (DAG
#'   display label -> data column name). If theories use different labels
#'   for the same underlying construct, include all label variants as
#'   separate names pointing at the same column.
#' @param type Type of implied-CI set. Default `"basis.set"`.
#' @param alpha Significance threshold. Default 0.05.
#' @param verbose Logical. Print a readable summary. Default `TRUE`.
#'
#' @return A list with:
#'   * `per_theory`: named list of each theory's full `cauda.test_implications()` result
#'   * `summary`: data frame with one row per theory: `theory`, `n_tested`,
#'     `n_untestable`, `n_violated`, `pct_survived`
#'
#' @export
cauda.test_implications_compare <- function(dag_list, data, var_map,
                                             type = "basis.set", alpha = 0.05,
                                             verbose = TRUE) {

  if (!is.list(dag_list) || length(dag_list) == 0 ||
      is.null(names(dag_list)) || any(names(dag_list) == ""))
    stop("dag_list must be a non-empty named list of DAGs (names = theory labels)")

  per_theory <- lapply(dag_list, function(d) {
    cauda.test_implications(d, data, var_map, type = type, alpha = alpha, verbose = FALSE)
  })

  summary_df <- do.call(rbind, lapply(names(per_theory), function(nm) {
    r <- per_theory[[nm]]
    pct <- if (r$n_tested > 0) round(100 * (r$n_tested - r$n_violated) / r$n_tested, 1) else NA_real_
    data.frame(
      theory       = nm,
      n_tested     = r$n_tested,
      n_untestable = r$n_untestable,
      n_violated   = r$n_violated,
      pct_survived = pct,
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_df) <- NULL
  summary_df <- summary_df[order(-summary_df$pct_survived), ]
  rownames(summary_df) <- NULL

  if (verbose) {
    cat("=== Empirical Comparison Across Theories ===\n\n")
    print(summary_df, row.names = FALSE)
    cat("\nNOTE: pct_survived is the share of a theory's TESTABLE implications\n")
    cat("that were NOT contradicted by data. This is a measure of how much a\n")
    cat("theory has survived empirical criticism so far, not a probability the\n")
    cat("theory is 'true' -- theories may also differ a lot in how many of\n")
    cat("their claims real data can even speak to (see n_untestable).\n")
  }

  list(per_theory = per_theory, summary = summary_df)
}
