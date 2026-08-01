#' Convert a Cauda DAG to dagitty and Derive Testable Implications
#'
#' Takes a bnlearn DAG produced by [cauda.claims_to_dag()] (or rebuilt by the
#' Shiny app's DAG editor, which sets the same attributes) and converts it to
#' dagitty format, then computes the model's testable implications: the
#' conditional independencies that MUST hold in real data if this causal
#' theory is correct. This is the core operation behind comparing competing
#' theories (see [cauda.dagitty_compare()]): each candidate DAG implies a
#' different set of testable conditional independencies, and checking those
#' against available data is a systematic way to subject each theory to
#' empirical criticism, per Cox's proposed CAUDA workflow.
#'
#' @param dag A bnlearn bn object with `attr(dag, "display_lookup")` set
#'   (as produced by [cauda.claims_to_dag()] or the Shiny app's DAG editor).
#'   `display_lookup` maps bnlearn's sanitized node names back to the
#'   human-readable claim labels; without it, implications are reported
#'   using the sanitized names.
#' @param type Type of implied-CI set to compute, passed through to
#'   `dagitty::impliedConditionalIndependencies()`. Default `"basis.set"`:
#'   Pearl's minimal/local test set — one entry per non-adjacent pair of
#'   nodes in topological order, conditioning on the later node's parents.
#'   This is sufficient to fully test the model under faithfulness, and is
#'   the smallest such set, so it's the right default for "what should we
#'   go test against data." Use `"all.pairs"` for the exhaustive set instead.
#' @param use_display_names Logical. If `TRUE` (default), implications use
#'   the human-readable claim labels instead of bnlearn's sanitized node
#'   names (e.g. `UPF.consumption` becomes `UPF consumption`).
#' @param verbose Logical. Print a readable summary. Default `TRUE`.
#'
#' @return A list with:
#'   * `dagitty`: the `dagitty` graph object (for further dagitty functions,
#'     e.g. `dagitty::adjustmentSets()`)
#'   * `dagitty_string`: the model definition as dagitty syntax text — paste
#'     this directly into <http://dagitty.net> for interactive visual editing
#'   * `implied_CIs`: data frame with columns `X`, `Y`, `Z` (conditioning
#'     set, comma-separated, or `"(nothing)"`), one row per implication.
#'     Sorted with the simplest implications first (fewest conditioning
#'     variables = cheapest/most direct to test against real data), ties
#'     broken alphabetically by X then Y.
#'   * `n_implications`: number of testable implications returned
#'
#' @examples
#' \dontrun{
#'   dag <- cauda.claims_to_dag(claims)
#'   result <- cauda.dagitty(dag)
#'   result$implied_CIs
#'   cat(result$dagitty_string)  # paste into dagitty.net
#' }
#'
#' @export
#' @importFrom bnlearn arcs nodes
cauda.dagitty <- function(dag, type = "basis.set", use_display_names = TRUE, verbose = TRUE) {

  if (!requireNamespace("dagitty", quietly = TRUE)) {
    stop("dagitty required. Install with: install.packages('dagitty')")
  }
  if (!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("bnlearn required. Install with: install.packages('bnlearn')")
  }

  arcs_mat       <- bnlearn::arcs(dag)
  node_names     <- bnlearn::nodes(dag)
  display_lookup <- attr(dag, "display_lookup")

  if (length(node_names) == 0) stop("DAG has no nodes.")

  # Build dagitty model definition text: dag { "A" -> "B" ... isolated nodes too }
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

  empty_df <- data.frame(X = character(), Y = character(), Z = character(), stringsAsFactors = FALSE)

  if (length(implied) == 0) {
    if (verbose) cat("No testable implications: every pair of nodes is adjacent (the DAG is saturated), ",
                      "or there are too few nodes to imply any conditional independence.\n", sep = "")
    return(list(dagitty = dg, dagitty_string = dagitty_string,
                implied_CIs = empty_df, n_implications = 0))
  }

  to_display <- function(x) {
    if (use_display_names && !is.null(display_lookup)) {
      out <- unname(display_lookup[x])
      ifelse(is.na(out), x, out)
    } else x
  }

  ci_df <- do.call(rbind, lapply(implied, function(ci) {
    data.frame(
      X = to_display(ci$X),
      Y = to_display(ci$Y),
      Z = if (length(ci$Z) > 0) paste(to_display(ci$Z), collapse = ", ") else "(nothing)",
      n_z = length(ci$Z),
      stringsAsFactors = FALSE
    )
  }))
  rownames(ci_df) <- NULL

  # Sort "best"/most-useful-to-test-first: implications with FEWER
  # conditioning variables are simpler and cheaper to test directly against
  # real data (fewer covariates to control for, more statistical power), so
  # those surface first. Ties broken alphabetically by X then Y so the order
  # is stable/reproducible rather than depending on dagitty's internal
  # traversal order. n_z is dropped from the returned data frame afterward —
  # it's a display-ordering aid, not part of the public column contract.
  ci_df <- ci_df[order(ci_df$n_z, ci_df$X, ci_df$Y), ]
  ci_df$n_z <- NULL
  rownames(ci_df) <- NULL

  if (verbose) {
    cat("=== Testable Implications (", type, ") ===\n", sep = "")
    cat("Nodes:", length(node_names), " | Edges:", nrow(arcs_mat), " | Implications:", nrow(ci_df), "\n\n")
    for (i in seq_len(nrow(ci_df))) {
      cat(sprintf("  %s  _||_  %s   |   {%s}\n", ci_df$X[i], ci_df$Y[i], ci_df$Z[i]))
    }
    cat("\n")
  }

  list(
    dagitty        = dg,
    dagitty_string = dagitty_string,
    implied_CIs    = ci_df,
    n_implications = nrow(ci_df)
  )
}


#' Compare Testable Implications Across Competing Theory DAGs
#'
#' Given a named list of cauda DAGs — one per competing causal theory of the
#' same phenomenon — computes each theory's testable implications and flags
#' which implications are unique to a single theory. A "discriminating"
#' implication (one that only one theory predicts) is the most useful place
#' to look first when confronting theories with data: if it's violated, that
#' theory alone takes the hit, whereas implications shared by every theory
#' can't help you tell the theories apart.
#'
#' Per Cox's framing, this is meant as a tool for empirical criticism of
#' rival theories, NOT for declaring one DAG "the true model" — observational
#' data often can't distinguish between causally different models, and
#' theories may be complementary (operating on different sub-populations or
#' time periods) rather than strictly competing.
#'
#' @param dag_list Named list of bnlearn DAGs, one per theory (names are used
#'   as theory labels in the output).
#' @param verbose Logical. Print a readable summary. Default `TRUE`.
#'
#' @return A list with:
#'   * `per_theory`: named list of each theory's full `cauda.dagitty()` result
#'   * `all_implications`: data frame of every implication from every theory,
#'     tagged with which theory it came from
#'   * `discriminating`: subset of `all_implications` where the same
#'     `X _||_ Y | Z` triple appears under only one theory
#'
#' @examples
#' \dontrun{
#'   dags <- list(
#'     pharma_supply = cauda.claims_to_dag(claims_a),
#'     despair       = cauda.claims_to_dag(claims_b)
#'   )
#'   cmp <- cauda.dagitty_compare(dags)
#'   cmp$discriminating
#' }
#'
#' @export
cauda.dagitty_compare <- function(dag_list, verbose = TRUE) {

  if (!is.list(dag_list) || length(dag_list) == 0 ||
      is.null(names(dag_list)) || any(names(dag_list) == ""))
    stop("dag_list must be a non-empty named list of DAGs (names = theory labels)")

  per_theory <- lapply(dag_list, cauda.dagitty, verbose = FALSE)

  all_ci <- do.call(rbind, lapply(names(per_theory), function(nm) {
    df <- per_theory[[nm]]$implied_CIs
    if (nrow(df) == 0) return(NULL)
    df$theory <- nm
    # order-independent key so X_||_Y|Z and Y_||_X|Z collapse to the same row
    df$key <- paste(pmin(df$X, df$Y), pmax(df$X, df$Y), df$Z, sep = " || ")
    df
  }))

  if (is.null(all_ci) || nrow(all_ci) == 0) {
    if (verbose) cat("No implications to compare (all DAGs saturated or empty).\n")
    return(list(per_theory = per_theory,
                all_implications = data.frame(),
                discriminating = data.frame()))
  }

  key_counts     <- table(all_ci$key)
  discriminating <- all_ci[all_ci$key %in% names(key_counts[key_counts == 1]), ]
  discriminating$key <- NULL
  all_ci$key <- NULL

  if (verbose) {
    cat("=== Cross-Theory Implication Comparison ===\n")
    cat("Theories:", paste(names(dag_list), collapse = ", "), "\n")
    cat("Total implications across all theories:", nrow(all_ci), "\n")
    cat("Discriminating implications (unique to one theory):", nrow(discriminating), "\n\n")
    if (nrow(discriminating) > 0) {
      print(discriminating[, c("theory", "X", "Y", "Z")], row.names = FALSE)
    }
    cat("\n")
  }

  list(per_theory = per_theory, all_implications = all_ci, discriminating = discriminating)
}
