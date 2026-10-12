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
#'   * `implied_list`: the same implications as a list (conditioning sets as
#'     character vectors), used by [cauda.dagitty_compare()]
#'   * `display_lookup`: map from dagitty node names to display labels
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
                implied_CIs = empty_df, n_implications = 0,
                implied_list = list(), display_lookup = display_lookup))
  }

  to_display <- function(x) {
    if (use_display_names && !is.null(display_lookup)) {
      out <- unname(display_lookup[x])
      ifelse(is.na(out), x, out)
    } else x
  }

  # dagitty can group several targets into one implication (e.g.
  # "Z _||_ X, Y"). Expand every implication into one entry per (X, Y) pair,
  # keeping the conditioning set as a character vector of display labels (a
  # comma-joined string is ambiguous when a label itself contains a comma).
  # cauda.dagitty_compare() re-evaluates each entry by d-separation in rival
  # DAGs.
  ci_list <- list()
  for (ci in implied) {
    z_disp <- if (length(ci$Z) > 0) to_display(ci$Z) else character(0)
    for (x in ci$X) for (y in ci$Y) {
      ci_list[[length(ci_list) + 1]] <- list(X = to_display(x), Y = to_display(y), Z = z_disp)
    }
  }

  ci_df <- do.call(rbind, lapply(ci_list, function(ci) {
    data.frame(
      X = ci$X,
      Y = ci$Y,
      Z = if (length(ci$Z) > 0) paste(ci$Z, collapse = ", ") else "(nothing)",
      n_z = length(ci$Z),
      stringsAsFactors = FALSE
    )
  }))
  rownames(ci_df) <- NULL
  ci_df$idx <- seq_len(nrow(ci_df))

  # De-duplicate order-independent pairs: dagitty::impliedConditionalIndependencies()
  # can return BOTH "X _||_ Y | Z" and "Y _||_ X | Z" as separate entries for
  # certain DAG shapes (confirmed: a single common-cause/"star" DAG, e.g. one
  # source claim feeding many targets, produces every pair twice — 30 rows
  # for 6 leaf nodes instead of the 15 true pairs). The two orderings are the
  # same statement, so keep only one. This matters beyond just inflated counts
  # in the Testable Implications tab: cauda.dagitty_compare() below keys each
  # implication as pmin(X,Y)/pmax(X,Y)/Z specifically so "X⊥Y" and "Y⊥X"
  # collapse to one key — but if THIS function hands it two rows with the
  # same key from a single theory, that key's count is >=2 before any
  # cross-theory comparison even happens, so cauda.dagitty_compare() marked
  # every one of that theory's implications as "not discriminating" (shared)
  # even when no other theory came anywhere near it. That's what made a
  # perfectly good single-source theory disappear entirely from the Compare
  # Theories tab. Fixed 2026-08-01.
  dedup_key <- paste(pmin(ci_df$X, ci_df$Y), pmax(ci_df$X, ci_df$Y), ci_df$Z, sep = " || ")
  ci_df <- ci_df[!duplicated(dedup_key), ]

  # Sort "best"/most-useful-to-test-first: implications with FEWER
  # conditioning variables are simpler and cheaper to test directly against
  # real data (fewer covariates to control for, more statistical power), so
  # those surface first. Ties broken alphabetically by X then Y so the order
  # is stable/reproducible rather than depending on dagitty's internal
  # traversal order. n_z is dropped from the returned data frame afterward —
  # it's a display-ordering aid, not part of the public column contract.
  ci_df <- ci_df[order(ci_df$n_z, ci_df$X, ci_df$Y), ]
  ci_list <- ci_list[ci_df$idx]
  ci_df$n_z <- NULL
  ci_df$idx <- NULL
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
    n_implications = nrow(ci_df),
    implied_list   = ci_list,
    display_lookup = display_lookup
  )
}


#' Compare Testable Implications Across Competing Theory DAGs
#'
#' Given a named list of cauda DAGs — one per competing causal theory of the
#' same phenomenon — computes each theory's testable implications and checks
#' each one against every rival DAG by d-separation. An implication
#' `X _||_ Y | Z` from theory T is **discriminating** when at least one rival
#' DAG U contains all of X, Y and Z as nodes and does NOT entail the
#' independence (X and Y are d-connected given Z in U). Such an implication is
#' the most useful place to look first when confronting theories with data:
#' if it is violated, T takes the hit while U survives that test.
#'
#' The check is by d-separation, not by comparing printed implication lists.
#' A basis set is only one generating set of a graph's independence
#' constraints, so an implication absent from rival U's basis set can still be
#' entailed by U; comparing lists alone would wrongly call such an
#' implication discriminating. Implications that mention a variable absent
#' from a rival are reported as not evaluable in that rival, and are never
#' counted as discriminating against it.
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
#'     tagged with `theory` and with the rival-theory columns
#'     `entailed_by` (rivals whose DAG implies the same independence),
#'     `not_entailed_by` (rivals that contain all the variables but allow
#'     dependence), `not_evaluable_in` (rivals missing a variable), and
#'     `discriminating` (TRUE when `not_entailed_by` is non-empty)
#'   * `discriminating`: the rows of `all_implications` with
#'     `discriminating == TRUE`
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

  # Does theory `res` entail X _||_ Y | Z (labels are display labels)?
  # TRUE = entailed (d-separated); FALSE = dependence allowed; NA = a
  # variable is not a node of that theory's DAG, so it cannot be evaluated.
  entails <- function(res, ci) {
    lk <- res$display_lookup
    to_node <- function(lbl) {
      if (length(lbl) == 0) return(character(0))
      if (is.null(lk)) return(lbl)
      m <- names(lk)[match(lbl, unname(lk))]
      ifelse(is.na(m), lbl, m)
    }
    x <- to_node(ci$X); y <- to_node(ci$Y); z <- to_node(ci$Z)
    nodes <- names(res$dagitty)
    if (!all(c(x, y, z) %in% nodes)) return(NA)
    dagitty::dseparated(res$dagitty, x, y, z)
  }

  theory_names <- names(per_theory)

  all_ci <- do.call(rbind, lapply(theory_names, function(nm) {
    df <- per_theory[[nm]]$implied_CIs
    if (nrow(df) == 0) return(NULL)
    lst <- per_theory[[nm]]$implied_list
    rivals <- setdiff(theory_names, nm)
    verdicts <- lapply(lst, function(ci) {
      vapply(rivals, function(r) entails(per_theory[[r]], ci), logical(1))
    })
    pick <- function(v, want) {
      if (length(v) == 0) return("")
      paste(names(v)[!is.na(v) & v == want], collapse = ", ")
    }
    df$theory <- nm
    df$entailed_by      <- vapply(verdicts, pick, character(1), want = TRUE)
    df$not_entailed_by  <- vapply(verdicts, pick, character(1), want = FALSE)
    df$not_evaluable_in <- vapply(verdicts, function(v) {
      if (length(v) == 0) "" else paste(names(v)[is.na(v)], collapse = ", ")
    }, character(1))
    df$discriminating   <- nzchar(df$not_entailed_by)
    df
  }))

  if (is.null(all_ci) || nrow(all_ci) == 0) {
    if (verbose) cat("No implications to compare (all DAGs saturated or empty).\n")
    return(list(per_theory = per_theory,
                all_implications = data.frame(),
                discriminating = data.frame()))
  }

  rownames(all_ci) <- NULL
  discriminating <- all_ci[all_ci$discriminating, ]
  rownames(discriminating) <- NULL

  if (verbose) {
    cat("=== Cross-Theory Implication Comparison ===\n")
    cat("Theories:", paste(names(dag_list), collapse = ", "), "\n")
    cat("Total implications across all theories:", nrow(all_ci), "\n")
    cat("Discriminating implications (a rival DAG allows dependence):", nrow(discriminating), "\n\n")
    if (nrow(discriminating) > 0) {
      print(discriminating[, c("theory", "X", "Y", "Z", "not_entailed_by")], row.names = FALSE)
    }
    cat("\n")
  }

  list(per_theory = per_theory, all_implications = all_ci, discriminating = discriminating)
}
