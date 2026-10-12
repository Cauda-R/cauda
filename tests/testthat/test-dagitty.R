# Tests for cauda.dagitty() (R/05-dagitty-module.R).

test_that("a common-cause fork implies exactly one basis-set independence", {
  claims <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  result <- cauda.dagitty(dag, verbose = FALSE)

  expect_equal(result$n_implications, 1)
  expect_setequal(names(result$implied_CIs), c("X", "Y", "Z"))
  # B _||_ C | A, in either order -- display names, not bnlearn's sanitized ones.
  expect_setequal(c(result$implied_CIs$X[1], result$implied_CIs$Y[1]), c("B", "C"))
  expect_equal(result$implied_CIs$Z[1], "A")
})

test_that("a saturated two-node DAG has zero testable implications", {
  claims <- make_claims_df(source = "A", target = "B")
  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  result <- cauda.dagitty(dag, verbose = FALSE)

  expect_equal(result$n_implications, 0)
  expect_equal(nrow(result$implied_CIs), 0)
})

test_that("dagitty_string is valid dagitty syntax round-trippable by dagitty itself", {
  claims <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)
  result <- cauda.dagitty(dag, verbose = FALSE)

  expect_s3_class(result$dagitty, "dagitty")
  # Reparsing the emitted dagitty_string should not error, and should
  # produce the same three node labels.
  reparsed <- dagitty::dagitty(result$dagitty_string)
  expect_setequal(names(reparsed), c("A", "B", "C"))
})

test_that("cauda.dagitty_compare() flags implications a rival DAG does not entail", {
  # Shared variables A, B, C. Fork: A -> B, A -> C implies B _||_ C | A.
  # Collider: B -> A, C -> A implies B _||_ C (marginal). Each graph's
  # implication is allowed to fail in the other, so both are discriminating.
  fork     <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  collider <- make_claims_df(source = c("B", "C"), target = c("A", "A"))

  dag_fork     <- cauda.claims_to_dag(fork,     canonicalize_nodes = FALSE, verbose = FALSE)
  dag_collider <- cauda.claims_to_dag(collider, canonicalize_nodes = FALSE, verbose = FALSE)

  cmp <- cauda.dagitty_compare(list(fork = dag_fork, collider = dag_collider), verbose = FALSE)

  expect_true(all(c("fork", "collider") %in% cmp$all_implications$theory))
  expect_true(all(c("fork", "collider") %in% cmp$discriminating$theory))
  expect_true(all(cmp$discriminating$discriminating))
})

test_that("an implication a rival entails but does not list in its basis set is NOT discriminating", {
  # T:  X -> Y -> W <- Z.  Basis set includes  X _||_ W | {Y, Z}  and  X _||_ Z.
  # U:  X -> Y -> Z -> W.  Basis set does NOT list X _||_ W | {Y, Z}
  #     (it lists X _||_ W | Z), but U still entails it by d-separation.
  # Comparing printed lists would call X _||_ W | {Y, Z} "unique to T".
  t_claims <- make_claims_df(source = c("X", "Y", "Z"), target = c("Y", "W", "W"))
  u_claims <- make_claims_df(source = c("X", "Y", "Z"), target = c("Y", "Z", "W"))
  dag_t <- cauda.claims_to_dag(t_claims, canonicalize_nodes = FALSE, verbose = FALSE)
  dag_u <- cauda.claims_to_dag(u_claims, canonicalize_nodes = FALSE, verbose = FALSE)

  cmp <- cauda.dagitty_compare(list(T = dag_t, U = dag_u), verbose = FALSE)
  all_t <- cmp$all_implications[cmp$all_implications$theory == "T", ]

  is_pair <- function(df, a, b) mapply(function(x, y) setequal(c(x, y), c(a, b)), df$X, df$Y)

  xw <- all_t[is_pair(all_t, "X", "W"), ]
  expect_equal(nrow(xw), 1)
  expect_false(xw$discriminating)
  expect_equal(xw$entailed_by, "U")

  xz <- all_t[is_pair(all_t, "X", "Z"), ]
  expect_equal(nrow(xz), 1)
  expect_true(xz$discriminating)
  expect_equal(xz$not_entailed_by, "U")
})

test_that("an implication mentioning a variable absent from a rival is not evaluable there", {
  fork  <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  other <- make_claims_df(source = c("P", "Q"), target = c("Q", "R"))
  dag_fork  <- cauda.claims_to_dag(fork,  canonicalize_nodes = FALSE, verbose = FALSE)
  dag_other <- cauda.claims_to_dag(other, canonicalize_nodes = FALSE, verbose = FALSE)

  cmp <- cauda.dagitty_compare(list(fork = dag_fork, other = dag_other), verbose = FALSE)
  fork_rows <- cmp$all_implications[cmp$all_implications$theory == "fork", ]

  expect_equal(fork_rows$not_evaluable_in, "other")
  expect_false(any(fork_rows$discriminating))
})
