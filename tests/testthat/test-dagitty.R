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

test_that("cauda.dagitty_compare() flags implications unique to one theory", {
  fork    <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  chain   <- make_claims_df(source = c("X", "Y"), target = c("Y", "Z"))

  dag_fork  <- cauda.claims_to_dag(fork,  canonicalize_nodes = FALSE, verbose = FALSE)
  dag_chain <- cauda.claims_to_dag(chain, canonicalize_nodes = FALSE, verbose = FALSE)

  cmp <- cauda.dagitty_compare(list(fork = dag_fork, chain = dag_chain), verbose = FALSE)

  expect_true(nrow(cmp$discriminating) >= 1)
  expect_true(all(c("fork", "chain") %in% cmp$all_implications$theory))
})
