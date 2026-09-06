# Tests for cauda.test_implications() (R/06-cauda-empirical-test.R).
#
# Uses the classic fork A -> B, A -> C, whose single basis-set implication
# is B _||_ C | A (see test-dagitty.R). Synthetic data is generated with a
# fixed seed so results are deterministic, and thresholds are set well away
# from the borderline so the tests aren't flaky.

fork_dag <- function() {
  claims <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)
}

test_that("an implication consistent with the data is not flagged as violated", {
  set.seed(42)
  n <- 300
  a <- rnorm(n)
  b <- a + rnorm(n, sd = 0.1)
  c <- a + rnorm(n, sd = 0.1)
  data <- data.frame(a = a, b = b, c = c)

  dag <- fork_dag()
  result <- cauda.test_implications(dag, data, c(A = "a", B = "b", C = "c"), verbose = FALSE)

  expect_equal(result$n_tested, 1)
  expect_equal(result$n_untestable, 0)
  expect_true(result$tested$p_value[1] > 0.01)
  expect_equal(result$tested$conclusion[1], "consistent with data (not rejected)")
  expect_equal(result$n_violated, 0)
})

test_that("an implication genuinely violated by the data is flagged", {
  set.seed(42)
  n <- 300
  a <- rnorm(n)
  b <- a + rnorm(n, sd = 0.1)
  # c depends directly on b beyond what a explains, so B and C are NOT
  # independent given A -- the fork's implication should be violated.
  c <- a + 3 * b + rnorm(n, sd = 0.1)
  data <- data.frame(a = a, b = b, c = c)

  dag <- fork_dag()
  result <- cauda.test_implications(dag, data, c(A = "a", B = "b", C = "c"), verbose = FALSE)

  expect_equal(result$n_tested, 1)
  expect_true(result$tested$p_value[1] < 1e-6)
  expect_match(result$tested$conclusion[1], "^VIOLATED")
  expect_equal(result$n_violated, 1)
})

test_that("an implication with an unmapped variable is reported as untestable, not dropped", {
  set.seed(42)
  n <- 300
  data <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))

  dag <- fork_dag()
  # C has no entry in var_map.
  result <- cauda.test_implications(dag, data, c(A = "a", B = "b"), verbose = FALSE)

  expect_equal(result$n_tested, 0)
  expect_equal(result$n_untestable, 1)
  expect_match(result$untestable$reason[1], "C")
})

test_that("too few complete rows after NA-dropping errors instead of testing unreliably", {
  data <- data.frame(a = c(1:5, rep(NA, 20)), b = rnorm(25), c = rnorm(25))
  dag <- fork_dag()

  expect_error(
    cauda.test_implications(dag, data, c(A = "a", B = "b", C = "c"), verbose = FALSE),
    "too few to test reliably"
  )
})

test_that("var_map must be a named character vector", {
  dag <- fork_dag()
  data <- data.frame(a = 1:20, b = 1:20, c = 1:20)
  expect_error(
    cauda.test_implications(dag, data, c("a", "b", "c"), verbose = FALSE),
    "var_map must be a named character vector"
  )
})
