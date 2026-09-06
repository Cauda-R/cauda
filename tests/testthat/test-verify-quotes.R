# Tests for cauda.verify_quotes() (R/02-claims-extraction.R).
#
# These matter more than a typical unit test: cauda.verify_quotes() is the
# function the paper's Section 4.3 relies on to say a quotation is
# trustworthy, and this session found two real bugs in it by hand
# (utils::agrepl() doesn't exist -- agrepl() is base R -- so the fuzzy-match
# fallback was silently failing on every call via a swallowed tryCatch
# error; and quote-mark type needed to be stripped, not just normalized,
# because an LLM serializing a quotation into JSON can turn a source's
# double quotes into single quotes). Both are covered explicitly below so a
# future refactor can't quietly reintroduce either one.

test_that("an exact substring match is reported as exact match", {
  source_text <- "The quick brown fox jumps over the lazy dog in the morning sunlight, according to the report published last year."
  claims <- make_claims_df("fox", "dog",
    verbatim_quote = "The quick brown fox jumps over the lazy dog in the morning sunlight")

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_true(out$quote_verified[1])
  expect_equal(out$quote_verification[1], "exact match")
})

test_that("dash and curly-quote variants normalize to an exact match", {
  # Mirrors the real false negative found on the Case & Deaton "deaths of
  # despair" quote: the source PDF renders an em-dash and curly double
  # quotes, while the extracted claim uses a plain hyphen and straight
  # single quotes around the same nested term.
  source_text <- paste0(
    "Sales fell — dramatically — after the policy change, ",
    "which analysts called “a turning point” in the industry's history."
  )
  claims <- make_claims_df("policy change", "sales",
    verbatim_quote = "Sales fell - dramatically - after the policy change, which analysts called 'a turning point' in the industry's history.")

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_true(out$quote_verified[1])
  expect_equal(out$quote_verification[1], "exact match")
})

test_that("a quote missing one word is a near-exact match, not NOT FOUND", {
  # This is the regression test for the utils::agrepl() bug: agrepl() is
  # exported from base, not utils, so utils::agrepl(...) always threw and
  # the surrounding tryCatch(error = function(e) FALSE) turned every fuzzy
  # comparison into a silent FALSE. If that bug comes back, this quote,
  # which is only a few characters off from a ~190-character sentence,
  # will incorrectly come back as "NOT FOUND" instead of "near-exact match".
  source_text <- paste(
    "Even after controlling for confounders, the treatment group showed a",
    "marked and statistically significant improvement in outcomes relative",
    "to the control group over the full follow-up period."
  )
  claims <- make_claims_df("treatment", "outcomes",
    verbatim_quote = paste(
      "Even after controlling for confounders, the treatment group showed a",
      "marked statistically significant improvement in outcomes relative",
      "to the control group over the full follow-up period."
    ))

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_true(out$quote_verified[1])
  expect_match(out$quote_verification[1], "^near-exact match")
})

test_that("a quote with no relationship to the source is flagged NOT FOUND", {
  source_text <- "The quick brown fox jumps over the lazy dog in the morning sunlight."
  claims <- make_claims_df("moon", "cheese",
    verbatim_quote = "The moon is made of green cheese and aliens built it in 1823.")

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_false(out$quote_verified[1])
  expect_match(out$quote_verification[1], "^NOT FOUND")
})

test_that("a missing verbatim_quote is reported distinctly from a bad quote", {
  source_text <- "Some source text that is irrelevant to this claim."
  claims <- make_claims_df("a", "b", verbatim_quote = NA_character_)

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_false(out$quote_verified[1])
  expect_equal(out$quote_verification[1], "no quote provided")
})

test_that("an empty claims data frame is handled without error", {
  empty <- make_claims_df(character(0), character(0))
  out <- cauda.verify_quotes(empty, "irrelevant source text", verbose = FALSE)

  expect_equal(nrow(out), 0)
  expect_type(out$quote_verified, "logical")
  expect_type(out$quote_verification, "character")
})

test_that("verification runs independently over multiple claims", {
  source_text <- "Alpha causes beta. Gamma has nothing to do with delta in this paper."
  claims <- make_claims_df(
    source = c("alpha", "gamma"),
    target = c("beta", "delta"),
    verbatim_quote = c("Alpha causes beta.", "Gamma directly causes delta via an unstated mechanism.")
  )

  out <- cauda.verify_quotes(claims, source_text, verbose = FALSE)

  expect_true(out$quote_verified[1])
  expect_equal(out$quote_verification[1], "exact match")
  expect_false(out$quote_verified[2])
  expect_match(out$quote_verification[2], "^NOT FOUND")
})
