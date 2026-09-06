# Tests for the node-canonicalization helpers in R/10-cauda.R:
# .cauda.canonicalize_labels_editdist(), and the .cauda.canonicalize_labels()
# dispatcher's fallback behavior when no LLM is available. The LLM-based
# path (.cauda.canonicalize_labels_llm()) is intentionally NOT tested here
# since it makes a real network call to OpenAI; it's exercised end to end,
# against real paper data, in the validation work described in the paper.
# These tests only cover the deterministic offline path, which is also the
# path CAUDA falls back to whenever no API key is configured -- including,
# per Section 7 of the paper, whenever a reviewer runs the pipeline without
# one.

test_that("near-duplicate labels merge under the edit-distance threshold", {
  labels <- c("OxyContin exposure", "oxycontin exposure", "Heroin deaths")
  out <- .cauda.canonicalize_labels_editdist(labels, threshold = 0.25)

  expect_equal(out[["OxyContin exposure"]], out[["oxycontin exposure"]])
  expect_false(out[["Heroin deaths"]] == out[["OxyContin exposure"]])
  expect_equal(length(unique(out)), 2)
})

test_that("labels describing genuinely different constructs do not merge", {
  # This is the offline analogue of the over-merge bug caught in the LLM
  # path during testing: "OxyContin distribution" is a pill-volume quantity
  # a real dataset maps to a numeric column, while "OxyContin exposure" is a
  # qualitative construct. They share a common prefix but are not the same
  # thing, and their normalized edit distance is well above a conservative
  # threshold.
  labels <- c("OxyContin distribution", "OxyContin exposure")
  out <- .cauda.canonicalize_labels_editdist(labels, threshold = 0.25)

  expect_equal(length(unique(out)), 2)
  expect_false(out[["OxyContin distribution"]] == out[["OxyContin exposure"]])
})

test_that("case-only variants of the same label always merge to one label", {
  labels <- c("Heroin deaths", "heroin deaths", "HEROIN DEATHS")
  out <- .cauda.canonicalize_labels_editdist(labels, threshold = 0.25)

  expect_equal(length(unique(out)), 1)
})

test_that("a single label is returned unchanged", {
  out <- .cauda.canonicalize_labels_editdist("Solo node", threshold = 0.25)
  expect_equal(unname(out), "Solo node")
})

test_that("the dispatcher falls back to edit-distance when no API key is set", {
  without_openai_key({
    out <- .cauda.canonicalize_labels(c("Heroin deaths", "heroin deaths"), threshold = 0.25)
    expect_match(attr(out, "method"), "^edit-distance")
    expect_equal(length(unique(out)), 1)
  })
})
