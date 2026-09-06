# Tests for parse_claims_json() (R/02-claims-extraction.R). This is pure JSON
# parsing and schema-normalization logic with no network dependency, so it's
# fully testable without an API key.

test_that("a well-formed claim is parsed into the expected schema", {
  raw_json <- '{
    "claims": [{
      "source": "A", "target": "B", "claim": "A causes B",
      "claim_type": "causal_effect", "confidence": "high",
      "established": true, "verbatim_quote": "A causes B directly."
    }],
    "study_summary": "test paper"
  }'

  df <- parse_claims_json(raw_json, paper_id = "P1")

  expect_equal(nrow(df), 1)
  expect_equal(df$paper_id, "P1")
  expect_equal(df$source, "A")
  expect_equal(df$target, "B")
  expect_equal(df$claim_type, "causal_effect")
  expect_equal(df$confidence, "high")
  expect_true(df$established)
  expect_equal(df$verbatim_quote, "A causes B directly.")
})

test_that("an invalid claim_type falls back to the association default", {
  raw_json <- '{"claims": [{"source": "A", "target": "B", "claim_type": "not_a_real_type"}]}'
  df <- parse_claims_json(raw_json)
  expect_equal(df$claim_type, "association")
})

test_that("an invalid confidence falls back to the low default", {
  raw_json <- '{"claims": [{"source": "A", "target": "B", "confidence": "extremely sure"}]}'
  df <- parse_claims_json(raw_json)
  expect_equal(df$confidence, "low")
})

test_that("claims missing source or target are dropped, not kept with NAs", {
  raw_json <- '{
    "claims": [
      {"source": "A", "target": "B", "claim_type": "causal_effect"},
      {"source": "A", "claim_type": "causal_effect"}
    ]
  }'
  df <- parse_claims_json(raw_json)
  expect_equal(nrow(df), 1)
  expect_equal(df$source, "A")
  expect_equal(df$target, "B")
})

test_that("a response with no claims returns the empty schema, not an error", {
  raw_json <- '{"claims": []}'
  df <- parse_claims_json(raw_json)
  expect_equal(nrow(df), 0)
  expect_true(all(c("source", "target", "claim_type", "confidence") %in% names(df)))
})

test_that("markdown code fences around the JSON are stripped before parsing", {
  raw_json <- '```json\n{"claims": [{"source": "A", "target": "B"}]}\n```'
  df <- parse_claims_json(raw_json)
  expect_equal(nrow(df), 1)
})

test_that("malformed JSON raises an informative error rather than a cryptic one", {
  expect_error(parse_claims_json("{not valid json"), "Failed to parse GPT JSON response")
})
