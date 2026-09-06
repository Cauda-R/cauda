# Test helpers shared across test files.
#
# make_claims_df() builds a claims data frame matching the exact schema
# cauda.claims_to_dag() and friends expect (same columns as
# .empty_claims_df() in R/02-claims-extraction.R), so tests exercise the
# real column contract rather than a hand-picked subset that happens to
# work today but would silently drift from the real schema.

make_claims_df <- function(source, target,
                            claim_type  = "causal_effect",
                            confidence  = "high",
                            established = TRUE,
                            pathway     = "unknown",
                            paper_id    = "P1",
                            verbatim_quote = NA_character_) {
  n <- length(source)
  stopifnot(length(target) == n)
  recycle <- function(x) if (length(x) == 1) rep(x, n) else x

  data.frame(
    paper_id       = recycle(paper_id),
    verbatim_quote = recycle(verbatim_quote),
    source         = source,
    target         = target,
    claim          = if (n == 0) character(0) else paste(source, "->", target),
    claim_type     = recycle(claim_type),
    claim_class    = rep("A", n),
    causal_trigger = rep(TRUE, n),
    policy_trigger = rep(FALSE, n),
    claim_scope    = rep("overall_exposure", n),
    confidence     = recycle(confidence),
    effect_size    = rep(NA_character_, n),
    p_value        = rep(NA_character_, n),
    sample_size    = rep(NA_character_, n),
    pathway        = recycle(pathway),
    established    = recycle(established),
    evidence       = rep(NA_character_, n),
    notes          = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
}

# Temporarily clears OPENAI_API_KEY for the duration of `expr`, restoring
# whatever was there afterward. Node-canonicalization tests use this to force
# the deterministic offline edit-distance path rather than depending on
# network access or an API key being present in the test environment.
without_openai_key <- function(expr) {
  old <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  Sys.setenv(OPENAI_API_KEY = "")
  on.exit({
    if (is.na(old)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old)
  })
  force(expr)
}
