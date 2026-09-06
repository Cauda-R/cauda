# Tests for cauda.claims_to_dag() (R/10-cauda.R).

test_that("a simple fork of directed claims becomes a DAG with the right shape", {
  claims <- make_claims_df(source = c("A", "A"), target = c("B", "C"))

  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  display_lookup <- attr(dag, "display_lookup")
  expect_equal(sort(unname(display_lookup)), c("A", "B", "C"))
  expect_equal(nrow(bnlearn::arcs(dag)), 2)
})

test_that("an 'association' claim is excluded from the graph (the claim-type filter)", {
  claims <- rbind(
    make_claims_df("A", "B", claim_type = "causal_effect"),
    make_claims_df("A", "D", claim_type = "association")
  )

  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  display_lookup <- attr(dag, "display_lookup")
  # D never appears as a node at all: association claims don't even
  # contribute isolated nodes, only causal_effect/mechanism/mediation/
  # interaction/moderation claims do.
  expect_false("D" %in% unname(display_lookup))
  expect_equal(nrow(bnlearn::arcs(dag)), 1)
})

test_that("a claims data frame with only association claims errors clearly", {
  claims <- make_claims_df("A", "B", claim_type = "association")
  expect_error(
    cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE),
    "No directed causal claims"
  )
})

test_that("an empty claims data frame errors clearly", {
  claims <- make_claims_df(character(0), character(0))
  expect_error(
    cauda.claims_to_dag(claims, verbose = FALSE),
    "No claims provided"
  )
})

test_that("confidence_threshold excludes low-confidence claims", {
  claims <- rbind(
    make_claims_df("A", "B", confidence = "high"),
    make_claims_df("A", "D", confidence = "low")
  )

  dag <- cauda.claims_to_dag(claims, confidence_threshold = "high",
                              canonicalize_nodes = FALSE, verbose = FALSE)

  expect_false("D" %in% unname(attr(dag, "display_lookup")))
  expect_equal(nrow(bnlearn::arcs(dag)), 1)
})

test_that("include_speculative = FALSE excludes claims not marked established", {
  claims <- rbind(
    make_claims_df("A", "B", established = TRUE),
    make_claims_df("A", "D", established = FALSE)
  )

  dag <- cauda.claims_to_dag(claims, include_speculative = FALSE,
                              canonicalize_nodes = FALSE, verbose = FALSE)

  expect_false("D" %in% unname(attr(dag, "display_lookup")))
  expect_equal(nrow(bnlearn::arcs(dag)), 1)
})

test_that("canonicalize_nodes = FALSE preserves near-duplicate raw labels as separate nodes", {
  claims <- make_claims_df(
    source = c("OxyContin exposure", "oxycontin exposure"),
    target = c("opioid deaths", "opioid deaths")
  )

  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  display_lookup <- attr(dag, "display_lookup")
  expect_equal(length(unique(unname(display_lookup))), 3)
})

test_that("canonicalize_nodes = TRUE merges near-duplicate raw labels (offline fallback)", {
  without_openai_key({
    claims <- make_claims_df(
      source = c("OxyContin exposure", "oxycontin exposure"),
      target = c("opioid deaths", "opioid deaths")
    )

    dag <- cauda.claims_to_dag(claims, canonicalize_nodes = TRUE,
                                node_merge_threshold = 0.25, verbose = FALSE)

    display_lookup <- attr(dag, "display_lookup")
    # Merged down to 2 nodes: one canonical exposure label, one death label.
    expect_equal(length(unique(unname(display_lookup))), 2)

    aliases <- attr(dag, "node_aliases")
    merged_group <- Filter(function(x) length(x) > 1, aliases)
    expect_length(merged_group, 1)
    expect_setequal(merged_group[[1]], c("OxyContin exposure", "oxycontin exposure"))
  })
})

test_that("node_aliases attribute records a singleton group for every unmerged node", {
  claims <- make_claims_df(source = c("A", "A"), target = c("B", "C"))
  dag <- cauda.claims_to_dag(claims, canonicalize_nodes = FALSE, verbose = FALSE)

  aliases <- attr(dag, "node_aliases")
  expect_equal(length(aliases), 3)
  expect_true(all(vapply(aliases, length, integer(1)) == 1))
})
