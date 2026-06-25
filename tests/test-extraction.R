# ============================================================================
# Test Suite for Claims Extraction System
# ============================================================================
# Tests the extraction pipeline end-to-end

library(cauda)

# Test 1: Extract from sample text
test_sample_text <- function() {
  cat("\n=== TEST 1: Extract from Sample Text ===\n")

  sample_text <- paste(
    "BACKGROUND: We studied the relationship between sleep deprivation and cognitive performance.",
    "METHODS: 284 healthy adults were randomly assigned to sleep restriction (4 hours/night) or",
    "normal sleep (8 hours/night) for 7 days. Cognitive performance was measured with digit span",
    "and reaction time tasks.",
    "RESULTS: Sleep-restricted group showed significantly worse performance. Specifically,",
    "each additional hour of sleep loss increased error rate by 12% (95% CI: 8-16%, p<0.001).",
    "The effect was dose-dependent, with larger deficits in those with greatest sleep loss.",
    "We also found that the negative effects were moderated by baseline cognitive ability,",
    "with lower-performing individuals showing 18% error increase per hour lost.",
    "DISCUSSION: Our findings demonstrate a causal relationship between sleep duration and",
    "cognitive function, consistent with prior mechanistic studies showing impaired prefrontal",
    "cortex function under sleep deprivation.",
    sep = " "
  )

  # Extract claims
  claims <- cauda.extract(sample_text, return_raw_text = FALSE)

  cat("\nExtracted", nrow(claims), "claims:\n")
  print(claims)

  # Check structure
  expected_cols <- c("source", "target", "claim", "claim_type", "confidence",
                     "effect_size", "p_value", "sample_size", "pathway",
                     "established", "evidence", "notes")
  missing_cols <- setdiff(expected_cols, names(claims))

  if (length(missing_cols) > 0) {
    cat("\n❌ FAIL: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    return(FALSE)
  }

  if (nrow(claims) == 0) {
    cat("\n❌ FAIL: No claims extracted\n")
    return(FALSE)
  }

  cat("\n✓ PASS: Extracted", nrow(claims), "claims with all required columns\n")
  return(TRUE)
}

# Test 2: Verify claim types are normalized
test_claim_type_normalization <- function() {
  cat("\n=== TEST 2: Claim Type Normalization ===\n")

  sample_text <- paste(
    "RESULTS: Intervention A directly caused improvement in outcomes (p<0.001).",
    "The effect was mediated through mechanism B, which operates through pathway C.",
    "Under high stress conditions, the relationship was stronger.",
    sep = " "
  )

  claims <- cauda.extract(sample_text)

  valid_types <- c("causal_effect", "mechanism", "conditional", "dose-response", "moderated", "other")
  invalid_types <- claims$claim_type[!claims$claim_type %in% valid_types]

  if (length(invalid_types) > 0) {
    cat("\n❌ FAIL: Invalid claim types:", paste(invalid_types, collapse = ", "), "\n")
    return(FALSE)
  }

  cat("\n✓ PASS: All claim types properly normalized\n")
  return(TRUE)
}

# Test 3: Verify confidence levels
test_confidence_normalization <- function() {
  cat("\n=== TEST 3: Confidence Level Normalization ===\n")

  claims_data <- data.frame(
    source = c("A", "B"),
    target = c("C", "D"),
    claim = c("Test1", "Test2"),
    claim_type = c("causal_effect", "mechanism"),
    confidence = c("high", "low"),
    effect_size = c("d=1.2", NA),
    p_value = c("p<0.001", NA),
    sample_size = c("N=500", NA),
    pathway = c("behavioral", "unknown"),
    established = c(TRUE, FALSE),
    evidence = c("RCT", "observational"),
    notes = c(NA, NA),
    stringsAsFactors = FALSE
  )

  valid_confs <- c("high", "medium", "low")
  invalid_confs <- claims_data$confidence[!claims_data$confidence %in% valid_confs]

  if (length(invalid_confs) > 0) {
    cat("\n❌ FAIL: Invalid confidence levels:", paste(invalid_confs, collapse = ", "), "\n")
    return(FALSE)
  }

  cat("\n✓ PASS: All confidence levels valid\n")
  return(TRUE)
}

# Test 4: Verify pathway classification
test_pathway_normalization <- function() {
  cat("\n=== TEST 4: Pathway Classification ===\n")

  sample_text <- paste(
    "We found that initial stress triggers a cascade of behavioral changes.",
    "Shared genetic factors predispose both conditions.",
    "Institutional barriers create structural inequities.",
    "Learned behaviors establish the pattern.",
    sep = " "
  )

  claims <- cauda.extract(sample_text)

  valid_pathways <- c("gateway", "common_liability", "structural", "behavioral",
                      "physiological", "unknown")
  invalid_pathways <- claims$pathway[!claims$pathway %in% valid_pathways]

  if (length(invalid_pathways) > 0) {
    cat("\n❌ FAIL: Invalid pathways:", paste(invalid_pathways, collapse = ", "), "\n")
    return(FALSE)
  }

  cat("\n✓ PASS: All pathways properly classified\n")
  return(TRUE)
}

# Test 5: Test DAG creation from extracted claims
test_dag_generation <- function() {
  cat("\n=== TEST 5: DAG Generation from Claims ===\n")

  # Create minimal valid claims dataframe
  claims <- data.frame(
    source = c("Sleep", "Stress", "Sleep"),
    target = c("Cognition", "Cognition", "Stress"),
    claim = c("Sleep affects cognition", "Stress affects cognition", "Sleep reduces stress"),
    claim_type = c("causal_effect", "causal_effect", "causal_effect"),
    confidence = c("high", "medium", "high"),
    effect_size = c("d=1.2", NA, NA),
    p_value = c("p<0.001", "p=0.05", NA),
    sample_size = c("N=500", NA, NA),
    pathway = c("behavioral", "behavioral", "behavioral"),
    established = c(TRUE, FALSE, TRUE),
    evidence = c("RCT", "observational", "mechanistic"),
    notes = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  tryCatch({
    dag <- cauda.claims_to_dag(claims, verbose = TRUE)

    if (is.null(dag)) {
      cat("\n❌ FAIL: DAG creation returned NULL\n")
      return(FALSE)
    }

    cat("\n✓ PASS: DAG generated successfully\n")
    return(TRUE)

  }, error = function(e) {
    cat("\n❌ FAIL: DAG generation error:", e$message, "\n")
    return(FALSE)
  })
}

# Run all tests
run_all_tests <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   CAUDA CLAIMS EXTRACTION TEST SUITE                      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results <- list()

  results$test1 <- test_sample_text()
  results$test2 <- test_claim_type_normalization()
  results$test3 <- test_confidence_normalization()
  results$test4 <- test_pathway_normalization()
  results$test5 <- test_dag_generation()

  # Summary
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║   TEST SUMMARY                                             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  passed <- sum(unlist(results))
  total <- length(results)

  cat("\nTests Passed:", passed, "/", total, "\n")

  if (passed == total) {
    cat("\n✓ ALL TESTS PASSED\n")
  } else {
    cat("\n❌ SOME TESTS FAILED\n")
  }

  cat("\n")
  return(passed == total)
}

# Run tests when script is sourced
if (!interactive() || exists("run_tests")) {
  run_all_tests()
}
