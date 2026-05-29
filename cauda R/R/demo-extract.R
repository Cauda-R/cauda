# =============================================================================
# demo-extract.R
# Simple step-by-step demo of the extraction module
#
# Usage: Copy-paste sections into your R console one at a time
#
# This demonstrates:
#   1. Loading the extraction module
#   2. Running tests (no API needed)
#   3. Using extract_claims with ChatGPT (requires API)
#   4. Converting claims to DAGs
#   5. Visualizing with cauda
#
# =============================================================================


# ============================================================
# SETUP (RUN THESE ONCE)
# ============================================================

# Set your working directory to where you copied the files
# setwd("~/Downloads/ML Projects/cauda R")

# Load cauda first
source("cauda.R")

# Load the extraction attachment
source("cauda-extract.R")

# Load test functions
source("test-extract.R")

cat("All modules loaded successfully!\n\n")


# ============================================================
# TEST 1: Smoke test (NO API NEEDED - run this first!)
# ============================================================

cat("=== Running smoke test (quick, no API needed) ===\n")
result_smoke <- smoke_test()

# If this passes, everything is working


# ============================================================
# TEST 2: Test with synthetic claims (NO API NEEDED)
# ============================================================

cat("=== Testing with synthetic claims ===\n")
result_synthetic <- test_with_fake_claims()

# This shows precision/recall validation without needing ChatGPT


# ============================================================
# TEST 3: Full pipeline with ChatGPT (REQUIRES API KEY)
# ============================================================

# To enable this section:
# 1. Get API key from https://platform.openai.com/account/api-keys
# 2. Set it here:
# Sys.setenv(OPENAI_API_KEY = "sk-proj-YOUR_KEY_HERE")
# 3. Uncomment and run below:

# result_full <- test_full_pipeline()


# ============================================================
# SECTION 4: Extract from your own paper
# ============================================================

# After you have API key and validate the tests, try this:

# my_paper_text <- "
# Your paper text here...
# Copy-paste text from a paper, or use:
# my_paper_text <- pdftools::pdf_text('paper.pdf') %>% paste(collapse = '\n')
# "

# my_claims <- extract_claims(
#   my_paper_text,
#   domain = "opioid crisis",
#   model = "gpt-4-mini",
#   verbose = TRUE
# )

# head(my_claims, 10)  # See first 10 claims


# ============================================================
# SECTION 5: Convert to DAG and visualize
# ============================================================

# my_dag <- claims_to_dag(my_claims, verbose = TRUE)

# # Use cauda to visualize
# cauda.dag(my_dag, highlight = "Overdose", verbose = FALSE)


# ============================================================
# SECTION 6: Validate against ground truth
# ============================================================

# If you have a reference DAG:
# validation <- validate_extraction(my_dag, truth_dag)
# print(validation$precision)  # How accurate?
# print(validation$recall)     # How complete?
