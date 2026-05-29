================================================================================
cauda EXTRACTION ATTACHMENT - QUICK START
================================================================================

This is an OPTIONAL module that extends cauda for research use.
It does NOT change or modify the main cauda package.

FILES:
  - cauda-extract.R    (core functions - ~460 lines)
  - test-extract.R     (test harness - ~250 lines)
  - demo-extract.R     (simple examples - ~100 lines)

================================================================================
WHAT IT DOES
================================================================================

Extracts causal theories from scientific papers:

  Paper Text → ChatGPT API → Causal Claims → DAG → Compare with Data DAGs

FOUR FUNCTIONS:
  1. extract_claims(text, domain, model, api_key)
     Input: Paper text
     Output: Table of causal claims
     Cost: $0.01-0.02 per paper

  2. extract_from_pdf(pdf_path, ...)
     Input: PDF file path
     Output: Table of causal claims

  3. claims_to_dag(claims, confidence_threshold, include_speculative)
     Input: Claims table
     Output: bnlearn DAG object

  4. validate_extraction(extracted_dag, ground_truth_dag)
     Input: Two DAGs (extracted vs truth)
     Output: Precision, recall, F1-score

================================================================================
QUICK START (10 minutes)
================================================================================

ON YOUR MACHINE:

1. Copy files to your cauda R folder:
   ~/Downloads/ML Projects/cauda R/
   ├── cauda-extract.R       (new)
   ├── test-extract.R        (new)
   └── demo-extract.R        (new)

2. Install required packages (if needed):
   install.packages("httr2")
   install.packages("jsonlite")

3. Open R and test:
   setwd("~/Downloads/ML Projects/cauda R")
   source("cauda.R")
   source("cauda-extract.R")
   source("test-extract.R")
   smoke_test()              # Should pass in 5 seconds

4. That's it! The module is working.

================================================================================
NEXT STEPS (Add ChatGPT)
================================================================================

1. Get API key: https://platform.openai.com/account/api-keys

2. Set environment variable:
   Sys.setenv(OPENAI_API_KEY = "sk-proj-YOUR_KEY_HERE")

3. Run full test:
   test_full_pipeline()

4. Extract your own paper:
   claims <- extract_claims(your_text, domain = "opioid crisis")
   dag <- claims_to_dag(claims)
   cauda.dag(dag, highlight = "Overdose")

================================================================================
FUNCTION SIGNATURES
================================================================================

extract_claims(text, domain = "opioid crisis", model = "gpt-4-mini",
               api_key = NULL, verbose = TRUE)
→ Data frame with columns: claim_type, source, target, pathway, direction,
  strength, confidence, established, quote

extract_from_pdf(pdf_path, ...)
→ Same as extract_claims (calls extract_claims internally)

claims_to_dag(claims, confidence_threshold = "low", include_speculative = TRUE,
              verbose = TRUE)
→ bnlearn bn object with metadata

validate_extraction(extracted_dag, ground_truth_dag, verbose = TRUE)
→ List with: precision, recall, f1, true_positives, false_positives,
  false_negatives, direction_errors

================================================================================
TEST FUNCTIONS (No API needed)
================================================================================

smoke_test()                # Quick sanity check (5 sec)
test_with_fake_claims()     # Test validation logic (10 sec)
test_full_pipeline()        # Full pipeline with ChatGPT (30 sec, needs API)

================================================================================
DEPENDENCIES
================================================================================

Required (for core functions):
  - bnlearn (already installed for cauda)
  - httr2   (install.packages("httr2"))
  - jsonlite (install.packages("jsonlite"))

Optional (for PDF extraction):
  - pdftools (install.packages("pdftools"))

Required for API:
  - OpenAI API key ($5-10 for testing, free tier available)

================================================================================
HOW IT FITS WITH CAUDA
================================================================================

CAUDA (core):
  - Learns causal structures FROM DATA using statistical methods
  - Main functions: cauda.dag(), cauda.prep(), cauda.optimize(), etc.
  - Your existing workflow is unchanged

EXTRACTION (attachment):
  - Extracts causal structures FROM PAPERS using LLMs
  - Supplements cauda for theoretical analysis
  - No code changes to cauda needed
  - Use it alongside cauda, not instead of it

================================================================================
COMMON WORKFLOW
================================================================================

1. Load cauda (existing)
   source("cauda.R")

2. Load extraction (optional)
   source("cauda-extract.R")
   source("test-extract.R")

3. Validate extraction works (no API)
   smoke_test()

4. Extract from papers (with API)
   claims <- extract_claims(paper_text, domain = "opioid crisis")

5. Convert to DAG
   theory_dag <- claims_to_dag(claims)

6. Compare with data DAGs
   data_dag <- cauda.dag(data_df)
   validate_extraction(theory_dag, data_dag)

7. Investigate disagreements = research insights

================================================================================
TROUBLESHOOTING
================================================================================

Error: "httr2 required"
→ Run: install.packages("httr2")

Error: "OPENAI_API_KEY not set"
→ Run: Sys.setenv(OPENAI_API_KEY = "sk-proj-YOUR_KEY")

Error: "bnlearn required"
→ Run: install.packages("bnlearn")

smoke_test() fails
→ Something is broken; start fresh with source() calls

API call fails with 401
→ Your API key is wrong; get a new one from OpenAI

================================================================================
SUPPORT
================================================================================

Check demo-extract.R for usage examples
Check INDEX.md in /mnt/project/ for full documentation
Check EXTRACTION_REFERENCE.md for detailed function docs

Questions? Ask Tony or re-read the docs.

================================================================================
THAT'S IT!

You have:
  ✓ cauda (existing package, unchanged)
  ✓ cauda-extract.R (extraction functions)
  ✓ test-extract.R (test suite)
  ✓ demo-extract.R (usage examples)

Next: Copy to your machine and run smoke_test()

Good luck!
================================================================================
