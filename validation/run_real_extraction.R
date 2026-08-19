# Run this in RStudio, in the git-ready project (where cauda is loaded).
# Produces genuine cauda.extract() output on the 4 real opioid theory PDFs
# and saves it as JSON so it can be shared back and used in the paper's
# Validation (Section 4) and Opioid Application (Section 5) sections.

library(cauda)

papers <- list(
  A_purdue_marketing   = "../opioid_papers/opioid_theoryA_purdue_marketing.pdf",
  B_deaths_of_despair  = "../opioid_papers/opioid_theoryB_deaths_of_despair.pdf",
  C_supply_substitution = "../opioid_papers/opioid_theoryC_supply_substitution.pdf",
  D_fentanyl_trade     = "../opioid_papers/opioid_theoryD_fentanyl_trade.pdf"
)

real_claims <- cauda.extract_multi(papers, is_pdf = TRUE)

jsonlite::write_json(
  real_claims,
  "real_extraction_output.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("\nSaved to:", normalizePath("real_extraction_output.json"), "\n")
cat("Rows extracted:", nrow(real_claims), "\n")
print(table(real_claims$paper_id))
