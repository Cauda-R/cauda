# Run this in RStudio, in the git-ready project (where cauda is loaded).
# Produces genuine cauda.extract() output on the 4 real UPF (ultra-processed
# food) theory PDFs, for Section 6 of the paper.

library(cauda)

papers <- list(
  A_processing_causal    = "../upf_papers/upf_theoryA_processing_causal.pdf",
  B_nutrient_profile     = "../upf_papers/upf_theoryB_nutrient_profile.pdf",
  C_emulsifier_microbiome = "../upf_papers/upf_theoryC_emulsifier_microbiome.pdf",
  D_confounding           = "../upf_papers/upf_theoryD_confounding.pdf"
)

upf_claims <- cauda.extract_multi(papers, is_pdf = TRUE)

jsonlite::write_json(
  upf_claims,
  "upf_extraction_output.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("\nSaved to:", normalizePath("upf_extraction_output.json"), "\n")
cat("Rows extracted:", nrow(upf_claims), "\n")
print(table(upf_claims$paper_id))
