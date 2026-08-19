# Build DAGs from genuine UPF extraction output and inspect node structure.

upf_claims <- jsonlite::fromJSON("upf_extraction_output.json")

upf_theory_names <- c(
  A_processing_causal     = "A: Processing itself causes overconsumption (Hall 2019 RCT)",
  B_nutrient_profile      = "B: Nutrient profile, not processing, is what matters (Poti 2017)",
  C_emulsifier_microbiome = "C: Emulsifier-microbiome mechanism (Chassaing 2015)",
  D_confounding           = "D: Confounding explains the association (Robinson & Jones 2024)"
)

upf_dag_list <- list()
for (pid in names(upf_theory_names)) {
  sub <- upf_claims[upf_claims$paper_id == pid, ]
  upf_dag_list[[upf_theory_names[pid]]] <- cauda.claims_to_dag(sub, verbose = FALSE)
}

cat("=== UPF DAG node names per theory (display labels) ===\n")
for (nm in names(upf_dag_list)) {
  cat("\n", nm, ":\n", sep = "")
  print(unname(attr(upf_dag_list[[nm]], "display_lookup")))
}

cat("\n\n=== Edges retained per theory (claims_to_dag filters to causal_effect/mechanism) ===\n")
for (pid in names(upf_theory_names)) {
  sub <- upf_claims[upf_claims$paper_id == pid, c("source","target","claim_type","claim_class")]
  cat("\n", upf_theory_names[pid], ":\n", sep = "")
  print(sub)
}
