# Build DAGs from the GENUINE extraction output (real_extraction_output.json)
# and run cauda.test_implications_compare() against the real CDC/DEA dataset.
# This replaces the earlier claims.json-based test, which was built on
# hand-authored stand-in data that didn't match the real source PDFs.

real_claims <- jsonlite::fromJSON("real_extraction_output.json")

theory_names <- c(
  A_purdue_marketing    = "A: Purdue marketing / triplicate-law natural experiment",
  B_deaths_of_despair   = "B: Deaths of despair",
  C_supply_substitution = "C: Supply restriction -> heroin substitution",
  D_fentanyl_trade      = "D: International trade -> fentanyl smuggling"
)

dag_list <- list()
for (pid in names(theory_names)) {
  sub <- real_claims[real_claims$paper_id == pid, ]
  dag_list[[theory_names[pid]]] <- cauda.claims_to_dag(sub, verbose = FALSE)
}

cat("=== DAG node names per theory (display labels) ===\n")
for (nm in names(dag_list)) {
  cat("\n", nm, ":\n", sep = "")
  print(unname(attr(dag_list[[nm]], "display_lookup")))
}

real_data <- read.csv("inst/extdata/opioid_real_data.csv", stringsAsFactors = FALSE)
cat("\n=== real_data columns ===\n")
print(names(real_data))
