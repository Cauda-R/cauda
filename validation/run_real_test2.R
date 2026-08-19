# Map genuinely-extracted DAG nodes to real CDC/DEA columns and run the
# empirical test, using dag_list built in run_real_test.R (already in the
# R session).

var_map <- c(
  "OxyContin distribution"          = "arcos_total_pills_2006_2014",
  "overdose deaths"                 = "overdose_deaths_total",
  "opioid-related overdose deaths"  = "overdose_deaths_total",
  "heroin deaths"                   = "heroin_deaths",
  "natural-opioid deaths"           = "natural_semisynth_deaths"
)

cat("\n\n=== Running cauda.test_implications_compare() on REAL extraction output ===\n\n")
cmp2 <- cauda.test_implications_compare(dag_list, real_data, var_map, verbose = TRUE)

saveRDS(list(dag_list = dag_list, var_map = var_map, cmp = cmp2),
        "real_opioid_test_implications_result.rds")
cat("\nSaved real_opioid_test_implications_result.rds\n")

# Also print each theory's edge list with claim_type, so we can see exactly
# which claims survived the causal_effect/mechanism filter in claims_to_dag()
cat("\n\n=== Edges retained per theory (claims_to_dag filters to causal_effect/mechanism) ===\n")
for (pid in names(theory_names)) {
  sub <- real_claims[real_claims$paper_id == pid, c("source","target","claim_type","claim_class")]
  cat("\n", theory_names[pid], ":\n", sep = "")
  print(sub)
}
