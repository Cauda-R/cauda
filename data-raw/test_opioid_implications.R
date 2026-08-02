# Quick end-to-end test of cauda.test_implications() using the real claims
# extracted from the 4 opioid theory papers (task #38) and the real opioid
# dataset built by build_opioid_test_data.R (task #42).

claims <- jsonlite::fromJSON(
  "/Users/aadisoni/Library/Application Support/Claude/local-agent-mode-sessions/08df209f-df16-4127-89d1-58bc7800f609/812bc3cf-0973-4139-a3ea-22f8d18084d6/local_a4ebf56f-6070-4e6a-a9e4-8498653a81b1/outputs/claims.json"
)
claims$theory <- substr(claims$paper_id, 1, 1)

real_data <- read.csv("inst/extdata/opioid_real_data.csv", stringsAsFactors = FALSE)

theory_names <- c(
  A = "A: Purdue marketing / overprescribing",
  B = "B: Deaths of despair",
  C = "C: Supply restriction -> heroin substitution",
  D = "D: Fentanyl supply contamination"
)

dag_list <- list()
for (th in names(theory_names)) {
  sub <- claims[claims$theory == th, ]
  dag_list[[theory_names[th]]] <- cauda.claims_to_dag(sub, verbose = FALSE)
}

cat("=== DAG node names per theory (display labels) ===\n")
for (nm in names(dag_list)) {
  cat("\n", nm, ":\n", sep = "")
  print(unname(attr(dag_list[[nm]], "display_lookup")))
}

# Mapping notes (real, free, keyless public data only — CDC VSRR overdose
# deaths by drug category + DEA ARCOS pill-volume totals):
#   - Most of each theory's causal chain runs through constructs no public
#     dataset directly measures (physician prescribing norms, psychosocial
#     despair, OUD prevalence, drug-purity/adulteration). Those nodes are
#     deliberately left UNMAPPED rather than force-fit to a poor proxy --
#     cauda.test_implications() reports them as untestable-with-reason
#     instead of silently skipping them.
#   - "Illicit synthetic fentanyl market" -> synthetic_opioid_deaths and
#     "Heroin substitution" -> heroin_deaths are the two ROOT causes feeding
#     the collider in Theory D ("...supply contamination"). Basis-set
#     implications test root causes of a collider for UNCONDITIONAL
#     independence (Z = {}), so this pair is testable with NO intermediate
#     mediator data needed -- the one clean real test this dataset supports.
var_map <- c(
  "Overdose death"                                  = "overdose_deaths_total",
  "Heroin substitution"                             = "heroin_deaths",
  "Illicit synthetic fentanyl market (from ~2013)"  = "synthetic_opioid_deaths",
  "Population Rx opioid exposure"                   = "arcos_total_pills_2006_2014"
)

cat("\n\n=== Running cauda.test_implications_compare() ===\n\n")
cmp <- cauda.test_implications_compare(dag_list, real_data, var_map, verbose = TRUE)

saveRDS(list(dag_list = dag_list, var_map = var_map, cmp = cmp),
        "data-raw/opioid_test_implications_result.rds")
cat("\nSaved data-raw/opioid_test_implications_result.rds\n")
