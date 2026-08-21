# CAUDA

**What is CAUDA?** An R package with two independent capabilities: (1) extracting causal claims from academic papers, assembling them into source-traceable directed acyclic graphs (DAGs), and testing those graphs' implications against real data, and (2) learning a causal DAG directly from tabular data using structure-learning algorithms.

**Status:** In active development. See [Known Issues](#known-issues) below — some functions in this repository are not currently working.
**License:** MIT

---

## 1. Literature → DAG → Empirical Test pipeline

This is CAUDA's primary, actively maintained capability, and the one used to produce every result in the accompanying paper. Given one or more papers, it extracts causal claims with a source quotation, builds a DAG with per-edge provenance, critiques each claim's evidentiary strength, derives the DAG's testable conditional-independence implications via `dagitty`, and tests those implications against a real dataset you supply.

```r
library(cauda)
Sys.setenv(OPENAI_API_KEY = "sk-your-api-key")

# 1. Extract claims from one or more PDFs (or raw text)
claims <- cauda.extract_pdf("paper.pdf", paper_id = "paper1")
# claims_multi <- cauda.extract_multi(list("paper1.pdf", "paper2.pdf"), is_pdf = TRUE)

# 2. Build a DAG from the extracted claims (source traceability preserved per edge)
dag <- cauda.claims_to_dag(claims)

# 3. Critique each claim's evidentiary support (confounding, reverse causation, selection)
claims_critiqued <- cauda.critique(claims)

# 4. Derive the DAG's testable implications
implications <- cauda.dagitty(dag)
implications$implied_CIs      # data frame of implied conditional independencies
cat(implications$dagitty_string)  # paste into https://dagitty.net for interactive editing

# 5. Comparing multiple theories? Derive implications for each DAG and see which
#    implications are shared vs. unique to one theory
comparison <- cauda.dagitty_compare(list(theoryA = dagA, theoryB = dagB))

# 6. Test implications against a real dataset you provide
var_map <- c("OxyContin distribution" = "arcos_total_pills",
             "overdose deaths"        = "overdose_deaths_total")
test_result <- cauda.test_implications(dag, my_data, var_map)
# test_result$untestable has a `reason` column naming exactly which
# variable had no mapped data column, for every implication that couldn't be tested
```

`cauda.synthesize()` and `cauda.validate_dag()` are also part of this pipeline: `cauda.synthesize(text, claims, critique)` produces a narrative synthesis of a paper's claims and their critique, and `cauda.validate_dag(extracted_dag, ground_truth_dag)` scores an extracted DAG against a hand-built reference DAG (used for the validation reported in the paper).

All of this is also available through the accompanying Shiny app, which adds interactive DAG editing and a guided real-data-mapping UI: **[live demo](https://aadisoni.shinyapps.io/cauda-paper-analysis/)** (tabs: Status & Summary, Detailed Claims, Critique, Synthesis, Causal DAG, Testable Implications, Compare Theories, Test Against Real Data).

For the full method description, worked examples on real papers, and an honest accounting of what this pipeline does and does not currently do well, see the accompanying paper (`validation/reference_claims.md` and the `*_extraction_output.json` / `*.rds` files in this repo are the real inputs and outputs behind that paper's results).

---

## 2. Tabular causal discovery

A separate, independent feature for learning a causal structure directly from a data frame, using `bnlearn` structure-learning algorithms plus correlation and partial-correlation analysis. This does not use claim extraction or an LLM.

```r
library(cauda)

data <- data.frame(
  Age      = c(25, 30, 45, 50),
  Exercise = c(3, 5, 2, 1),
  Health   = c(7, 8, 5, 4)
)

# Interactive, all-in-one walkthrough (pauses between steps with readline() —
# run this in an interactive R session, not a script or Shiny server)
results <- cauda.analyze(data, target = "Health")

# Or call the individual steps directly, which is what you want for scripted use:
dag   <- cauda.dag(data)          # learn the causal network
corr  <- cauda.corr(data)         # Pearson / Spearman / Kendall correlations
pcorr <- cauda.pcorr(data)        # partial correlations (direct effects)
opt   <- cauda.optimize(data)     # recommended variable values
cons  <- cauda.consensus(data)    # structure learned across several bnlearn algorithms
```

---

## Installation

```r
remotes::install_github("Cauda-R/cauda")
```

If a dependency fails to install:

```r
install.packages(c("bnlearn", "ppcor", "iml", "tidyverse", "dagitty", "igraph", "qgraph", "corrplot"))
remotes::install_github("Cauda-R/cauda", force = TRUE)
```

### OpenAI setup (required for the extraction/critique/synthesis pipeline only)

1. Create a key at <https://platform.openai.com/api/keys>.
2. `Sys.setenv(OPENAI_API_KEY = "sk-your-actual-key")`, or add it to a `.Renviron` file, which `cauda.extract()` and `cauda.critique()` read automatically.
3. Add a payment method at <https://platform.openai.com/account/billing> and set a usage limit.

The tabular causal-discovery functions in Section 2 above do not call OpenAI and don't need a key.

---

## Known issues

- **`cauda.analyze_papers()`, `cauda.batch_process()`, `cauda.papers_summary()`, `cauda.papers_metrics()`, `cauda.papers_anomalies()`, and `cauda.papers_quality_gates()`** (defined in `R/01-paper-core.R` and `R/02-paper.R`) are exported but not currently functional. `cauda.batch_process()` is a stub that returns an empty result set regardless of input, and the metrics it feeds into report fixed, hardcoded numbers rather than anything computed from your data. **Do not use this batch-processing pathway.** Use the pipeline in Section 1 above (`cauda.extract_pdf()` / `cauda.extract_multi()` → `cauda.claims_to_dag()` → `cauda.critique()` → `cauda.dagitty()` → `cauda.test_implications()`), which is real, is what the Shiny app uses, and is what produced every result reported in the paper.
- **`cauda.analyze()`** calls `readline()` between steps and so requires an interactive R session; it will hang or error under `Rscript`, in a Shiny server process, or in any other non-interactive context. Call `cauda.dag()`, `cauda.corr()`, `cauda.pcorr()`, and `cauda.optimize()` directly instead for scripted or automated use.
- Claim extraction (`cauda.extract()` / `cauda.extract_pdf()` / `cauda.extract_multi()`) asks the underlying language model to quote source text verbatim, but does not currently verify that the returned quotation actually appears in the source PDF before attaching it to a claim; a meaningful fraction of quotations are paraphrases, and a smaller fraction contain phrasing not present in the source at all. Treat a claim's quotation as a pointer to where to look in the source paper, not as a substitute for checking it yourself. See the paper's discussion of this for the full audit.

---

## Package organization

```
R/
├── 00-load-all.R              Package initialization
├── 01-paper-core.R            Legacy batch-processing framework (see Known Issues)
├── 02-claims-extraction.R     Claim extraction (cauda.extract / extract_pdf / extract_multi)
├── 02-paper.R                 Legacy cauda.analyze_papers() wrapper (see Known Issues)
├── 03-critique-module.R       Evidentiary critique of each claim
├── 04-synthesis-module.R      Narrative synthesis of claims + critique
├── 05-dagitty-module.R        DAG → dagitty conversion, testable-implication derivation
├── 06-cauda-empirical-test.R  Testing implications against real data
├── 10-cauda.R                 DAG construction from claims + tabular causal discovery
└── zzz.R                      Final initialization

inst/shiny/     Accompanying Shiny app
validation/     Independently constructed reference claim set used to validate extraction
data-raw/       Sample datasets for testing
man/            Function documentation
tests/          Package tests
```

---

## Help

- `?cauda.extract`, `?cauda.claims_to_dag`, `?cauda.dagitty`, `?cauda.test_implications` — in-R documentation for the extraction/DAG/testing pipeline
- **GitHub Issues:** <https://github.com/Cauda-R/cauda/issues>
