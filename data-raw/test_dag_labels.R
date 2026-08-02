# Stress-test cauda.dag_theory() label rendering on a big graph with long
# labels, similar in shape to the ~29-node tPBM/insomnia DAG Aadi flagged as
# having illegibly truncated text.

long_labels_from <- c(
  "tPBM therapy", "tPBM therapy", "tPBM therapy", "tPBM therapy", "tPBM therapy",
  "EEG changes", "framework accounting for neurophysiological concept structure",
  "GAN-based oversampling", "DA-optimized XGBoost", "age range of 20-29 years",
  "framework accounting for neurophysiological concept structure",
  "CPTabKAN-Second Order", "concept-structured, interaction-aware tabular learning",
  "algorithmic complexity measures", "concept importance findings",
  "concept-structured, interaction-aware tabular learning", "tPBM therapy",
  "insomnia intervention", "sleep disturbance improvements", "attentional control",
  "tolerability", "deep sleep architecture", "circadian rhythm regulation",
  "cortical excitability measures", "melatonin secretion pathway",
  "hypothalamic-pituitary-adrenal axis activity", "REM sleep latency"
)
long_labels_to <- c(
  "insomnia alleviation and neural efficiency", "sleep disturbance improvements",
  "sleep disturbance", "attentional control", "insomnia intervention",
  "sleep outcomes", "improvement in MCI classification",
  "class imbalance", "generalization and prediction",
  "substance abuse and psychological distress", "model's decision logic",
  "classification performance", "clinical trust",
  "informative joint interpretation", "model's decision logic",
  "interpretable biomedical classification", "tolerability",
  "quality of life", "daytime functioning", "cognitive performance",
  "patient adherence", "memory consolidation", "mood regulation",
  "neural plasticity", "sleep onset latency", "stress reactivity",
  "next-day alertness"
)

claims <- data.frame(
  paper_id       = paste0("paper", seq_along(long_labels_from)),
  source         = long_labels_from,
  target         = long_labels_to,
  claim          = "synthetic test claim",
  claim_type     = "causal_effect",
  confidence     = "medium",
  established    = TRUE,
  pathway        = "physiological",
  verbatim_quote = "synthetic test quote",
  stringsAsFactors = FALSE
)

dag <- cauda.claims_to_dag(claims, verbose = FALSE)
cat("Nodes:", length(bnlearn::nodes(dag)), "\n")

cauda.save("data-raw/test_dag_labels.png", cauda.dag_theory(dag, verbose = FALSE),
           width = 2400, height = 1800, res = 200)
