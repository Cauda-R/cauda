# =============================================================================
# CAUDA STARTUP - Main Entry Point
# =============================================================================
# Source this script to load the complete CAUDA ecosystem
#
# Usage:
#   source("cauda-startup.R")
#
# Then use:
#   cauda.analyze(df, highlight = "target_var")          # Data analysis
#   cauda.analyze_papers(papers, job_name = "...")       # Paper analysis
#   cauda.dag(df, highlight = "target_var")              # Causal DAG learning
#   ... and many more!

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║   CAUDA: Causal Automated Unified Data Analysis           ║\n")
cat("║   Complete Enterprise Ecosystem (v2.0)                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Get the directory where this script is located
script_dir <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))

# Load all modules
cat("Loading CAUDA ecosystem...\n")

# Core framework
source(file.path(script_dir, "R/core/cauda-core-framework.R"))

# Production pipeline (batch processing)
source(file.path(script_dir, "R/extraction/production-batch-pipeline.R"))
source(file.path(script_dir, "R/extraction/confidence-aware-dag-visualization.R"))

# Metrics and analysis
source(file.path(script_dir, "R/metrics/metric-refinement-and-confidence-analysis.R"))
source(file.path(script_dir, "R/metrics/advanced-metrics-refinement.R"))

# Robustness and error handling
source(file.path(script_dir, "R/robustness/robustness-and-error-handling.R"))

# Performance optimization
source(file.path(script_dir, "R/performance/performance-optimization.R"))

# Full integration (batch processing + paper analysis)
source(file.path(script_dir, "R/integration/cauda-batch-integration.R"))
source(file.path(script_dir, "R/integration/cauda-paper-analysis.R"))

# =============================================================================
# STARTUP COMPLETE
# =============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              ✅ CAUDA READY FOR USE                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("📊 AVAILABLE FUNCTIONS:\n\n")

cat("DATA ANALYSIS:\n")
cat("  • cauda.analyze(df, highlight='var')    - Complete analysis pipeline\n")
cat("  • cauda.dag(df, highlight='var')        - Learn causal structure\n")
cat("  • cauda.corr(df, highlight='var')       - Correlation analysis\n")
cat("  • cauda.pcorr(df, highlight='var')      - Partial correlation\n")
cat("  • cauda.prep(df)                        - Auto-prepare data\n")
cat("  • cauda.optimize(df, target, dir)       - Optimization\n\n")

cat("PAPER ANALYSIS:\n")
cat("  • cauda.analyze_papers(papers, ...)     - Extract causal claims from papers\n")
cat("  • cauda.papers_summary(results)         - Summary statistics\n")
cat("  • cauda.papers_quality_gates(results)   - Quality assessment\n")
cat("  • cauda.papers_metrics(results)         - Advanced metrics\n")
cat("  • cauda.papers_anomalies(results)       - Anomaly detection\n\n")

cat("BATCH PROCESSING:\n")
cat("  • cauda.batch_process(papers, ...)      - Process multiple papers\n\n")

cat("Type '?cauda.analyze' or '?cauda.analyze_papers' for detailed help.\n")
cat("Visit the README for complete documentation.\n\n")

invisible(NULL)
