# =============================================================================
# CAUDA - Master Startup Script
# =============================================================================
# Load the complete CAUDA system:
# - Data analysis (DAG, correlation, optimization)
# - Paper analysis (extract causal claims)
#
# USAGE: source("startup.R")
# =============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                  CAUDA v1.0 - Loading System                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Load data analysis
cat("📦 Loading data analysis...\n")
source("cauda.R", local = TRUE)
cat("   ✓ cauda.R loaded (DAG, correlation, optimize, etc.)\n\n")

# Load paper analysis
cat("📄 Loading paper analysis...\n")
source("load.R", local = TRUE)
cat("   ✓ paper.R loaded (extract claims, validation, metrics)\n\n")

cat("✅ CAUDA system ready!\n")
cat("   • cauda.analyze(df) - Full data analysis pipeline\n")
cat("   • cauda.analyze_papers(papers) - Extract from papers\n\n")
