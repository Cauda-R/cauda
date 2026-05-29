# =============================================================================
# CAUDA MASTER LOAD SCRIPT
# =============================================================================
# Loads entire CAUDA enterprise ecosystem when package is attached

load_cauda_enterprise <- function() {
  # Core framework (plugin system, file management, caching)
  source("R/core/cauda-core-framework.R")

  # Production pipeline (batch processing engine)
  source("R/extraction/production-batch-pipeline.R")
  source("R/extraction/confidence-aware-dag-visualization.R")

  # Metrics and statistical analysis
  source("R/metrics/metric-refinement-and-confidence-analysis.R")
  source("R/metrics/advanced-metrics-refinement.R")

  # Robustness and error handling
  source("R/robustness/robustness-and-error-handling.R")

  # Performance optimization
  source("R/performance/performance-optimization.R")

  # Full ecosystem integration
  source("R/integration/cauda-batch-integration.R")
  source("R/integration/cauda-paper-analysis.R")

  cat("\n✓ CAUDA Enterprise System loaded\n\n")
}

# Auto-load on package attach
.onLoad <- function(libname, pkgname) {
  load_cauda_enterprise()
}
