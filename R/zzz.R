# Package initialization - load everything
.onLoad <- function(libname, pkgname) {
  # Get package directory
  pkg_dir <- system.file(package = pkgname)

  # Just print ready message
  cat("\n╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                  CAUDA v1.0 - Ready!                          ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  cat("✅ All functions loaded:\n")
  cat("   • cauda.extract()       - Extract causal claims from a paper\n")
  cat("   • cauda.critique()      - Evaluate claim strength & evidence gaps\n")
  cat("   • cauda.synthesize()    - Generate synthesis report\n")
  cat("   • cauda.claims_to_dag() - Build causal DAG from claims\n")
  cat("   • cauda.dag_theory()    - Plot pathway-colored DAG\n")
  cat("   • cauda.analyze()       - Data analysis (numeric datasets)\n\n")
}
