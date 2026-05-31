# Package initialization - load everything
.onLoad <- function(libname, pkgname) {
  # Get package directory
  pkg_dir <- system.file(package = pkgname)

  # Just print ready message
  cat("\n╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                  CAUDA v1.0 - Ready!                          ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  cat("✅ All functions loaded:\n")
  cat("   • cauda.analyze() - Data analysis\n")
  cat("   • cauda.analyze_papers() - Paper analysis\n")
  cat("   • And 40+ more functions\n\n")
}
