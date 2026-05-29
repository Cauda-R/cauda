# ============================================================================
# BATCH TESTING PIPELINE FOR CAUSAL CLAIM EXTRACTION
# ============================================================================
# Tests the full end-to-end pipeline without API dependency
# Generates synthetic wind energy papers, extracts mock claims, builds DAGs,
# evaluates quality, and produces comprehensive reports

# ============================================================================
# PART 1: SYNTHETIC PAPER GENERATOR
# ============================================================================
# Generates realistic wind energy papers with varying claim patterns

generate_wind_paper_1_ideal <- function() {
  # PAPER 1: "Ideal" extraction - clean, well-structured claims
  # Scenario: Classic wind farm wake effects paper

  list(
    title = "Wake Effects on Wind Farm Performance",
    domain = "wind_energy",
    scenario = "ideal",
    text = "
      Wind farm efficiency is significantly reduced by wake effects.
      When a turbine operates, it creates a wake deficit that propagates
      downstream, affecting downwind turbines. Wake effects increase
      mechanical stress on turbine components by 15-30%. The severity
      depends on wind direction and farm layout geometry. Turbulence
      intensity in the wake region accelerates fatigue crack initiation.

      To mitigate these effects, careful layout design and control
      strategies are essential. Modern wind farms increasingly use
      SCADA data to model wake interactions.
    ",
    ground_truth_edges = data.frame(
      source = c("WakeDeficit", "WindDirection", "LayoutGeometry",
                 "TurbulenceIntensity", "MechanicalStress"),
      target = c("MechanicalStress", "WakeIntensity", "WakeInteractions",
                 "FatigueInitiation", "TurbineFailure"),
      stringsAsFactors = FALSE
    )
  )
}

generate_wind_paper_2_noisy <- function() {
  # PAPER 2: "Noisy" extraction - mixed signal with some irrelevant text
  # Scenario: Broader paper on renewable energy with wind section

  list(
    title = "Renewable Energy Systems and Grid Integration",
    domain = "wind_energy",
    scenario = "noisy",
    text = "
      Solar and wind energy are the fastest-growing renewable sources.
      Wind turbines convert kinetic energy into electricity efficiently.
      Wake effects in wind farms represent a critical challenge. When
      turbines are placed too close, wake propagation causes significant
      power loss—studies show 10-40% depending on layout. Furthermore,
      aerodynamic interference increases fatigue on downstream machines.

      The economics of wind farms depend on optimal turbine spacing.
      Battery storage and smart grids improve renewable integration.
      Wake modeling using Gaussian models or computational fluid dynamics
      provides better predictions. Transfer learning from high-fidelity
      simulations to field data is an emerging technique.
    ",
    ground_truth_edges = data.frame(
      source = c("WakeEffects", "TurbineSpacing", "AerodynamicInterference",
                 "WakeModeling", "TransferLearning"),
      target = c("PowerLoss", "DownstreamFatigue", "StructuralLoads",
                 "PredictionAccuracy", "FieldDataAccuracy"),
      stringsAsFactors = FALSE
    )
  )
}

generate_wind_paper_3_technical <- function() {
  # PAPER 3: "Technical" extraction - dense technical claims
  # Scenario: Machine learning for wind farm optimization

  list(
    title = "Graph Neural Networks for Wind Farm Dynamics",
    domain = "wind_energy",
    scenario = "technical",
    text = "
      Wind turbine interactions form a complex networked system amenable
      to graph representations. We propose a GNN architecture trained on
      PyWake simulation data to learn wake physics in a differentiable way.

      The model takes wind direction, inflow velocity, and farm layout as
      inputs and predicts damage-equivalent loads (DEL) for each turbine.
      Pre-training on synthetic PyWake data significantly improves
      fine-tuning performance on field data from real wind farms.

      Transfer learning reduces data requirements by 60% compared to
      training from scratch. The GNN learns latent representations of
      wake interactions that generalize across different farm layouts
      and wind regimes. Pathway analysis reveals that the model captures
      both direct wake effects and higher-order interactions between
      non-adjacent turbines.
    ",
    ground_truth_edges = data.frame(
      source = c("GNNArchitecture", "PyWakePretraining", "WindDirectionInput",
                 "InflowVelocity", "LayoutRepresentation", "TransferLearning",
                 "LatentRepresentations"),
      target = c("DELPrediction", "FineTuningPerformance", "WakePrediction",
                 "LoadPrediction", "InteractionCapture", "DataRequirementReduction",
                 "GeneralizationAcrossLayouts"),
      stringsAsFactors = FALSE
    )
  )
}

generate_wind_paper_4_missing <- function() {
  # PAPER 4: "Missing edges" - extractable but incomplete claims
  # Scenario: Wind farm control strategies

  list(
    title = "Control Strategies for Wind Farm Optimization",
    domain = "wind_energy",
    scenario = "missing_edges",
    text = "
      Coordinated control can mitigate wake effects. Individual turbine
      yaw control has been shown to reduce downstream effects. Wind farm
      control strategies optimize for either maximum power or load
      alleviation or both.

      Wake losses affect overall farm revenue. Modeling uncertainty from
      wind speed variability complicates control design. Real-time SCADA
      data enables adaptive strategies. Yaw control effectiveness depends
      on wind direction consensus across the farm.
    ",
    ground_truth_edges = data.frame(
      source = c("CoordinatedControl", "YawControl", "WindSpeedVariability",
                 "SCADAData", "WindDirectionConsensus"),
      target = c("WakeMitigation", "DownstreamEffects", "ControlUncertainty",
                 "AdaptiveStrategy", "ControlEffectiveness"),
      stringsAsFactors = FALSE
    )
  )
}

# ============================================================================
# PART 2: MOCK CLAIM EXTRACTOR
# ============================================================================
# Simulates what the API would extract from each paper

extract_mock_claims <- function(paper_obj, scenario = "perfect") {
  # Scenario modes:
  # - "perfect": extract all ground truth edges with no errors
  # - "realistic": capture 80% with 1-2 hallucinations
  # - "degraded": capture 60% with more hallucinations

  gt_edges <- paper_obj$ground_truth_edges

  if (scenario == "perfect") {
    # All edges extracted perfectly
    extracted <- gt_edges
    extracted$claim_type <- "causal_effect"
    extracted$pathway <- NA_character_
    extracted$direction <- "positive"
    extracted$strength <- "high"
    extracted$confidence <- "high"
    extracted$established <- TRUE
    extracted$quote <- paste(extracted$source, "leads to", extracted$target)

  } else if (scenario == "realistic") {
    # 80% recall, ~1 hallucination
    n_gt <- nrow(gt_edges)
    keep_idx <- sample(1:n_gt, size = ceiling(0.8 * n_gt), replace = FALSE)
    extracted <- gt_edges[keep_idx, ]

    # Add 1 hallucination
    halluc <- data.frame(
      source = "RandomNoise",
      target = "UnrelatedVar",
      stringsAsFactors = FALSE
    )
    extracted <- rbind(extracted, halluc)

    extracted$claim_type <- "causal_effect"
    extracted$pathway <- NA_character_
    extracted$direction <- "positive"
    extracted$strength <- ifelse(is.na(extracted$source), "low", "high")
    extracted$confidence <- ifelse(is.na(extracted$source), "low", "high")
    extracted$established <- !is.na(extracted$source)
    extracted$quote <- paste(extracted$source, "affects", extracted$target)

  } else if (scenario == "degraded") {
    # 60% recall, more hallucinations
    n_gt <- nrow(gt_edges)
    keep_idx <- sample(1:n_gt, size = ceiling(0.6 * n_gt), replace = FALSE)
    extracted <- gt_edges[keep_idx, ]

    # Add 2-3 hallucinations
    halluc1 <- data.frame(source = "NoiseA", target = "NoiseB", stringsAsFactors = FALSE)
    halluc2 <- data.frame(source = "NoiseC", target = "NoiseD", stringsAsFactors = FALSE)
    extracted <- rbind(extracted, halluc1, halluc2)

    extracted$claim_type <- "causal_effect"
    extracted$pathway <- NA_character_
    extracted$direction <- "positive"
    extracted$strength <- ifelse(grepl("Noise", extracted$source), "low", "high")
    extracted$confidence <- ifelse(grepl("Noise", extracted$source), "low", "medium")
    extracted$established <- !grepl("Noise", extracted$source)
    extracted$quote <- paste(extracted$source, "→", extracted$target)
  }

  return(extracted)
}

# ============================================================================
# PART 3: BATCH EVALUATION FRAMEWORK
# ============================================================================

evaluate_single_paper <- function(paper_obj, extracted_claims,
                                  verbose = TRUE) {
  # Compare extracted claims to ground truth

  gt_edges <- paper_obj$ground_truth_edges

  # Clean extracted claims
  extracted_clean <- extracted_claims[!is.na(extracted_claims$source) &
                                       !is.na(extracted_claims$target), ]

  # Build edge sets for comparison
  gt_edge_set <- paste(gt_edges$source, "->", gt_edges$target)
  extracted_edge_set <- paste(extracted_clean$source, "->", extracted_clean$target)

  # Calculate metrics
  tp <- length(intersect(gt_edge_set, extracted_edge_set))
  fp <- length(setdiff(extracted_edge_set, gt_edge_set))
  fn <- length(setdiff(gt_edge_set, extracted_edge_set))

  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0
  f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  # Confidence calibration
  high_conf <- extracted_clean[extracted_clean$confidence == "high", ]
  high_correct <- sum(paste(high_conf$source, "->", high_conf$target) %in% gt_edge_set)
  high_calibration <- if (nrow(high_conf) > 0) high_correct / nrow(high_conf) else 0

  # Hallucination rate
  halluc_rate <- fp / max(length(extracted_edge_set), 1)

  results <- list(
    paper_title = paper_obj$title,
    scenario = paper_obj$scenario,
    domain = paper_obj$domain,

    gt_edges = nrow(gt_edges),
    extracted_edges = nrow(extracted_clean),
    true_positives = tp,
    false_positives = fp,
    false_negatives = fn,

    precision = precision,
    recall = recall,
    f1 = f1,
    high_confidence_calibration = high_calibration,
    hallucination_rate = halluc_rate,

    composite_score = mean(c(f1, high_calibration, 1 - halluc_rate)),

    ready_for_production = (mean(c(f1, high_calibration, 1 - halluc_rate)) > 0.80)
  )

  if (verbose) {
    cat("\n========================================\n")
    cat("PAPER:", results$paper_title, "\n")
    cat("Scenario:", results$scenario, "\n")
    cat("========================================\n")
    cat(sprintf("Edge accuracy: %.3f (P: %.3f, R: %.3f, F1: %.3f)\n",
                results$f1, results$precision, results$recall, results$f1))
    cat(sprintf("High-confidence calibration: %.3f\n", results$high_confidence_calibration))
    cat(sprintf("Hallucination rate: %.3f\n", results$hallucination_rate))
    cat(sprintf("COMPOSITE SCORE: %.3f %s\n",
                results$composite_score,
                ifelse(results$ready_for_production, "✓ READY", "⚠ NEEDS WORK")))
    cat("========================================\n")
  }

  return(results)
}

# ============================================================================
# PART 4: BATCH RUNNER
# ============================================================================

run_batch_test <- function(verbose = TRUE) {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║       BATCH PIPELINE TEST - WIND ENERGY DOMAIN            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Generate all synthetic papers
  papers <- list(
    generate_wind_paper_1_ideal(),
    generate_wind_paper_2_noisy(),
    generate_wind_paper_3_technical(),
    generate_wind_paper_4_missing()
  )

  all_results <- list()

  # Extract and evaluate each paper with different extraction scenarios
  for (i in seq_along(papers)) {
    paper <- papers[[i]]

    if (verbose) {
      cat("\n--- PAPER", i, ":", paper$title, "---\n")
      cat("Scenario:", paper$scenario, "\n")
    }

    # Try different extraction quality levels
    for (extraction_scenario in c("perfect", "realistic", "degraded")) {
      extracted <- extract_mock_claims(paper, scenario = extraction_scenario)
      eval_result <- evaluate_single_paper(paper, extracted, verbose = FALSE)
      eval_result$extraction_scenario <- extraction_scenario
      all_results[[length(all_results) + 1]] <- eval_result
    }
  }

  return(all_results)
}

# ============================================================================
# PART 5: SUMMARY REPORT GENERATOR
# ============================================================================

generate_batch_report <- function(all_results) {
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║           BATCH TEST SUMMARY REPORT                        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Create summary table
  summary_df <- data.frame(
    Paper = sapply(all_results, \(x) x$paper_title),
    Scenario = sapply(all_results, \(x) x$extraction_scenario),
    F1 = round(sapply(all_results, \(x) x$f1), 3),
    Precision = round(sapply(all_results, \(x) x$precision), 3),
    Recall = round(sapply(all_results, \(x) x$recall), 3),
    Calibration = round(sapply(all_results, \(x) x$high_confidence_calibration), 3),
    Hallucinations = round(sapply(all_results, \(x) x$hallucination_rate), 3),
    Composite = round(sapply(all_results, \(x) x$composite_score), 3),
    Status = ifelse(sapply(all_results, \(x) x$ready_for_production), "✓", "⚠"),
    stringsAsFactors = FALSE
  )

  print(summary_df)

  # Aggregated stats
  cat("\n--- AGGREGATED STATISTICS ---\n")
  cat("Mean F1-score:             ", round(mean(sapply(all_results, \(x) x$f1)), 3), "\n")
  cat("Mean Precision:            ", round(mean(sapply(all_results, \(x) x$precision)), 3), "\n")
  cat("Mean Recall:               ", round(mean(sapply(all_results, \(x) x$recall)), 3), "\n")
  cat("Mean Composite Score:      ", round(mean(sapply(all_results, \(x) x$composite_score)), 3), "\n")
  cat("Papers Ready for Production:", sum(sapply(all_results, \(x) x$ready_for_production)), "/", length(all_results), "\n")

  # Failure analysis
  failures <- all_results[!sapply(all_results, \(x) x$ready_for_production)]
  if (length(failures) > 0) {
    cat("\n--- PAPERS NEEDING REFINEMENT ---\n")
    for (f in failures) {
      cat("\n", f$paper_title, " (", f$extraction_scenario, " extraction)\n")
      cat("  Composite score:", round(f$composite_score, 3), "\n")
      if (f$f1 < 0.7) cat("  ⚠ F1-score too low:", round(f$f1, 3), "\n")
      if (f$hallucination_rate > 0.2) cat("  ⚠ Too many hallucinations:", round(f$hallucination_rate, 3), "\n")
      if (f$high_confidence_calibration < 0.7) cat("  ⚠ Poor confidence calibration:", round(f$high_confidence_calibration, 3), "\n")
    }
  }

  return(summary_df)
}

# ============================================================================
# RUN THE FULL TEST
# ============================================================================

if (!interactive()) {
  cat("Running batch pipeline test...\n")
  results <- run_batch_test(verbose = TRUE)
  summary <- generate_batch_report(results)

  cat("\n✓ Batch test complete. Results ready for analysis.\n")
} else {
  cat("Script loaded. Run: results <- run_batch_test(); summary <- generate_batch_report(results)\n")
}
