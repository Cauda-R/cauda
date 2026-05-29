# =============================================================================
# auto_model.R
# A library-grade automated model comparison tool for regression tasks.
#
# Improvements over v1:
#   - seed=       : explicit reproducibility control (or NULL to disable)
#   - na_action=  : clean missing value handling (fail / omit / impute)
#   - preprocess= : consistent scaling policy across models
#   - S3 class    : print() and plot() methods for structured output
# =============================================================================

library(caret)
library(randomForest)
library(gbm)
library(kernlab)
library(rpart)

# =============================================================================
# MAIN FUNCTION
# =============================================================================

auto_model <- function(
    data,
    target,
    test_split    = 0.8,
    cv_folds      = 10,
    include_plots = TRUE,
    seed          = 42,
    na_action     = "fail",
    preprocess    = "auto"
) {
  # ---------------------------------------------------------------------------
  # INPUT VALIDATION
  # ---------------------------------------------------------------------------
  if (!is.data.frame(data))
    stop("'data' must be a data frame.")
  if (!target %in% names(data))
    stop(paste0("Target column '", target, "' not found in data."))
  if (!na_action %in% c("fail", "omit", "impute"))
    stop("'na_action' must be one of: 'fail', 'omit', or 'impute'.")
  if (!preprocess %in% c("auto", "all", "none"))
    stop("'preprocess' must be one of: 'auto', 'all', or 'none'.")

  # ---------------------------------------------------------------------------
  # SEED — set once up front, then again before each model
  # Pass seed = NULL to disable reproducibility control entirely
  # ---------------------------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)

  # ---------------------------------------------------------------------------
  # MISSING VALUE HANDLING
  # "fail"   -> stop with an informative message (default -- safest)
  # "omit"   -> drop rows with any NA
  # "impute" -> fill NAs with column medians
  # ---------------------------------------------------------------------------
  na_count <- sum(is.na(data))
  if (na_count > 0) {
    if (na_action == "fail") {
      stop(paste0(
        "Data contains ", na_count, " missing value(s). ",
        "Set na_action = 'omit' to remove affected rows, ",
        "or na_action = 'impute' to fill with column medians."
      ))
    } else if (na_action == "omit") {
      original_n <- nrow(data)
      data <- na.omit(data)
      cat("Note: Removed", original_n - nrow(data),
          "row(s) with missing values.", nrow(data), "rows remaining.\n\n")
    } else if (na_action == "impute") {
      cat("Note: Imputing", na_count, "missing value(s) with column medians.\n\n")
      for (col in names(data)) {
        if (any(is.na(data[[col]]))) {
          data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # PREPROCESSING HELPER
  # "auto"  -> scale only SVM (the model that actually needs it)
  # "all"   -> scale every model
  # "none"  -> no scaling anywhere
  # ---------------------------------------------------------------------------
  get_preproc <- function(model_needs_scaling) {
    if (preprocess == "all")                          return(c("center", "scale"))
    if (preprocess == "none")                         return(NULL)
    if (preprocess == "auto" && model_needs_scaling)  return(c("center", "scale"))
    return(NULL)
  }

  # ---------------------------------------------------------------------------
  # TRAIN / TEST SPLIT
  # ---------------------------------------------------------------------------
  formula     <- as.formula(paste(target, "~ ."))
  train_index <- createDataPartition(data[[target]], p = test_split, list = FALSE)
  train_data  <- data[train_index, ]
  test_data   <- data[-train_index, ]

  cat("Dataset size:", nrow(data),       "rows\n")
  cat("Training set:", nrow(train_data), "rows\n")
  cat("Test set:    ", nrow(test_data),  "rows\n\n")

  # ---------------------------------------------------------------------------
  # CROSS-VALIDATION CONTROL
  # ---------------------------------------------------------------------------
  cv_control <- trainControl(
    method      = "cv",
    number      = cv_folds,
    verboseIter = FALSE
  )

  results <- list()

  # ---------------------------------------------------------------------------
  # MODEL TRAINING
  # ---------------------------------------------------------------------------

  cat("=== Training Linear Regression ===\n")
  if (!is.null(seed)) set.seed(seed)
  results$lm <- train(
    formula, data = train_data, method = "lm",
    trControl  = cv_control,
    preProcess = get_preproc(FALSE)
  )

  cat("\n=== Training CART ===\n")
  if (!is.null(seed)) set.seed(seed)
  results$cart <- train(
    formula, data = train_data, method = "rpart",
    trControl  = cv_control,
    preProcess = get_preproc(FALSE),
    tuneGrid   = expand.grid(cp = seq(0.001, 0.1, by = 0.01))
  )

  cat("\n=== Training Random Forest ===\n")
  if (!is.null(seed)) set.seed(seed)
  num_predictors <- ncol(train_data) - 1
  mtry_values    <- unique(c(1, floor(sqrt(num_predictors)),
                             floor(num_predictors / 2), num_predictors))
  mtry_values    <- mtry_values[mtry_values >= 1 & mtry_values <= num_predictors]

  results$rf <- train(
    formula, data = train_data, method = "rf",
    trControl  = cv_control,
    preProcess = get_preproc(FALSE),
    tuneGrid   = expand.grid(mtry = mtry_values),
    ntree      = 500
  )

  cat("\n=== Training GBM ===\n")
  if (!is.null(seed)) set.seed(seed)
  results$gbm <- train(
    formula, data = train_data, method = "gbm",
    trControl  = cv_control,
    preProcess = get_preproc(FALSE),
    tuneGrid   = expand.grid(
      n.trees           = c(100, 200),
      interaction.depth = c(1, 2, 3),
      shrinkage         = 0.1,
      n.minobsinnode    = max(2, floor(nrow(train_data) * 0.05))
    ),
    verbose = FALSE
  )

  cat("\n=== Training SVM ===\n")
  if (!is.null(seed)) set.seed(seed)
  results$svm <- train(
    formula, data = train_data, method = "svmRadial",
    trControl  = cv_control,
    preProcess = get_preproc(TRUE),
    tuneLength = 5
  )

  # ---------------------------------------------------------------------------
  # BUILD RESULTS TABLES
  # ---------------------------------------------------------------------------
  model_names <- c("Linear Regression", "CART", "Random Forest", "GBM", "SVM")
  model_keys  <- c("lm", "cart", "rf", "gbm", "svm")

  cv_rmse    <- sapply(model_keys, function(k) min(results[[k]]$results$RMSE))
  cv_rmse_sd <- sapply(model_keys, function(k) {
    results[[k]]$results$RMSESD[which.min(results[[k]]$results$RMSE)]
  })

  cv_comparison <- data.frame(
    Model      = model_names,
    CV_RMSE    = round(cv_rmse, 4),
    CV_RMSE_SD = round(cv_rmse_sd, 4)
  )
  cv_comparison <- cv_comparison[order(cv_comparison$CV_RMSE), ]

  test_rmse <- sapply(model_keys, function(k) {
    preds <- predict(results[[k]], newdata = test_data)
    sqrt(mean((test_data[[target]] - preds)^2))
  })

  test_comparison <- data.frame(
    Model     = model_names,
    Test_RMSE = round(test_rmse, 4)
  )
  test_comparison <- test_comparison[order(test_comparison$Test_RMSE), ]

  # ---------------------------------------------------------------------------
  # ASSEMBLE OUTPUT OBJECT — registered as S3 class "auto_model"
  # This separates the data from the display, so print() and plot() can be
  # called independently at any time after the fact.
  # ---------------------------------------------------------------------------
  output <- structure(
    list(
      cv_results   = cv_comparison,
      test_results = test_comparison,
      models       = results,
      train_data   = train_data,
      test_data    = test_data,
      call = list(
        target     = target,
        test_split = test_split,
        cv_folds   = cv_folds,
        seed       = seed,
        na_action  = na_action,
        preprocess = preprocess
      )
    ),
    class = "auto_model"
  )

  # Delegate all printing and plotting to the S3 methods below
  print(output)
  if (include_plots) plot(output)

  return(invisible(output))
}

# =============================================================================
# S3 PRINT METHOD
# Called automatically when you type `result` in the console,
# or explicitly with print(result)
# =============================================================================

print.auto_model <- function(x, ...) {
  cat("\n==========================================\n")
  cat("CROSS-VALIDATION RESULTS (sorted by RMSE)\n")
  cat("==========================================\n")
  print(x$cv_results, row.names = FALSE)

  cat("\n==========================================\n")
  cat("TEST SET RESULTS (sorted by RMSE)\n")
  cat("==========================================\n")
  print(x$test_results, row.names = FALSE)

  cat("\n==========================================\n")
  cat("SUMMARY\n")
  cat("==========================================\n")
  cat("Best model (CV):   ", x$cv_results$Model[1],
      "-- RMSE:", x$cv_results$CV_RMSE[1], "\n")
  cat("Best model (Test): ", x$test_results$Model[1],
      "-- RMSE:", x$test_results$Test_RMSE[1], "\n")
  cat("Most stable model: ",
      x$cv_results$Model[which.min(x$cv_results$CV_RMSE_SD)],
      "-- SD:", min(x$cv_results$CV_RMSE_SD), "\n")
  cat("\nSettings used: seed =", x$call$seed,
      "| na_action =", x$call$na_action,
      "| preprocess =", x$call$preprocess, "\n")

  invisible(x)
}

# =============================================================================
# S3 PLOT METHOD
# Called automatically with plot(result)
# Shows a boxplot of CV RMSE distributions across all 5 models
# =============================================================================

plot.auto_model <- function(x, ...) {
  resamps <- resamples(x$models)
  print(bwplot(resamps, metric = "RMSE", main = "Model Comparison: CV RMSE"))
  invisible(x)
}
