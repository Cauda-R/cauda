library(caret)
library(randomForest)
library(gbm)
library(kernlab)
library(rpart)

auto_model <- function(data, target, test_split = 0.8, cv_folds = 10, include_plots = TRUE) {
  
  set.seed(42)
  formula <- as.formula(paste(target, "~ ."))
  
  train_index <- createDataPartition(data[[target]], p = test_split, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  cat("Dataset size:", nrow(data), "rows\n")
  cat("Training set:", nrow(train_data), "rows\n")
  cat("Test set:", nrow(test_data), "rows\n\n")
  
  cv_control <- trainControl(
    method = "cv",
    number = cv_folds,
    verboseIter = TRUE
  )
  
  results <- list()
  
  cat("=== Training Linear Regression ===\n")
  set.seed(42)
  results$lm <- train(formula, data = train_data, method = "lm", trControl = cv_control)
  
  cat("\n=== Training CART ===\n")
  set.seed(42)
  results$cart <- train(
    formula, 
    data = train_data, 
    method = "rpart", 
    trControl = cv_control,
    tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.01))
  )
  
  cat("\n=== Training Random Forest ===\n")
  set.seed(42)
  num_predictors <- ncol(train_data) - 1
  mtry_values <- unique(c(1, floor(sqrt(num_predictors)), floor(num_predictors/2), num_predictors))
  mtry_values <- mtry_values[mtry_values >= 1 & mtry_values <= num_predictors]
  
  results$rf <- train(
    formula, 
    data = train_data, 
    method = "rf", 
    trControl = cv_control,
    tuneGrid = expand.grid(mtry = mtry_values),
    ntree = 500
  )
  
  cat("\n=== Training GBM ===\n")
  set.seed(42)
  results$gbm <- train(
    formula, 
    data = train_data, 
    method = "gbm", 
    trControl = cv_control,
    tuneGrid = expand.grid(
      n.trees = c(100, 200),
      interaction.depth = c(1, 2, 3),
      shrinkage = 0.1,
      n.minobsinnode = 10
    ),
    verbose = FALSE
  )
  
  cat("\n=== Training SVM ===\n")
  set.seed(42)
  results$svm <- train(
    formula, 
    data = train_data, 
    method = "svmRadial", 
    trControl = cv_control,
    preProcess = c("center", "scale"),
    tuneLength = 5
  )
  
  model_names <- c("Linear Regression", "CART", "Random Forest", "GBM", "SVM")
  model_keys <- c("lm", "cart", "rf", "gbm", "svm")
  
  cv_rmse <- sapply(model_keys, function(k) min(results[[k]]$results$RMSE))
  cv_rmse_sd <- sapply(model_keys, function(k) {
    results[[k]]$results$RMSESD[which.min(results[[k]]$results$RMSE)]
  })
  
  cv_comparison <- data.frame(
    Model = model_names,
    CV_RMSE = round(cv_rmse, 4),
    CV_RMSE_SD = round(cv_rmse_sd, 4)
  )
  cv_comparison <- cv_comparison[order(cv_comparison$CV_RMSE), ]
  
  test_rmse <- sapply(model_keys, function(k) {
    preds <- predict(results[[k]], newdata = test_data)
    sqrt(mean((test_data[[target]] - preds)^2))
  })
  
  test_comparison <- data.frame(
    Model = model_names,
    Test_RMSE = round(test_rmse, 4)
  )
  test_comparison <- test_comparison[order(test_comparison$Test_RMSE), ]
  
  cat("\n==========================================\n")
  cat("CROSS-VALIDATION RESULTS (sorted by RMSE)\n")
  cat("==========================================\n")
  print(cv_comparison, row.names = FALSE)
  
  cat("\n==========================================\n")
  cat("TEST SET RESULTS (sorted by RMSE)\n")
  cat("==========================================\n")
  print(test_comparison, row.names = FALSE)
  
  cat("\n==========================================\n")
  cat("SUMMARY\n")
  cat("==========================================\n")
  cat("Best model (CV):", cv_comparison$Model[1], "with RMSE:", cv_comparison$CV_RMSE[1], "\n")
  cat("Best model (Test):", test_comparison$Model[1], "with RMSE:", test_comparison$Test_RMSE[1], "\n")
  cat("Most stable model:", cv_comparison$Model[which.min(cv_comparison$CV_RMSE_SD)], 
      "with SD:", min(cv_comparison$CV_RMSE_SD), "\n")
  
  if (include_plots) {
    resamps <- resamples(results)
    print(bwplot(resamps, metric = "RMSE", main = "Model Comparison: CV RMSE"))
  }
  
  output <- list(
    cv_results = cv_comparison,
    test_results = test_comparison,
    models = results,
    train_data = train_data,
    test_data = test_data
  )
  
  return(invisible(output))
}