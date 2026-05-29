# Mall Customer Score Prediction
# Goal: Minimize 10-fold CV RMSE, compare models, try stacking

library(caret)
library(caretEnsemble)
library(randomForest)
library(gbm)
library(kernlab)
library(rpart)

# Load the data (UPDATE THIS PATH to where you saved Mall.csv)
mall <- read.csv("Mall.csv")

# Look at the data
str(mall)
summary(mall)
head(mall)

# Remove the first column (it's just row numbers)
mall <- mall[, -1]

# Check what we have now
str(mall)

# Quick visualization of Score distribution
hist(mall$Score, main = "Distribution of Spending Score", xlab = "Score")

# See if there are obvious patterns
plot(mall$Income, mall$Score, main = "Income vs Score", xlab = "Income", ylab = "Score")
plot(mall$Age, mall$Score, main = "Age vs Score", xlab = "Age", ylab = "Score")

# Train/test split
set.seed(42)
train_index <- createDataPartition(mall$Score, p = 0.8, list = FALSE)
train_data <- mall[train_index, ]
test_data <- mall[-train_index, ]

cat("Training set:", nrow(train_data), "rows\n")
cat("Test set:", nrow(test_data), "rows\n")

# ============================================================
# IMPORTANT: For caretEnsemble, we need a special CV setup
# ============================================================
set.seed(42)
cv_folds <- createFolds(train_data$Score, k = 10, returnTrain = TRUE)

cv_control <- trainControl(
  method = "cv",
  number = 10,
  index = cv_folds,
  savePredictions = "final"
)

# ============================================================
# Train individual models
# ============================================================
results <- list()

# Model 1: Linear Regression
cat("\n=== Training Linear Regression ===\n")
set.seed(42)
lm_model <- train(
  Score ~ .,
  data = train_data,
  method = "lm",
  trControl = cv_control
)
results$lm <- lm_model

# Model 2: CART
cat("\n=== Training CART ===\n")
set.seed(42)
cart_model <- train(
  Score ~ .,
  data = train_data,
  method = "rpart",
  trControl = cv_control,
  tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.005))
)
results$cart <- cart_model

# Model 3: Random Forest
cat("\n=== Training Random Forest ===\n")
set.seed(42)
rf_model <- train(
  Score ~ .,
  data = train_data,
  method = "rf",
  trControl = cv_control,
  tuneGrid = expand.grid(mtry = c(1, 2, 3)),
  ntree = 500
)
results$rf <- rf_model

# Model 4: Gradient Boosting (GBM)
cat("\n=== Training Gradient Boosting ===\n")
set.seed(42)
gbm_model <- train(
  Score ~ .,
  data = train_data,
  method = "gbm",
  trControl = cv_control,
  tuneGrid = expand.grid(
    n.trees = c(100, 200, 300),
    interaction.depth = c(1, 2, 3),
    shrinkage = 0.1,
    n.minobsinnode = 10
  ),
  verbose = FALSE
)
results$gbm <- gbm_model

# Model 5: SVM
cat("\n=== Training SVM ===\n")
set.seed(42)
svm_model <- train(
  Score ~ .,
  data = train_data,
  method = "svmRadial",
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
results$svm <- svm_model

# ============================================================
# Build comparison table for individual models
# ============================================================
comparison <- data.frame(
  Model = c("Linear Regression", "CART", "Random Forest", "GBM", "SVM"),
  CV_RMSE = c(
    min(results$lm$results$RMSE),
    min(results$cart$results$RMSE),
    min(results$rf$results$RMSE),
    min(results$gbm$results$RMSE),
    min(results$svm$results$RMSE)
  ),
  CV_RMSE_SD = c(
    results$lm$results$RMSESD[which.min(results$lm$results$RMSE)],
    results$cart$results$RMSESD[which.min(results$cart$results$RMSE)],
    results$rf$results$RMSESD[which.min(results$rf$results$RMSE)],
    results$gbm$results$RMSESD[which.min(results$gbm$results$RMSE)],
    results$svm$results$RMSESD[which.min(results$svm$results$RMSE)]
  )
)

comparison <- comparison[order(comparison$CV_RMSE), ]
comparison$CV_RMSE <- round(comparison$CV_RMSE, 4)
comparison$CV_RMSE_SD <- round(comparison$CV_RMSE_SD, 4)

cat("\n========================================\n")
cat("INDIVIDUAL MODEL COMPARISON (CV RMSE)\n")
cat("========================================\n")
print(comparison, row.names = FALSE)

# ============================================================
# STACKED ENSEMBLE using caretEnsemble
# ============================================================
cat("\n=== Building Stacked Ensemble ===\n")

# First, train a list of models using caretList
set.seed(42)
model_list <- caretList(
  Score ~ .,
  data = train_data,
  trControl = cv_control,
  methodList = c("lm", "rpart", "svmRadial"),
  tuneList = list(
    rf = caretModelSpec(method = "rf", tuneGrid = expand.grid(mtry = 2)),
    gbm = caretModelSpec(method = "gbm", 
                         tuneGrid = expand.grid(n.trees = 200, 
                                                interaction.depth = 2,
                                                shrinkage = 0.1,
                                                n.minobsinnode = 10),
                         verbose = FALSE)
  )
)

# Check correlation between model predictions
cat("\n=== Model Correlation ===\n")
print(modelCor(resamples(model_list)))

# Stack the models using a linear combination (greedy ensemble)
set.seed(42)
greedy_ensemble <- caretEnsemble(model_list)

cat("\n=== Ensemble Weights ===\n")
print(summary(greedy_ensemble))

# Stack using a more sophisticated meta-learner (glm)
set.seed(42)
stack_glm <- caretStack(
  model_list,
  method = "glm",
  trControl = trainControl(method = "cv", number = 10)
)

cat("\n=== Stacked Model (GLM meta-learner) ===\n")
print(stack_glm)

# ============================================================
# Test Set Evaluation
# ============================================================
cat("\n========================================\n")
cat("TEST SET EVALUATION\n")
cat("========================================\n")

test_rmse <- function(model, test_data) {
  preds <- predict(model, newdata = test_data)
  sqrt(mean((test_data$Score - preds)^2))
}

# Individual models
test_results <- data.frame(
  Model = c("Linear Regression", "CART", "Random Forest", "GBM", "SVM"),
  Test_RMSE = c(
    test_rmse(results$lm, test_data),
    test_rmse(results$cart, test_data),
    test_rmse(results$rf, test_data),
    test_rmse(results$gbm, test_data),
    test_rmse(results$svm, test_data)
  )
)

# Ensemble predictions
ensemble_preds_greedy <- predict(greedy_ensemble, newdata = test_data)
ensemble_preds_stack <- predict(stack_glm, newdata = test_data)

ensemble_rmse_greedy <- sqrt(mean((test_data$Score - ensemble_preds_greedy)^2))
ensemble_rmse_stack <- sqrt(mean((test_data$Score - ensemble_preds_stack)^2))

# Add ensembles to results
test_results <- rbind(test_results, 
                      data.frame(Model = "Greedy Ensemble", Test_RMSE = ensemble_rmse_greedy),
                      data.frame(Model = "Stacked Ensemble (GLM)", Test_RMSE = ensemble_rmse_stack))

test_results <- test_results[order(test_results$Test_RMSE), ]
test_results$Test_RMSE <- round(test_results$Test_RMSE, 4)

print(test_results, row.names = FALSE)

# ============================================================
# Visual Comparison
# ============================================================
resamps <- resamples(results)
bwplot(resamps, metric = "RMSE", main = "Individual Model Comparison: CV RMSE")

# ============================================================
# Summary Statistics
# ============================================================
cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")

best_single <- comparison$Model[1]
best_single_rmse <- comparison$CV_RMSE[1]

cat("Best single model:", best_single, "with CV RMSE:", best_single_rmse, "\n")
cat("Greedy Ensemble Test RMSE:", round(ensemble_rmse_greedy, 4), "\n")
cat("Stacked Ensemble Test RMSE:", round(ensemble_rmse_stack, 4), "\n")

# Did ensemble help?
best_single_test <- min(test_results$Test_RMSE[test_results$Model != "Greedy Ensemble" & 
                                                 test_results$Model != "Stacked Ensemble (GLM)"])
cat("\nBest single model Test RMSE:", best_single_test, "\n")
cat("Ensemble improvement:", round(best_single_test - min(ensemble_rmse_greedy, ensemble_rmse_stack), 4), "\n")