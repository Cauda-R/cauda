# Model Comparison on Carseats Dataset

library(ISLR)
library(caret)
library(glmnet)
library(randomForest)
library(kernlab)

# Load data and split
data(Carseats)

set.seed(42)
train_index <- createDataPartition(Carseats$Sales, p = 0.8, list = FALSE)
train_data <- Carseats[train_index, ]
test_data  <- Carseats[-train_index, ]

# Cross-validation setup (same for all models)
cv_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

results <- list()

# Model 1: Linear Regression
cat("\n=== Training Linear Regression ===\n")
set.seed(42)
lm_model <- train(
  Sales ~ .,
  data = train_data,
  method = "lm",
  trControl = cv_control
)
results$lm <- lm_model

# Model 2: Ridge Regression
cat("\n=== Training Ridge Regression ===\n")
set.seed(42)
ridge_grid <- expand.grid(
  alpha = 0,
  lambda = 10^seq(-4, 1, length = 50)
)
ridge_model <- train(
  Sales ~ .,
  data = train_data,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = ridge_grid,
  preProcess = c("center", "scale")
)
results$ridge <- ridge_model

# Model 3: Lasso Regression
cat("\n=== Training Lasso Regression ===\n")
set.seed(42)
lasso_grid <- expand.grid(
  alpha = 1,
  lambda = 10^seq(-4, 1, length = 50)
)
lasso_model <- train(
  Sales ~ .,
  data = train_data,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = lasso_grid,
  preProcess = c("center", "scale")
)
results$lasso <- lasso_model

# Model 4: CART
cat("\n=== Training CART ===\n")
set.seed(42)
cart_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))
cart_model <- train(
  Sales ~ .,
  data = train_data,
  method = "rpart",
  trControl = cv_control,
  tuneGrid = cart_grid
)
results$cart <- cart_model

# Model 5: Random Forest
cat("\n=== Training Random Forest ===\n")
set.seed(42)
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
rf_model <- train(
  Sales ~ .,
  data = train_data,
  method = "rf",
  trControl = cv_control,
  tuneGrid = rf_grid,
  ntree = 500
)
results$rf <- rf_model

# Model 6: SVM
cat("\n=== Training SVM ===\n")
set.seed(42)
svm_model <- train(
  Sales ~ .,
  data = train_data,
  method = "svmRadial",
  trControl = cv_control,
  preProcess = c("center", "scale"),
  tuneLength = 10
)
results$svm <- svm_model

# Build comparison table
comparison <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso", "CART", "Random Forest", "SVM"),
  CV_RMSE = c(
    min(results$lm$results$RMSE),
    min(results$ridge$results$RMSE),
    min(results$lasso$results$RMSE),
    min(results$cart$results$RMSE),
    min(results$rf$results$RMSE),
    min(results$svm$results$RMSE)
  ),
  CV_RMSE_SD = c(
    results$lm$results$RMSESD[which.min(results$lm$results$RMSE)],
    results$ridge$results$RMSESD[which.min(results$ridge$results$RMSE)],
    results$lasso$results$RMSESD[which.min(results$lasso$results$RMSE)],
    results$cart$results$RMSESD[which.min(results$cart$results$RMSE)],
    results$rf$results$RMSESD[which.min(results$rf$results$RMSE)],
    results$svm$results$RMSESD[which.min(results$svm$results$RMSE)]
  )
)

comparison <- comparison[order(comparison$CV_RMSE), ]
comparison$CV_RMSE <- round(comparison$CV_RMSE, 4)
comparison$CV_RMSE_SD <- round(comparison$CV_RMSE_SD, 4)

cat("\n\n========================================\n")
cat("CV RMSE COMPARISON (sorted best to worst)\n")
cat("========================================\n\n")
print(comparison, row.names = FALSE)

# Best tuning parameters
cat("\n=== BEST TUNING PARAMETERS ===\n")
cat("Ridge lambda:", results$ridge$bestTune$lambda, "\n")
cat("Lasso lambda:", results$lasso$bestTune$lambda, "\n")
cat("CART cp:", results$cart$bestTune$cp, "\n")
cat("Random Forest mtry:", results$rf$bestTune$mtry, "\n")
cat("SVM sigma:", results$svm$bestTune$sigma, "| C:", results$svm$bestTune$C, "\n")

# Test set evaluation
test_rmse <- function(model, test_data) {
  preds <- predict(model, newdata = test_data)
  sqrt(mean((test_data$Sales - preds)^2))
}

test_results <- data.frame(
  Model = c("Linear Regression", "Ridge", "Lasso", "CART", "Random Forest", "SVM"),
  Test_RMSE = c(
    test_rmse(results$lm, test_data),
    test_rmse(results$ridge, test_data),
    test_rmse(results$lasso, test_data),
    test_rmse(results$cart, test_data),
    test_rmse(results$rf, test_data),
    test_rmse(results$svm, test_data)
  )
)

test_results <- test_results[order(test_results$Test_RMSE), ]
test_results$Test_RMSE <- round(test_results$Test_RMSE, 4)

cat("\n=== TEST SET RMSE (sorted best to worst) ===\n")
print(test_results, row.names = FALSE)

# Visual comparison
resamps <- resamples(results)
bwplot(resamps, metric = "RMSE", main = "Model Comparison: CV RMSE")