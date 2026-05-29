# ============================================================
# CART Regression on Carseats Dataset
# Goal: Predict Sales using all other variables
# ============================================================

# Load required packages
library(ISLR)       # Contains Carseats dataset
library(caret)      # For model training with CV
library(rpart)      # CART algorithm
library(rpart.plot) # For visualizing trees

# ------------------------------------------------------------
# Step 1: Load and inspect the data
# ------------------------------------------------------------
data(Carseats)
str(Carseats)
summary(Carseats)

# Quick look at the target variable
hist(Carseats$Sales, main = "Distribution of Sales", xlab = "Sales (thousands)")

# ------------------------------------------------------------
# Step 2: Train/Test Split (80/20)
# ------------------------------------------------------------
set.seed(42)  # For reproducibility — always set this!

# Create indices for training set
train_index <- createDataPartition(Carseats$Sales, p = 0.8, list = FALSE)

# Split the data
train_data <- Carseats[train_index, ]
test_data  <- Carseats[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# ------------------------------------------------------------
# Step 3: Set up 10-fold Cross-Validation
# ------------------------------------------------------------
# trainControl specifies HOW we evaluate models during training
cv_control <- trainControl(
  method = "cv",      # Cross-validation
  number = 10,        # 10 folds
  verboseIter = TRUE  # Show progress
)

# ------------------------------------------------------------
# Step 4: Define the tuning grid for complexity parameter (cp)
# ------------------------------------------------------------
# cp controls tree complexity:
#   - Higher cp = simpler tree (more pruning)
#   - Lower cp = more complex tree (less pruning)
# We'll try a range of values to find the sweet spot

cp_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))

# ------------------------------------------------------------
# Step 5: Train the CART model with CV tuning
# ------------------------------------------------------------
set.seed(42)

cart_model <- train(
  Sales ~ .,              # Predict Sales using all other variables
  data = train_data,
  method = "rpart",       # CART algorithm
  trControl = cv_control, # Use our 10-fold CV setup
  tuneGrid = cp_grid,     # Try these cp values
  metric = "RMSE"         # Optimize for RMSE
)

# ------------------------------------------------------------
# Step 6: Examine the results
# ------------------------------------------------------------

# Print summary
print(cart_model)

# Best tuning parameter
cat("\n=== BEST TUNING PARAMETER ===\n")
cat("Optimal cp:", cart_model$bestTune$cp, "\n")

# Cross-validated RMSE (at optimal cp)
best_result <- cart_model$results[cart_model$results$cp == cart_model$bestTune$cp, ]
cat("\n=== CROSS-VALIDATED PERFORMANCE ===\n")
cat("CV RMSE:", round(best_result$RMSE, 4), "\n")
cat("CV R-squared:", round(best_result$Rsquared, 4), "\n")
cat("CV MAE:", round(best_result$MAE, 4), "\n")

# ------------------------------------------------------------
# Step 7: Evaluate on TEST set (held-out data)
# ------------------------------------------------------------
test_predictions <- predict(cart_model, newdata = test_data)

# Calculate test RMSE
test_rmse <- sqrt(mean((test_data$Sales - test_predictions)^2))
test_mae  <- mean(abs(test_data$Sales - test_predictions))
test_r2   <- cor(test_data$Sales, test_predictions)^2

cat("\n=== TEST SET PERFORMANCE ===\n")
cat("Test RMSE:", round(test_rmse, 4), "\n")
cat("Test R-squared:", round(test_r2, 4), "\n")
cat("Test MAE:", round(test_mae, 4), "\n")

# ------------------------------------------------------------
# Step 8: PLOTS
# ------------------------------------------------------------

# Plot 1: Cross-validated error vs. complexity parameter
plot(cart_model, main = "CV RMSE vs. Complexity Parameter (cp)")

# Plot 2: The fitted tree
# Extract the final model and plot it
rpart.plot(
  cart_model$finalModel,
  main = "CART Regression Tree for Carseats Sales",
  extra = 101,  # Show n and percentage
  under = TRUE,
  faclen = 0    # Don't abbreviate factor names
)

# Plot 3: Variable importance
var_imp <- varImp(cart_model)
plot(var_imp, main = "Variable Importance")

# Plot 4: Predicted vs. Actual (test set)
plot(test_data$Sales, test_predictions,
     xlab = "Actual Sales", ylab = "Predicted Sales",
     main = "Test Set: Predicted vs. Actual",
     pch = 19, col = rgb(0, 0, 0, 0.5))
abline(0, 1, col = "red", lwd = 2)  # Perfect prediction line

# ------------------------------------------------------------
# Step 9: Examine the tree structure
# ------------------------------------------------------------