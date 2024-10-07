library(tuneRanger)
library(ranger)
library(data.table)
library(caret)

contrast_data <- fread("Bird_Clm_IC_04Sep.csv")
contrast_data <- as.data.frame(contrast_data)
# Set up the tuning task
response <- "IC_COOP2"
tuning_task <- makeRegrTask(data = contrast_data, target = response)

# Define resampling strategy for cross-validation (CV) with 5 iterations
resampling_strategy <- makeResampleDesc("CV", iters = 5)

# Define the performance measure (use RMSE or MAE for regression)
performance_measure <- list(rmse)  # You can also use mae for Mean Absolute Error

# Run the tuning process
# Note: We no longer use 'auc' as this is for classification. We're using RMSE for regression.
tuned_params <- tuneRanger(
  task = tuning_task,
  measure = performance_measure,
  num.trees = 2000,  # Number of trees
  iters = 100,       # Number of iterations for Bayesian optimization
  tune.parameters = c("mtry", "min.node.size", "sample.fraction"),
  save.file.path = NULL,  # Optional: specify a path to save tuning results
  num.threads = 1,        # Number of threads to use
  build.final.model = TRUE # Build the final model with the best hyperparameters
)

# Print the best hyperparameters
print(tuned_params$recommended.pars)
# mtry min.node.size sample.fraction       rmse exec.time
# 1    1            56       0.8879883 0.06210978 0.1275714
