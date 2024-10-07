library(tuneRanger)
library(ranger)
library(data.table)
library(caret)

# Prepare data: Define predictors and response variable
predictors <- c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", 
  "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym", "Cluster")
# c("TMPavg", "PREavg")  # Update with relevant features
response <- "COOP2"
all_vars <- c(response, predictors)

# Subset and clean data
analysis_data <- reduced_data[, all_vars]
analysis_data <- na.omit(analysis_data)

# Set up the tuning task
# Convert to data frame for tuneRanger
analysis_data_df <- as.data.frame(analysis_data)

# Set up the tuning task
tuning_task <- makeClassifTask(data = analysis_data_df, target = response)

# Define the resampling strategy (e.g., 5-fold cross-validation)
# mlr package's way of resampling
resampling_strategy <- makeResampleDesc("CV", iters = 5)

# Run the tuning process
# Note: No 'resampling' and 'verbose' arguments
tuned_params <- tuneRanger(
  task = tuning_task,
  measure = list(auc),
  num.trees = 2000,  # Number of trees
  iters = 100,       # Number of iterations for Bayesian optimization
  tune.parameters = c("mtry", "min.node.size", "sample.fraction"),
  save.file.path = NULL,  # Optional: specify a path to save tuning results
  num.threads = 1,        # Number of threads to use
  build.final.model = TRUE # Build the final model with the best hyperparameters
)

# Print the best hyperparameters
print(tuned_params$recommended.pars)
