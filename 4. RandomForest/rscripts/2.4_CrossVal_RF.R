# Load necessary libraries
library(data.table)
library(ranger)
library(caret)
library(ggplot2)
library(ggpubr)

# Set a random seed for reproducibility
set.seed(123)
# c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym", "Cluster")
# print(tuned_params$recommended.pars)
# mtry min.node.size sample.fraction       auc exec.time
# 1    1            56       0.8962444 0.8011724 0.1231429
# > c(mean(results_df$`Validation R^2`), median(results_df$`Validation R^2`), sd(results_df$`Validation R^2`))
# [1] 0.15984243 0.15067987 0.07928453
# c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym")
# > print(tuned_params$recommended.pars)
# mtry min.node.size sample.fraction       auc exec.time
# 1    1            57       0.8691356 0.7864717     0.123
# > c(mean(results_df$`Validation R^2`), median(results_df$`Validation R^2`), sd(results_df$`Validation R^2`))
# [1] 0.14551812 0.13738846 0.07493506
# 
# Parameters from tuning results
mtry_optimal <- 1
min_node_size_optimal <- 56
sample_fraction_optimal <- 0.8962444

# Select predictors and response
predictors <- c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", 
                "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym", "Cluster")
response <- "COOP2"
all_vars <- c(response, predictors)

# Remove NA values
analysis_data <- reduced_data[, all_vars]
analysis_data <- na.omit(analysis_data)

# Define function to perform one split, train and evaluate
run_single_split <- function(data) {
  # Create stratified split
  index <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
  train_data <- data[index, ]
  valid_data <- data[-index, ]
  
  # Train random forest model
  rf_model <- ranger(
    COOP2 ~ ., 
    data = train_data, 
    num.trees = 2000,
    mtry = mtry_optimal,  # Optimal number of features at each split
    min.node.size = min_node_size_optimal,  # Optimal minimum node size
    sample.fraction = sample_fraction_optimal,  # Optimal sample fraction
    seed = 123
  )
  
  # Predict and evaluate on training data
  train_pred <- predict(rf_model, data = train_data)$predictions
  r_squared_train <- cor(train_pred, train_data[[response]])^2
  
  # Predict and evaluate on validation data
  valid_pred <- predict(rf_model, data = valid_data)$predictions
  r_squared_val <- cor(valid_pred, valid_data[[response]])^2
  
  return(c(r_squared_train, r_squared_val))
}

# Number of repetitions
n_iterations <- 1000
results <- replicate(n_iterations, run_single_split(analysis_data))

# Convert results to a data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- c("Training R^2", "Validation R^2")

# Plotting results
a<- ggplot(results_df, aes(x = `Training R^2`, y = `Validation R^2`)) +
  geom_point(alpha = 0.25, stroke= 0) +
  #geom_smooth(method = "lm", color = "red") +
  labs(title = expression(paste("Training vs. Validation ", R^2," (RF means+cluster)")),
       x = expression(paste("Training ", R^2)),
       y = expression(paste("Validation ", R^2)),) +
  theme_minimal() +
  xlim(0, 1) + 
  ylim(0, 1) 

# Histogram of Validation R^2
b<- ggplot(results_df, aes(x = `Validation R^2`)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = expression(paste("Distribution of Validation ", R^2)),
       x = expression(paste("Validation ", R^2)),
       y = "Frequency") +
  theme_minimal() +
  xlim(0.0, 0.4) 

ggarrange(a,b,nrow=1)

c(mean(results_df$`Validation R^2`), median(results_df$`Validation R^2`), sd(results_df$`Validation R^2`))

fwrite(results_df,"CrossVal_RF.csv")
