# Load necessary libraries
library(data.table)
library(ranger)
library(caret)
library(ggplot2)
library(ggpubr)

# Set a random seed for reproducibility
set.seed(123)

# Parameters from tuning results
mtry_optimal <- 1
min_node_size_optimal <- 56
sample_fraction_optimal <- 0.8879883

# Select predictors and response
predictors <- c("IC_TMPavg", "IC_TMPsd", "IC_TMPcv", "IC_TMPprd_Ent", "IC_TMPprd_Fix", "IC_TMPprd_Dym", 
                "IC_PREavg", "IC_PREsd", "IC_PREcv", "IC_PREprd_Ent", "IC_PREprd_Fix", "IC_PREprd_Dym")
response <- "IC_COOP2"
all_vars <- c(response, predictors)

# Remove NA values
analysis_data <- contrast_data[, all_vars]
analysis_data <- na.omit(analysis_data)

# Define function to perform one split, train and evaluate
run_single_split <- function(data) {
  # Create stratified split
  index <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
  train_data <- data[index, ]
  valid_data <- data[-index, ]
  
  # Train random forest model
  rf_model <- ranger(
    IC_COOP2 ~ ., 
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
  labs(title = expression(paste("Training vs. Validation ", R^2," (RF Independent Contrast)")),
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
fwrite(results_df,"CrossVal_RF_IC.csv")
