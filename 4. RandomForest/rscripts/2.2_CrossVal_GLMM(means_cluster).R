# Load necessary libraries
library(lme4)      # For GLMM
library(caret)     # For data partitioning
library(boot)      # For calculating R squared
library(ggplot2)   # For plotting
library(ggpubr)

# Set a random seed for reproducibility
set.seed(123)

# Select predictors and response for glmm
predictors_glmm <- c("TMPavg", "PREavg")
response <- "COOP2"
all_vars_glmm <- c(response, predictors_glmm, "Cluster")

# Remove NA values
analysis_data_glm <- reduced_data[, all_vars_glmm]
analysis_data_glm <- na.omit(analysis_data_glm)

# Function to calculate R squared for GLMM
calc_r_squared <- function(model, data, response_var) {
  # Predict on the provided data
  predictions <- predict(model, newdata = data, re.form = NULL, type = "response", allow.new.levels = TRUE)
  actuals <- data[[response_var]]
  
  # Calculate R squared
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(r_squared)
}

# Function to perform one split, train and evaluate the GLMM
run_single_split_glmm <- function(data) {
  # Create stratified split
  index <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
  train_data <- data[index, ]
  valid_data <- data[-index, ]
  
  # Ensure all clusters are in training data
  while (length(unique(train_data$Cluster)) < length(unique(data$Cluster))) {
    index <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
    train_data <- data[index, ]
    valid_data <- data[-index, ]
  }
  
  # Train GLMM model
  glmm_model <- glmer(
    formula = COOP2 ~ TMPavg + sqrt(PREavg) + (1 | Cluster),
    data = train_data,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  # Calculate R squared on training data
  r_squared_train <- calc_r_squared(glmm_model, train_data, response)
  
  # Calculate R squared on validation data
  r_squared_val <- calc_r_squared(glmm_model, valid_data, response)
  
  return(c(r_squared_train, r_squared_val))
}

# Number of repetitions
n_iterations <- 1000
results_glmm <- replicate(n_iterations, run_single_split_glmm(analysis_data_glm))

# Convert results to a data frame
results_df_glmm <- as.data.frame(t(results_glmm))
colnames(results_df_glmm) <- c("Training R^2", "Validation R^2")

# Display results
head(results_df_glmm)

# Plotting results
a <- ggplot(results_df_glmm, aes(x = `Training R^2`, y = `Validation R^2`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Training vs. Validation R^2 (GLMM)",
       x = expression(paste("Training ", R^2)),
       y = expression(paste("Validation ", R^2))) +
  theme_minimal() +
  xlim(0, 1) + ylim(0, 1)

# Histogram of Validation R^2
b <- ggplot(results_df_glmm, aes(x = `Validation R^2`)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = expression(paste("Distribution of Validation ", R^2)),
       x = expression(paste("Validation ", R^2)),
       y = "Frequency") +
  theme_minimal() +
  xlim(0, 0.4)

# Arrange plots side by side
ggarrange(a, b, nrow = 1)

fwrite(results_df_glmm,"CrossVal_GLMM.csv")
