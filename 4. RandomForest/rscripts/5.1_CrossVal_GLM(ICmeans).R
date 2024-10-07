# Load necessary libraries
library(data.table)
library(caret)
library(ggplot2)
library(ggpubr)

# Set a random seed for reproducibility
set.seed(123)

# Select predictors and response for glm
predictors_glm <- c("IC_TMPavg", "IC_PREavg")
response <- "IC_COOP2"
all_vars_glm <- c(response, predictors_glm)

# Remove NA values
analysis_data_glm <- contrast_data[, all_vars_glm]
analysis_data_glm <- na.omit(analysis_data_glm)

# Define function to perform one split, train, and evaluate for glm
run_single_split_glm <- function(data) {
  # Create stratified split
  index <- createDataPartition(data[[response]], p = 0.8, list = FALSE)
  train_data <- data[index, ]
  valid_data <- data[-index, ]
  
  # Train GLM model
  glm_model <- glm(IC_COOP2 ~ IC_TMPavg + IC_PREavg, 
                   data = train_data, 
                   family = "gaussian")
  
  # Predict and evaluate on training data
  train_pred <- predict(glm_model, newdata = train_data, type = "response")
  r_squared_train <- cor(train_pred, train_data[[response]])^2
  
  # Predict and evaluate on validation data
  valid_pred <- predict(glm_model, newdata = valid_data, type = "response")
  r_squared_val <- cor(valid_pred, valid_data[[response]])^2
  
  return(c(r_squared_train, r_squared_val))
}

# Number of repetitions
n_iterations <- 1000
results_glm <- replicate(n_iterations, run_single_split_glm(analysis_data_glm))

# Convert results to a data frame
results_df_glm <- as.data.frame(t(results_glm))
colnames(results_df_glm) <- c("Training R^2", "Validation R^2")

# Display results
head(results_df_glm)

# Plotting results for GLM
a_glm <- ggplot(results_df_glm, aes(x = `Training R^2`, y = `Validation R^2`)) +
  geom_point(alpha = 0.25, stroke= 0) +
  labs(title = expression(paste("Training vs. Validation ", R^2," (GLM)")),
       x = expression(paste("Training ", R^2)),
       y = expression(paste("Validation ", R^2))) +
  theme_minimal() +
  xlim(0, 1) + 
  ylim(0, 1) 

# Histogram of Validation R^2
b_glm <- ggplot(results_df_glm, aes(x = `Validation R^2`)) +
  geom_histogram(binwidth = 0.01, fill = "skyblue", color = "black") +
  labs(title = expression(paste("Distribution of Validation ", R^2)),
       x = expression(paste("Validation ", R^2)),
       y = "Frequency") +
  theme_minimal() +
  xlim(0.0, 0.4) 

# Arrange plots side by side
ggarrange(a_glm, b_glm, nrow = 1)

c(median(results_df_glm$`Validation R^2`))
c(median(results_df_glmm$`Validation R^2`))

fwrite(results_df_glm,"CrossVal_GLM_IC.csv")
