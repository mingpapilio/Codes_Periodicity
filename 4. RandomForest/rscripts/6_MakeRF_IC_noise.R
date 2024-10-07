# Load necessary libraries
library(data.table)
library(tuneRanger)
library(ranger)
library(caret)
library(ape)
library(phytools)
library(ranger)
library(shapviz)
library(kernelshap)
library(ggplot2)
library(ggpubr)
library(ggdendro)
theme_set(theme_minimal())

# 
set.seed(321)
contrast_data$NoiseFeature <- rnorm(nrow(contrast_data))

set.seed(123)
# Prepare data: Define predictors and response variable
predictors <-c("IC_TMPavg", "IC_TMPsd", "IC_TMPcv", "IC_TMPprd_Ent", "IC_TMPprd_Fix", "IC_TMPprd_Dym", 
               "IC_PREavg", "IC_PREsd", "IC_PREcv", "IC_PREprd_Ent", "IC_PREprd_Fix", "IC_PREprd_Dym", "NoiseFeature")
response <- "IC_COOP2"
all_vars <- c(response, predictors)

# Subset and clean data
analysis_data <- contrast_data[, all_vars]
analysis_data <- na.omit(analysis_data)

# Set up the tuning task
# Convert to data frame for tuneRanger
analysis_data_df <- as.data.frame(analysis_data)

# Set up the tuning task
tuning_task <- makeRegrTask(data = analysis_data_df, target = response)

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
# 1    1            56       0.8641245 0.06232182     0.124
mtry_optimal <- tuned_params$recommended.pars$mtry
min_node_size_optimal <- tuned_params$recommended.pars$min.node.size
sample_fraction_optimal <- tuned_params$recommended.pars$sample.fraction
#
rf_model<- ranger(
  IC_COOP2 ~ ., 
  data = analysis_data, 
  num.trees = 2000,
  mtry = mtry_optimal,  # Optimal number of features at each split
  min.node.size = min_node_size_optimal,  # Optimal minimum node size
  sample.fraction = sample_fraction_optimal,  # Optimal sample fraction
  seed = 123
)

# Sample rows for SHAP value explanation from training data
X <- analysis_data[sample(nrow(analysis_data), 200), predictors]
# Select background data from training data
bg_X <- analysis_data[sample(nrow(analysis_data), 100), predictors]

# Calculate SHAP values
system.time({
  ks <- kernelshap(rf_model, X, bg_X = bg_X)
})

#####
focal_ks <- ks
# Visualize the importance
sv <- shapviz(focal_ks)
sv_plot <- sv_importance(sv, kind = "bee") +
  labs(title = "Bee swarm plot") +
  theme(axis.title = element_blank())

rank_plot <-  sv_importance(sv, show_numbers = TRUE)+
  labs(title = "Ranking plot")+
  theme(axis.title = element_blank())

# Extract SHAP values matrix if needed
shap_values <- focal_ks$S  # Adjust this based on the actual structure of 'ks'
# Assuming 'shap_values' is a data frame where columns are variables and rows are observations
mean_abs_shap <- colMeans(abs(shap_values), na.rm = TRUE)
ordered_vars <- names(sort(mean_abs_shap, decreasing = TRUE))[1:13]
# Subset and reorder SHAP values for the top 15 variables
shap_values_top <- shap_values[, ordered_vars]
# Compute the distance matrix for these top variables (transpose to measure distance between variables)
dist_matrix_top <- dist(t(shap_values_top))
# Perform hierarchical clustering
hc_top <- hclust(dist_matrix_top)
# Convert to a dendrogram
dendro_top <- as.dendrogram(hc_top)
# Convert the dendrogram to a format suitable for ggplot
dendro_data_top <- dendro_data(dendro_top, type = "rectangle")

# Create a ggplot dendrogram, ensuring variable names are visible
gg_dendro_top <- ggplot() +
  geom_segment(data = dendro_data_top$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = dendro_data_top$labels, aes(x = x, y = y, label = label), size=2.5, hjust = 1, vjust = -0.5) +
  coord_flip() +  # Rotates the plot by 90 degrees
  scale_x_reverse() +
  scale_y_reverse() +
  theme_minimal() +
  labs(title = "  Dendrogram of features")+
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank()
  )
# Print the plot
ggarrange(gg_dendro_top, rank_plot, sv_plot, nrow=1, labels=c('a','b','c'), widths = c(0.6,1,1))

ggsave("RandomForest_IC_0905.jpg", width = 12, height = 5)


# fwrite(reduced_data, "RF_dataset.csv")
saveRDS(rf_model,"RF_IC_main.rds")
saveRDS(ks,"KernelShap_IC_main.rds")
