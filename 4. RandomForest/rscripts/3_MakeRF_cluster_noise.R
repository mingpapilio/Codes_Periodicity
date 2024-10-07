set.seed(321)
reduced_data$NoiseFeature <- rnorm(nrow(reduced_data))

set.seed(123)
# Prepare data: Define predictors and response variable
predictors <- c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", 
                "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym",  "Cluster", "NoiseFeature")
# c("TMPavg", "PREavg", "Cluster")  # Update with relevant features
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
#   mtry min.node.size sample.fraction       auc exec.time
# 1    1            34        0.853302 0.8046236 0.1385714
mtry_optimal <- tuned_params$recommended.pars$mtry
min_node_size_optimal <- tuned_params$recommended.pars$min.node.size
sample_fraction_optimal <- tuned_params$recommended.pars$sample.fraction
#
rf_model<- ranger(
  COOP2 ~ ., 
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
ordered_vars <- names(sort(mean_abs_shap, decreasing = TRUE))[1:14]
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

ggsave("RandomForest_baseline_0805.jpg", width = 12, height = 5)


fwrite(reduced_data, "RF_dataset.csv")
saveRDS(rf_model,"RF_cluster_main.rds")
saveRDS(ks,"KernelShap_cluster_main.rds")
