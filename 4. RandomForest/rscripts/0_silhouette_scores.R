library(cluster)

# Calculate the hierarchical clustering as before
phylo_dist <- cophenetic.phylo(bird_tree)
hc <- hclust(as.dist(phylo_dist))

# Function to calculate average silhouette for different numbers of clusters
calculate_silhouette <- function(k) {
  groups <- cutree(hc, k = k)
  silhouette_widths <- silhouette(groups, dist(phylo_dist))
  mean(silhouette_widths[, "sil_width"])
}

# Test different values of k and store results
k_values <- 2:40  # Adjust this range based on your expectation and dataset size
silhouette_scores <- sapply(k_values, calculate_silhouette)

# Plot silhouette scores to find the optimal k
plot(k_values, silhouette_scores, type = 'b', pch = 19, xlab = "Number of Clusters", ylab = "Average Silhouette Width",
     main = "Silhouette Analysis to Determine Optimal k")

