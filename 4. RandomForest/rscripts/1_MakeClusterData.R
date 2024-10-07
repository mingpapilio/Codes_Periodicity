# Load required libraries
library(data.table)
library(ape)        # For phylogenetic analysis

# Read and prepare data
bird_data <- fread("../Data/Bird_Clm_01Oct.csv")
bird_tree <- read.tree("../Data/Bird_Tree_17Mar.tre")

# Set row names to match animal names for easier subsetting
bird_data2 <- as.data.frame(bird_data)
rownames(bird_data2) <- bird_data$animal

# Check for consistent species names between data and tree
species_in_data <- rownames(bird_data2)
species_in_tree <- bird_tree$tip.label

# Ensure all species in data are in the tree
matched_species <- intersect(species_in_data, species_in_tree)

# Subset bird_data2 and the tree to matched species
bird_data2 <- bird_data2[matched_species, , drop = FALSE]
bird_tree <- drop.tip(bird_tree, setdiff(species_in_tree, matched_species))

# Step 1: Compute the phylogenetic distance matrix
phylo_dist <- cophenetic.phylo(bird_tree)

# Step 2: Perform hierarchical clustering
hc <- hclust(as.dist(phylo_dist), method = "average")

# Step 3: Cut the tree into 2 clusters
clusters <- cutree(hc, k = 22)

# Step 4: Create a new data column for cluster assignment
bird_data2$Cluster <- clusters[match(rownames(bird_data2), names(clusters))]

# Selecting the specific predictors related to temperature and precipitation
predictors <- c("TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", 
                "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym", "Cluster")

# Include the response variable
all_vars <- c("COOP2", predictors)
reduced_data <- bird_data2[, all_vars]
