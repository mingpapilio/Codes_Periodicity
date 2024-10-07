library(ape)
library(data.table)
library(tidyverse)
library(cluster)  # For hierarchical clustering
library(data.table)
library(ape)        # For phylogenetic analysis
library(vegan)      # For PCA
set.seed(123) # For reproducibility

# Step 1: Read and Prepare Data ####
# Read data and phylogenetic tree
bird_data <- fread("../Data/Bird_Clm_01Oct.csv")
bird_tree <- read.tree("../Data/Bird_Tree_17Mar.tre")

# Selecting the specific predictors related to temperature and precipitation
vars <- c("COOP2","TMPavg", "TMPsd", "TMPcv", "TMPprd_Ent", "TMPprd_Fix", "TMPprd_Dym", 
          "PREavg", "PREsd", "PREcv", "PREprd_Ent", "PREprd_Fix", "PREprd_Dym")

# Set row names to match animal names for easier subsetting
bird_data2 <- bird_data %>% dplyr::select(all_of(vars))
bird_data2 <- as.data.frame(bird_data2)
rownames(bird_data2) <- bird_data$animal
# Sort bird_data2 to match the tree tips
bird_data2 <- bird_data2[match(bird_tree$tip.label, rownames(bird_data2)), ]


# Calculate independent contrasts for each variable
calculate_contrasts <- function(variable, tree) {
  contrasts <- pic(bird_data2[[variable]], tree)
  return(contrasts)
}

# List of all variables for which we want to calculate contrasts
variables_to_contrast <- names(bird_data2)

# Create an empty list to store the contrasts
independent_contrasts <- list()

# Loop through each variable and calculate the contrasts
for (var in variables_to_contrast) {
  independent_contrasts[[paste0("IC_", var)]] <- calculate_contrasts(var, bird_tree)
}

# Convert the list to a data frame
contrast_data <- as.data.frame(independent_contrasts)

# Check the final dataset
head(contrast_data)

fwrite(contrast_data, "Bird_Clm_IC_04Sep.csv")
