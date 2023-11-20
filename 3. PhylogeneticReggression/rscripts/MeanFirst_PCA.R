# Load packages
library(data.table)
library(phylolm)
library(ape)
library(rr2)
library(dplyr)
# Read in the files
bird_data<- fread("../data/Bird_Clm_01Oct.csv")
bird_tree<- read.tree("../data/Bird_Tree_17Mar.tre")
# Standardise the climatic quantities
bird_data<- bird_data %>% mutate(across(TMPavg:PREprd_Ent, .fns= ~scale(.x), .names="Z{col}"))

# Make the PCs and phylogenetic regressions (Modified from Charlie's codes)
## 1. Only the means ####
bird_focal<- dplyr::select(bird_data, ZTMPavg, ZPREavg)
# Need to convert to matrix to properly assign rownames
bird_focal<- as.matrix(bird_focal)
rownames(bird_focal)<- bird_data$animal
# Make the phyloPCA model
pca_mean<- phyl.pca(bird_tree, bird_focal, method="BM", mode="cov")
# Collect the first two PCs
focal_pca<- pca_mean
data_means<- data.frame(bird_data, PC1= focal_pca$S[,1][match(bird_data$animal, rownames(focal_pca$S))],
                        PC2= focal_pca$S[,2][match(bird_data$animal, rownames(focal_pca$S))])
## 2. Means+ SDs ####
bird_focal<- dplyr::select(bird_data, ZTMPavg, ZPREavg, ZTMPsd, ZPREsd)
bird_focal<- as.matrix(bird_focal)
rownames(bird_focal)<- bird_data$animal
pca_meansd<- phyl.pca(bird_tree, bird_focal, method="BM", mode="cov")
# Collect the first two PCs
focal_pca<- pca_meansd
data_meansd<- data.frame(bird_data, PC1= focal_pca$S[,1][match(bird_data$animal, rownames(focal_pca$S))],
                         PC2= focal_pca$S[,2][match(bird_data$animal, rownames(focal_pca$S))],
                         PC3= focal_pca$S[,3][match(bird_data$animal, rownames(focal_pca$S))])
## 3. Means+ SDs+ prds (fixed binning) ####
bird_focal<- dplyr::select(bird_data, ZTMPavg, ZPREavg, ZTMPsd, ZPREsd, ZTMPprd_Fix, ZPREprd_Fix)
bird_focal<- as.matrix(bird_focal)
rownames(bird_focal)<- bird_data$animal
pca_fix<- phyl.pca(bird_tree, bird_focal, method="BM", mode="cov")
# Collect the first two PCs
focal_pca<- pca_fix
data_fix<- data.frame(bird_data, PC1= focal_pca$S[,1][match(bird_data$animal, rownames(focal_pca$S))],
                      PC2= focal_pca$S[,2][match(bird_data$animal, rownames(focal_pca$S))],
                      PC3= focal_pca$S[,3][match(bird_data$animal, rownames(focal_pca$S))])
## 4. Means+ SDs+ prds (dynamic binning) ####
bird_focal<- dplyr::select(bird_data, ZTMPavg, ZPREavg, ZTMPsd, ZPREsd, ZTMPprd_Dym, ZPREprd_Dym)
bird_focal<- as.matrix(bird_focal)
rownames(bird_focal)<- bird_data$animal
pca_dym<- phyl.pca(bird_tree, bird_focal, method="BM", mode="cov")
# Collect the first two PCs
focal_pca<- pca_dym
data_dym<- data.frame(bird_data, PC1= focal_pca$S[,1][match(bird_data$animal, rownames(focal_pca$S))],
                      PC2= focal_pca$S[,2][match(bird_data$animal, rownames(focal_pca$S))],
                      PC3= focal_pca$S[,3][match(bird_data$animal, rownames(focal_pca$S))])
## 5. Means+ SDs+ prds (spectral entropy) ####
bird_focal<- dplyr::select(bird_data, ZTMPavg, ZPREavg, ZTMPsd, ZPREsd, ZTMPprd_Ent, ZPREprd_Ent)
bird_focal<- as.matrix(bird_focal)
rownames(bird_focal)<- bird_data$animal
pca_ent<- phyl.pca(bird_tree, bird_focal, method="BM", mode="cov")
# Collect the first two PCs
focal_pca<- pca_ent
data_ent<- data.frame(bird_data, PC1= focal_pca$S[,1][match(bird_data$animal, rownames(focal_pca$S))],
                      PC2= focal_pca$S[,2][match(bird_data$animal, rownames(focal_pca$S))],
                      PC3= focal_pca$S[,3][match(bird_data$animal, rownames(focal_pca$S))])
# Make the phylogenetic regression models
pcareg_mean<- phyloglm(COOP2~ PC1+ PC2, data= data_means, phy=bird_tree, method= "logistic_MPLE", btol=30)
pcareg_meansd<- phyloglm(COOP2~ PC1+ PC2, data= data_meansd, phy=bird_tree, method= "logistic_MPLE", btol=30)
pcareg_fix<- phyloglm(COOP2~ PC1+ PC2, data= data_fix, phy=bird_tree, method= "logistic_MPLE", btol=30)
pcareg_dym<- phyloglm(COOP2~ PC1+ PC2, data= data_dym, phy=bird_tree, method= "logistic_MPLE", btol=30)
pcareg_ent<- phyloglm(COOP2~ PC1+ PC2, data= data_ent, phy=bird_tree, method= "logistic_MPLE", btol=30)
## Compare overall R2s
c(R2_lik(pcareg_mean), R2_lik(pcareg_meansd), R2_lik(pcareg_fix), R2_lik(pcareg_dym), R2_lik(pcareg_ent))
## Compare AICs
c(AIC(pcareg_mean), AIC(pcareg_meansd), AIC(pcareg_fix), AIC(pcareg_dym), AIC(pcareg_ent))
