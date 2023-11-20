# Load packages
library(data.table)
library(MuMIn)
library(phytools)
library(phylolm)
library(dplyr)

# Read in
raw.data<- fread("../Data/Bird_Clm_01Oct.csv")
raw.tree<- read.tree("../Data/Bird_Tree_17Mar.tre")
# Data process for RVI and for phylolm
bird.data<- raw.data
bird.data<- bird.data %>% mutate(TMPsd_tr= sqrt(TMPsd), PREavg_tr= sqrt(PREavg), PREsd_tr= log(PREsd))
rownames(bird.data)<- raw.data$animal
# Pick the response and predictors (using spectral entropy as an example)
response <- 'COOP2'
predictors <- c('TMPavg','TMPsd_tr','TMPprd_Ent','PREavg_tr','PREsd_tr','PREprd_Ent') # For spectral entropy
# predictors <- c('TMPavg','TMPsd_tr','TMPprd_Fix','PREavg_tr','PREsd_tr','PREprd_Fix') # For fixed binning
# predictors <- c('TMPavg','TMPsd_tr','TMPprd_Dym','PREavg_tr','PREsd_tr','PREprd_Dym') # For dynamic binning

# Perform RVI analysis (Modified from Carlos' codes)
## Define all possible candidate models in the model set 
AllMods <- list(predictors)
counter <- 2
for (i in seq((length(predictors)-1), 1, by = -1)) {
  thesemods <- combn(predictors,i)
  for (j in 1:dim(thesemods)[2]){
    AllMods[[counter]] <- thesemods[,j]
    counter <- counter + 1
  }
}
## Estimate all models and compute their AICc weights
mymods <- list()
my.aicc <- NULL
n <- dim(bird.data)[1]
for ( i in 1:length(AllMods) ) {
  mymods[[i]] <- eval(parse(text=paste('phyloglm(formula =', response, '~', 
                                       paste(AllMods[[i]], collapse = ' + '),
                                       ', data = bird.data, phy=raw.tree, btol=100)')))
  k <- length(AllMods[[i]]) + 1 # Assumes estimating an intercept (otherwise remove the "+1")
  # Compute AICc 
  my.aicc[i] <- AIC(mymods[[i]]) + (2*k^2 + 2*k)/(n-k-1)
}
## Get the corresponding AICc weights
my.waicc <- Weights(my.aicc)

## Compute relative variable importance 
RVI <- data.frame(matrix(0,length(predictors)+1,1))
row.names(RVI) <- c('(Intercept)',predictors)
names(RVI) <- 'RVI'
RVI['(Intercept)',1] <- 1

for (i in 1:length(my.waicc)) {
  for (j in 1:length(predictors)) {
    if (length(grep(predictors[j], AllMods[[i]]))>0) {
      RVI[predictors[j],1] <- RVI[predictors[j],1] + my.waicc[i]
    }
  }
}

# Summary of relative variable importances
RVI
