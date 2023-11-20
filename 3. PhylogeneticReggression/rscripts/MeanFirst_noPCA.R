# Load packages
library(data.table)
library(phylolm)
library(ape)
library(rr2)
library(dplyr)
# Read in the files
bird_data<- fread("../data/Bird_Clm_01Oct.csv")
bird_tree<- read.tree("../data/Bird_Tree_17Mar.tre")
# Data processing for phyloglm
bird_data2<- bird_data
rownames(bird_data2)<- bird_data$animal
# Make phlylogenetic regression models
pgls_avgs<- phyloglm(COOP2~TMPavg+sqrt(PREavg)
                     , data= bird_data2, phy=bird_tree, method= "logistic_MPLE", btol=30)
pgls_sd<- phyloglm(COOP2~TMPavg+sqrt(PREavg)+sqrt(TMPsd)+log(PREsd)
                     , data= bird_data2, phy=bird_tree, method= "logistic_MPLE", btol=30)
pgls_full_fix<- phyloglm(COOP2~TMPavg+sqrt(PREavg)+sqrt(TMPsd)+log(PREsd)+TMPprd_Fix+PREprd_Fix
                         , data= bird_data2, phy=bird_tree, method= "logistic_MPLE", btol=30)
pgls_full_dym<- phyloglm(COOP2~TMPavg+sqrt(PREavg)+sqrt(TMPsd)+log(PREsd)+TMPprd_Dym+PREprd_Dym
                         , data= bird_data2, phy=bird_tree, method= "logistic_MPLE", btol=30)
pgls_full_ent<- phyloglm(COOP2~TMPavg+sqrt(PREavg)+sqrt(TMPsd)+log(PREsd)+TMPprd_Ent+PREprd_Ent
                         , data= bird_data2, phy=bird_tree, method= "logistic_MPLE", btol=30)
# Summary of each model
c(AIC(pgls_avgs), rr2::R2_lik(pgls_avgs))
c(AIC(pgls_sd), rr2::R2_lik(pgls_sd))
c(AIC(pgls_full_fix), rr2::R2_lik(pgls_full_fix))
c(AIC(pgls_full_dym), rr2::R2_lik(pgls_full_dym))
c(AIC(pgls_full_ent), rr2::R2_lik(pgls_full_ent))
