# Codes_Periodicity

This repository has the codes and data for the manuscript "__The explanatory power of climatic datasets: environmental predictability, correlations, and adaptation__".  There are three sections of the data and codes in this repository: global analysis, regional analysis, and phylogenetic regression. 

1. __GlobalAnalysis__ contains the processed CRUTS datasets in __/data__, the files to make them in __/makefiles__, and a map-making script in __/rscrips__.
2. __RegionalAnalysis__ contains the modified dataset in __/data__ and a script for correlation plots in __/rscripts__. The dataset is modified from [Cornwallis et al, 2017](https://www.nature.com/articles/s41559-016-0057). We updated the climatic quantities for each row.
3. __PhylogeneticRegression__ contains the dataset and the phynogenetic tree in __/data__, and the analysis codes in __/rscripts__. There are three code script files: one for the regression without phylogenetic PCA (Table 2), another one for the regression with PCA (Table 4), and the other one for the relative variable importance (RVI) analysis introduced in section 3.1.3.

4. __RanfomForest__ contains the dataset, phylogenetic tree, the analysis codes and the output. In particular, the ourput folder has the cross-validation data, the constructed RF model, and the SHAPviz object from the Shapley value analysis.
