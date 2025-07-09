# Environmental predictability in phylogenetic comparative analysis: how to measure it and does it matter?

This dataset contains the raw and processed data, derived climatic measures, phylogenetic trees, and analysis scripts associated with the manuscript titled above. The analyses explore the role of environmental predictability in shaping organismal traits and adaptation patterns, combining global climate data, regional biological datasets, and phylogenetic comparative methods.

## Description of the Data and File Structure

The dataset is structured into four main directories, each corresponding to a major analysis section of the manuscript:

### 1. `1. GlobalAnalysis`
- `/data/`: Contains processed CRU TS climate datasets, such as annual means and seasonality indices.
- `/makefiles/`: Scripts and configuration files for generating processed climatic variables from raw CRU TS data.
- `/rscripts/`: Contains R scripts for generating global summary maps (used in main figures).

### 2. `2. RegionalAnalysis`
- `/data/`: Contains modified species trait and distribution datasets originally based on [Cornwallis et al. 2017](https://www.nature.com/articles/s41559-016-0057). Climatic variables have been updated using the methods developed in this project.
- `/rscripts/`: Includes scripts for correlation visualization and exploratory analysis of trait–climate relationships.

### 3. `3. PhylogeneticRegression`
- `/data/`: Contains the trait data matrix and phylogenetic tree used in the regression analyses.
- `/rscripts/`: Includes three key scripts:
  - **Non-PCA regression**: Used to produce results in Table 2.
  - **Regression with phylogenetic PCA**: Corresponds to Table 4.
  - **Relative Variable Importance (RVI)** analysis: Introduced in Section 3.1.3.

### 4. `4. RandomForest`
- `/data/`: Contains input datasets and the phylogenetic tree used in the Random Forest model.
- `/output/`: Contains model output including:
  - Cross-validation performance metrics,
  - Trained Random Forest models,
  - SHAPviz R objects used for interpreting variable importance based on Shapley values.
- `/rscripts/`: Includes 12 R scripts for:
  - Data preprocessing and clustering,
  - Model fitting and parameter tuning,
  - SHAP-based interpretation.

## Sharing / Access Information

This dataset is also mirrored at the following public repository:

- Dryad: [https://doi.org/10.5061/dryad.gtht76j02](https://doi.org/10.5061/dryad.gtht76j02)

Note that the GitHub version may receive future updates or revisions, while the Dryad version represents the version archived at the time of manuscript publication.

### Data sources used or adapted:
- **CRU TS** (Climatic Research Unit time-series data): [https://crudata.uea.ac.uk/cru/data/hrg/](https://crudata.uea.ac.uk/cru/data/hrg/)
- **Cornwallis et al. 2017** dataset: [https://www.nature.com/articles/s41559-016-0057](https://www.nature.com/articles/s41559-016-0057)

All modifications or derived variables are documented in the `makefiles` directory and described in the manuscript.

## Code / Software

All R scripts used for analysis are included in the respective `/rscripts/` folders in each section. These scripts are compatible with R ≥ 4.2.1 and rely on standard packages including:

- `ape`
- `phylolm`
- `randomForest`
- `SHAPforxgboost`
- `cluster`
- `ggplot2`

Scripts are numbered to suggest the intended execution order. For example:
- `2.1_...R` and `2.2_...R` perform cross-validation and cluster preparation before Random Forest modeling in `2.4_CrossVal_RF.R`.
- SHAP value analysis is handled in `5.3_CrossVal_RF_IC.R`.

All code is released under the CC0 waiver as part of this dataset. If you use the code, please cite the accompanying publication.
