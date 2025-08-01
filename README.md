# Environmental predictability in phylogenetic comparative analysis: how to measure it and does it matter?

This dataset contains the raw and processed data, derived climatic measures, phylogenetic trees, and analysis scripts associated with the manuscript titled above. The analyses explore the role of environmental predictability in shaping organismal traits and adaptation patterns, combining global climate data, regional biological datasets, and phylogenetic comparative methods.

## Description of the Data and File Structure

The dataset is structured into four main directories, each corresponding to a major analysis section of the manuscript:

### 1. `GlobalAnalysis`
- `/data/`:  Contains processed CRU TS climate datasets.

  These datasets retain the same structure as CRU TS:
  - **Temporal resolution**: Monthly
  - **Spatial resolution**: 0.5° latitude × 0.5° longitude
  - **Units**:
    - Temperature: degrees Celsius (°C)
    - Precipitation: millimeters (mm)

  Temperature and precipitation are provided in **two separate files**, each containing the following columns (values are presented in raw scale unless otherwise noted):
  - Column names
    - `lon`, `lat`: geographic coordinates
    - `avg`, `sd`, `med`: mean, standard deviation, and median of the climate variable
    - `cv`: coefficient of variation (unitless)
    - `prd_SpcEntropy`: predictability based on **normalized spectral entropy** (unitless, 0–1)
    - `prd_ColwellFix`, `cst_ColwellFix`, `ctg_ColwellFix`: Colwell's predictability, constancy, and contingency with **fixed binning** (unitless, 0–1)
    - `prd_ColwellDym`, `cst_ColwellDym`, `ctg_ColwellDym`: same metrics calculated with **dynamic binning** (unitless, 0–1)

- `/makefiles/`: Scripts and configuration files for generating processed climatic variables from raw CRU TS data.
- `/rscripts/`: Contains R scripts for generating global summary maps (used in main figures).

### 2. `RegionalAnalysis`
- `/data/`: Contains modified species trait and distribution datasets originally based on [Cornwallis et al. 2017](https://www.nature.com/articles/s41559-016-0057). The dataset from the paper is publically available at this [link](https://static-content.springer.com/esm/art%3A10.1038%2Fs41559-016-0057/MediaObjects/41559_2016_BFs415590160057_MOESM227_ESM.xlsx). Climatic variables have been updated using the methods developed in this project and the species range provided by Cornwallis et al. 
  - Column names
    - `clade`: passerine or non-passerine
    - `animal` and `species`: species name
    - `care`: the type of parental/cooperative care
    - `breedsystem`: breeding system types
    - `coop`: whether the species is cooperative (string format)
    - `COOP2`: whether the species is cooperative (binary format)
    - `longitude` and `latitude`: the coordinate of the field site

  We added columns after 'latitude', following the same abbreviation rule as `GlobalAnalysis` with the prefix of TMP for temperature and PRE for precipitation. In addition, we also calculated similar summary stats for diurnal temperature range (DTP) and net primary production (NPP) from CRUTS.

  **Abbreviations for variable prefixes:**
  * `TMP`: Temperature (°C)
  * `PRE`: Precipitation (mm)
  * `DTP`: Diurnal Temperature Range (°C; not used in this project)
  * `NPP`: Net Primary Productivity (g C/m²/month; not used in this project)

- `/rscripts/`: Includes scripts for correlation visualization and exploratory analysis of trait–climate relationships.

### 3. `PhylogeneticRegression`
- `/data/`: Contains the trait data matrix and phylogenetic tree used in the regression analyses.
- `/rscripts/`: Includes three key scripts:
  - **Non-PCA regression**: Used to produce results in Table 2.
  - **Regression with phylogenetic PCA**: Corresponds to Table 4.
  - **Relative Variable Importance (RVI)** analysis: Introduced in Section 3.1.3.

### 4. `RandomForest`
- `/Data/`: Contains input datasets and the phylogenetic tree used in the Random Forest model. The data files are identical to those in `PhylogeneticRegression`.
- `/Output/`: Contains model output including:
  - Bird_Clm_IC_04Sep.csv: Input trait–climate dataset after phylogenetically independent contrasts (used for some Random Forest models)
  - CrossVal_GLM.csv: Cross-validation results for GLM
  - CrossVal_GLMM.csv: Cross-validation results for GLMM with with cluster-based random effects to control for phylogeny
  - CrossVal_RF.csv: Cross-validation results for RF with all variables and cluster
  - CrossVal_GLM_IC.csv: Cross-validation results for GLM using phylogenetically independent contrasts (IC) based on species-level trait averages
  - CrossVal_RF_IC.csv: Cross-validation results for RF with IC
  - RF_cluster_main.rds: Trained RF model (cluster-based)
  - RF_IC_main.rds: Trained RF model (IC-based)
  - KernelShap_cluster_main.rds: SHAP object for cluster-based RF
  - KernelShap_IC_main.rds: SHAP object for IC-based RF

- `/rscripts/`: Includes 12 R scripts for:
  - 0_silhouette_scores.R: Determine optimal cluster number
  - 1_MakeClusterData.R: Create cluster structure from trait data
  - 2.1_CrossVal_GLM(means).R: GLM cross-validation (cluster)
  - 2.2_CrossVal_GLMM(means_cluster).R: GLMM cross-validation
  - 2.3_HyperParamTuning.R: RF hyperparameter search (cluster)
  - 2.4_CrossVal_RF.R: RF cross-validation (cluster)
  - 3_MakeRF_cluster_noise.R: Build RF with cluster and noise (reference) predictors
  - 4_MakeDataset_IC.R: Construct datasets using phylogenetic independent contrasts (IC)
  - 5.1_CrossVal_GLM(ICmeans).R: GLM cross-validation (IC)
  - 5.2_HyperParams.R: RF hyperparameter search (IC)
  - 5.3_CrossVal_RF_IC.R: RF cross-validation (IC)
  - 6_MakeRF_IC_noise.R: Build RF with IC and noise (reference) predictors
  - (Scripts are numbered to suggest the intended execution order.)

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

## License

This project is released under [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). You are free to use, modify, and redistribute it for any purpose, without permission. If you use the code, please cite the accompanying publication.
