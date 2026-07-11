# A Comparative Study of Imputation Techniques for Clustering with Incomplete Giving Behavior Data

By Christie Ngo, An Truong, and Baixue (Doris) Zhang

## Project Overview

This project studies how different missing-data imputation techniques affect downstream clustering results on incomplete behavioral donation data.

The original analysis was developed in R for MATH 252 Cluster Analysis. 

## Research Question

How sensitive are clustering conclusions to the way missing values are handled?

More specifically, this project asks whether different imputation methods lead to materially different donor segments when applied to an incomplete charity giving dataset.

Are there any meaningful clusters in this charity dataset?

## Data

The dataset comes from a giving behavior / dictator game study. Participants made donation decisions under different multiplier conditions and also reported demographic, political, and religious information.

Key variables include:

- `amount`: amount donated in the dictator game
- `multiplier`: experimental multiplier applied to the donation
- `age`, `gender`, `income`: demographic variables
- political orientation score
- religious orientation score
- 2016 presidential vote
- U.S. state of residence
- political party identification
- religious service attendance
- work time variables used as data quality indicators

The raw data contains missing values because the dataset combines a main experiment with a follow-up survey. Not all participants completed the follow-up survey.

## R Workflow

1. Load and inspect the raw MTurk / survey data.
2. Remove identifiers and non-analytic metadata, including worker IDs, IP addresses, latitude/longitude, assignment IDs, survey codes, and high-cardinality timestamps.
3. Convert `CreationTime` into a batch indicator.
4. Explore missingness, variable distributions, and relationships between donation amount and political/religious variables.
5. Create multiple processed datasets using different missing-value strategies.
6. Run clustering methods on numeric, categorical, and mixed-type versions of the data.
7. Compare cluster stability across imputation methods using Adjusted Rand Index.
8. Interpret clusters using donation amount, demographics, political identity, and religious behavior.

## Dataset Lineage

The project contains two cleaned-data files:

- `Datasets/clean_data_V1.csv`
- `Datasets/clean_data.csv`

They have the same shape and columns: 792 rows and 14 variables. The difference is how special response categories were handled.

`clean_data_V1.csv` appears to be the earlier cleaned dataset used to generate the V1 imputed datasets. It still keeps special responses as ordinary categories:

- `age == "Prefer not to state"`: 1 row
- `income == "Prefer not to state"`: 11 rows
- `gender == "Other / Prefer not to state"`: 8 rows

`clean_data.csv` appears to be the later cleaned dataset. It applies additional cleaning:

- `age == "Prefer not to state"` is converted to missing
- `income == "Prefer not to state"` is converted to missing
- `gender == "Other / Prefer not to state"` is recoded as `Other`

The reconstructed R data flow is:

```text
Raw Data.csv
   ↓
Data_Cleaning.Rmd
   ↓
Datasets/clean_data_V1.csv
   ↓
Datasets/generate_v1_imputed_datasets.R
   ↓
Datasets/V1/*.csv
   ↓
DATASETS_Final.R
   ↓
Datasets/V2/*.csv
```

The corresponding Python/Zerve-oriented data flow is:

```text
Raw Data.csv
   ↓
Data_Cleaning/data_cleaning_python.ipynb
   ↓
Datasets/clean_data_V1.csv
   ↓
Datasets/generate_v1_imputed_datasets.ipynb
   ↓
Datasets/V1/*.csv
   ↓
Datasets/DATASETS_Final.py
   ↓
Datasets/V2/*.csv
```

The original first-stage imputation script that generated `Datasets/V1/*.csv` appears to have been removed accidentally. This repository now includes a reconstructed version:

- `Datasets/generate_v1_imputed_datasets.R`
- `Datasets/generate_v1_imputed_datasets.ipynb`

These files regenerate the V1 imputed datasets from `Datasets/clean_data_V1.csv`.

`DATASETS_Final.R` starts from the existing V1 files, cleans the remaining special categories, imputes the remaining `income` values, and writes the final V2 datasets.

For a cleaner Python/Zerve rebuild, the recommended flow is to start from the later `clean_data.csv` logic and generate all imputed datasets directly from that cleaned input.

## Imputation Methods

The project compares several versions of the dataset:

- Baseline with all the rows with missing values removed
- Mean/mode imputation
- KNN imputation with `k = 5`
- KNN imputation with `k = 10`
- MICE imputation, version 1
- MICE imputation, version 2

The purpose is not only to fill missing values, but to evaluate whether the imputation choice changes the cluster structure.

## Clustering Methods

The original R analysis uses several clustering approaches:

- K-means clustering for numeric variables
- Hierarchical clustering with average linkage
- K-prototypes for mixed numeric and categorical data
- Gower distance for mixed-data dissimilarity
- Gaussian mixture models
- Skew-t and generalized hyperbolic distribution mixture models
- Categorical clustering with MCA / clusCA

Model selection and evaluation include:

- Elbow plots
- Silhouette scores
- Gap statistic
- Calinski-Harabasz index
- Hopkins statistic
- VAT plots
- Adjusted Rand Index for cluster stability

## Repository Structure

```text
.
├── Clustering/
│   ├── Basline_Analysis.R
│   ├── Kmeans.qmd
│   ├── Model_Based.R
│   ├── k_prototype.R
│   └── without_amount.R
├── Data_Cleaning/
│   ├── Data_Cleaning.Rmd
│   └── Data_Cleaning.html
├── Datasets/
│   ├── clean_data.csv
│   ├── clean_data_V1.csv
│   ├── DATASETS_Final.R
│   ├── generate_v1_imputed_datasets.R
│   ├── generate_v1_imputed_datasets.ipynb
│   ├── V1/
│   └── V2/
├── EDA/
│   └── exclusive EDA Across Datasets.R
├── Paper and Materials/
│   ├── Raw Data.csv
│   └── price_of_giving.pdf
├── Report/
│   ├── MATH 252 Group Project_Group6_Slides.pdf
│   └── Math 252 Project Report_Group6_A Comparative Study of Imputation Techniques for Clustering with Incomplete Giving Behavior Data.pdf
└── README.md
```

## Framing

The main story of this project is:

> I studied how missing-data decisions can change unsupervised learning conclusions. I rebuilt a behavioral donation dataset under multiple imputation strategies, clustered each version, compared cluster stability, and interpreted whether donor segments remained consistent across preprocessing choices.

This framing emphasizes practical data science judgment:

- missing data handling
- reproducible preprocessing
- unsupervised model comparison
- cluster validation
- sensitivity analysis
- explainable segmentation
