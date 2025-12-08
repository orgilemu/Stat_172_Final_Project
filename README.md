
---
title: "Analysis of Discrimination Complaint Cases in Iowa"
authors: "Kendall Starcevich, Olivia Soppa, & Munkh-Orgil Tumurchudur"
date: "12/08/25"
output: 
---

## Introduction
This repository contains the code and data required to reproduce the results found in "Closed Discrimination Complaint Cases in Iowa". Our objective is to understand key factors of discrimination cases, and develop a predictive model to inform victims of the possibilities of winning their case by using...

## Requirements
To install the required R packages, run the following code in R:

```r
install.packages(c("tidyverse","pROC", "glmnet", "lubridate", "dplyr",
                   "forcats", "rpart", "rpart.plot", "ggplot2", "tidyr", "scales"))
```


## Data
The dataset we used in this project is publicly available through the State of Iowa's Open Data Portal:

State of Iowa, (2023). Closed Discrimination Complaint Cases in Iowa, https://catalog.data.gov/dataset/closed-discrimination-complaint-cases-in-iowa.

This data can be also found in the following sub-directories:
`raw/Closed_Discrimination_Complaint_Cases_in_Iowa.csv`

## Reproduce

To fully reproduce the analysis and results, run the scripts in the following order. 
all

1. Run `src/data_exploration_and_cleaning.R` to clean the raw data and create the binary outcome `Outcome_bin`.

2. Run `src/variable_exploration.R` to reproduce to explore variables to see what kind of relationships exists in this data.
  *  plots/Case_Outcomes_over_Processing_Time.png
  *  plots/Case_Outcomes_over_Time.png
  *  plots/Case_Outcomes_over_Timely_Cases.png
  *  plots/Favorable_Outcome_Rates_by_Complaint_Profile.png
  *  plots/Favorable_Outcome_Rates_by_Race_Type.png
  *  plots/Favorable_Outcomes_Rates_Vary_by_Complaints_Basis.png
  *  plots/Favorable_Outcomes_Rates_Vary_by_Processing_Agency.png
  *  plots/Favorable_Outcomes_by_Demographic_Basis.png
  *  plots/Favorable_Outcomes_tend_to_have_shorter_processing_time.png
  
  
3. Run `src/Lasso & Ridge.R` to reproduce logistic regression results (MLE, lasso, ridge) and ROC comparison.
  *  plots/ROC_curve.png

4. Run `src/random_forest.R` to reproduce random forest results and evaluation.
  *  plots/Random_forest.png
6. Run `src/hammock_plots.R` to reproduce hammock chart visualizations.
  *  plots/Discrimination_Basis_To_Outcome_Hammock


## References
State of Iowa, (2023). Closed Discrimination Complaint Cases in Iowa, https://catalog.data.gov/dataset/closed-discrimination-complaint-cases-in-iowa.
