# Evaluating the performance of satellite-derived evapotranspiration products across varying bioclimates in South Africa

This repository hosts the code for the processing of acquired evapotranspiration time series datasets.

## Summary
In this study we aim to evaluate the performance of freely available satellite-derived evapotranspiration products across varying bioclimates in South Africa. 

Satellite-derived evapotranspiration products were evaluated against in situ observations at a monthly time-step. Evapotranspiration time series datasets from flux towers across South Africa as well as from satellite-derived evapotranspiration products were collated and processed. Metrics used for the evaluation included correlation (Spearman Correlation Coefficient), Percent Bias and Kling-Gupta Efficiency for different temporal levels including overall, log transformed data, interannual and seasonal.
The best performing satellite-derived evapotranspiration products were identified and integrated into a mean ensemble model for South Africa, which was then also evaluated.

 - Note: Evapotranspiration refers to the combination of the evaporation of water from the Earths surface and transpiration from vegetation.

## Repository Components
The components of this repository include two primary steps each including a set of method workflow notes and code scripts
The two steps include the following:

A) Evaluate satellite-derived evapotranspiration products performance
- Collate and process field-measured and satellite-derived evapotranspiration time series datasets
- Evaluate the performance of satellite-derived evapotranspiration products

B) Select products for an ensemble and evaluate its performance
- Collate evapotranspiration time series using ensemble
- Evaluate the ensembles performance

![WorkFlow](https://github.com/user-attachments/assets/9db699e7-8c30-4ada-b09d-8215b40a854c)

