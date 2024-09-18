# Replication Package for "Can We Protect Time Series Data While Maintaining Accurate Forecasts?"

Cameron D. Bale, Matthew J. Schneider, and Jinwook Lee.
---
Overview & Contents
---

This repository contains the code and (links to) the data used to produce the results for [this paper](https://www.researchgate.net/publication/372621568_Can_We_Protect_Time_Series_Data_While_Maintaining_Accurate_Forecasts).

`Code` contains all code files in two subdirectories (1) `Analysis`, which contains all files used to produce the results in the paper, and (2) `Exploratory`, which contains old code files no longer relevant to the project. The `Code-Directions.txt` file contains directions for running the code which we have also written below.

---

## Computational Requirements

This repository includes code files written in both Python and R, so make sure you have both installed. We used the following packages and versions:

* Python (Anaconda Distribution, Python Ver. 3.11.7):
    - bayesian-optimization (1.5.0)
    - darts (0.30.0)
    - numpy (1.26.4)
    - pandas (2.1.4)
    - pmdarima (2.0.4)
    - scipy (1.11.4)
    - sktime (0.30.1)
    - statsmodels (0.14.0)
    - torch (2.3.1)

* R (Ver. 4.1.1): 
    - CORElearn (1.57.3)
    - e1071 (1.7.14)
    - ExtDist (0.7.2)
    - forecast (8.23.0)
    - ggh4x (0.2.8)
    - ggplot2 (3.5.1)
    - ggpubr (0.6.0)
    - gratis (1.0.7)
    - plyr (1.8.9)
    - ranger (0.16.0)
    - tidytext (0.4.2)
    - tidyverse (2.0.0)
    - tsDyn (11.0.4.1)
    - tsfeatures (1.1.1)

All file paths are relative to the `Code/Analysis/` directory. When running code, please set your working directory accordingly.

The code for this project has been run on high-end windows desktops with the following specs:

* CPU: Intel i9-14900k / AMD Ryzen 9 7900x
* RAM: 64 GB
* GPU: RTX 4090

Some of the code files have been verified to run on an M1 Macbook Pro with 32GB of RAM. However, the authors provide no guarantee that *all* code files will successfully execute on Mac based machines, or on machines with less than 64GB of memory. 

An Nvidia GPU is not required, but *drastically* speeds up training of neural network models. Instructions for taking advantage of GPU compute resource are given below. High core-count CPUs are recommended as well, as portions of the code are designed to take advantage of parallel computation resources.

Running all code on a single machine could take upwards of one month. Computation time can be reduced by running the code for each data set (M3, M3_rate, M4, M4_rate) on separate machines. The code instructions are broken out accordingly.

---

## Code to Output Mapping

Please see the README.md file in the `Code` directory for detailed steps on running code files. Here, we provide a list of the code files that directly produce the results shown in the plots and tables in the paper.

***Figures***:

The `feature_analysis.R` file is used to produce the following figures (listed in order of production in the code, not appearance in the paper). The figures are saved to the `Outputs/Figures/M3/` directory:

* Figure 11 (`M3_RReliefF_Weights.pdf`)
* Figure 12 (`M3_OOB.pdf`)
* Figure 5 (`M3_RFE_selection.pdf`)
* Figure 1 (`side-by-side-series.pdf`)
* Figure 6 (`side-by-side-protected-series.pdf`)
* Figure 7 (`features-boxplot.pdf`)
* Figure 8 (`features-pca.pdf`)
* Figure 14 (`full-feature-pca.pdf`)
* Figure 13 (`cross_correlation_distributions.pdf`)

The `simulation-accuracy-privacy-weighted-euclidean.R` file is used to produce the following figures (listed in order of production in the code, not appearance in the paper). The figures are saved to the `Outputs/Figures/Simulation/` directory:

* Figure 3 (`10_simulated_series.pdf`)
* Figure 10 (`feature_distribution_simulation.pdf`)
* Figure 4 (`idr-mae-10-orig.pdf`)

The `computation_time_analysis.R` file is used to produce Figure 9 (`computation_cost_plot.pdf`).

***Tables***:

Table 1 values output from lines 836-860 from `feature_analysis.R`.

Table 2 values output from lines 1064-1187, and 1665-1831 from `simulation-accuracy-privacy-weighted-euclidean.R`.

Table 4 contains values from several code and `.csv` files. The `.csv` files are saved to `Outputs/Results/M3/Tables/`: 

* `results_computation_M3.R` produces the following:
    - `avg_accuracy_by_protection.csv` gives the original and protected mean accuracy and the percent change in mean accuracy (across all models and data sets).
* `privacy_assessment_M3.R` produces the following:
    - `overall_privacy_averages.csv` contains the average identification disclosure risk across all data sets for each privacy method.
* `forecast_privacy_assessment_M3.R` produces the following:
    - `weighted_fcast_prop_ident.csv` contains the average forecast disclosure risk across all data sets for all non-VAR-based privacy methods.

The files `results_computation_m3_rate.R`, `privacy_assessment_M3_rate.R`, and `forecast_privacy_assessment_M3_rate.R` produce analagous files for the M3-rate data and inverse rate data saved to `Outputs/Results/M3_rate/Tables/`.

Table 5 contains values produced in a similar way to the values in Table 4:

* `results_computation_M3.R` produces the following:
    - `var_avg_accuracy_by_protection.csv`: same as above but for the VAR-simulated and VAR protected lag results.
* `privacy_assessment_M3.R` produces the following:
    - `overall_privacy_averages.csv` contains the average identification disclosure risk across all data sets for each privacy method.
* `forecast_privacy_assessment_M3.R` produces the following:
    - `weighted_var_fcast_prop_ident.csv` same as above but for VAR-based privacy methods.

Table 6 contains values produced in a similar way to the values in Tables 4 and 5, but using the `results_computation_M3_rate.R`, `privacy_assessment_M3_rate.R`, and `forecast_privacy_assessment_M3_rate.R` files.

Note that for the second rows in Tables 5 and 6, the accuracy results are obtained from the model-specific *k*-nTS+ results (`averages_by_model.csv` and `rate_averages_by_model.csv`)

Tables 7, 8, and 9 (similar to Table 2) values output from lines 1064-1187, and 1665-1831 from `simulation-accuracy-privacy-weighted-euclidean.R`.

Tables 10 and 11 contain values output from lines 1064-1187, and 1665-1831 from `simulation-accuracy-privacy-weighted-euclidean-large-N.R`.

Table 12 contains results produced by `results_computation_M3.R` and saved to `average_accuracy_by_magnitude_protection.R`.

Table 13 contains results produced by `results_computation_M3.R` and `results_computation_M3_rate.R` and saved to `averages_by_model.csv`, `rate_averages_by_model.csv` and `ir_averages_by_model.csv` in the `M3/Tables/` and `M3_rate/Tables/` subdirectories.

Table 14 contains results produced by `results_computation_M3.R` and `results_computation_M3_rate.R` and saved to `averages_by_data.csv`, `rate_averages_by_data.csv` and `ir_averages_by_data.csv` in the `M3/Tables/` and `M3_rate/Tables/` subdirectories.

Table 15 contains results produced by `results_computation_M4.R`, `privacy_assessment_M4.R`, and `forecast_privacy_assessment_M4.R`, saved to `protection_avgs.csv`, `overall_privacy_averages.csv`, and `forecast_identification_probabilities.csv` in the `Outputs/Results/M4/Tables/` directory.

*********Table 16 contains the same results as Table 15 but for the M4-rate data, produced using the analagous files.

Table 17 contains values output from lines 836-860 in `feature_analysis.R`.

Table 18 contains values from the files `mean_corr_diffs.csv` and `mean_var_corr_diffs.csv`, both produced using `feature_analysis.R`.

Tables 19 and 20 contain hyperparameter values found in the code files `Final_Forecasts_M3.py`, `Final_Forecasts_M3_rate.py`, `LGBM_functions.py` and `RNN_functions.py`.