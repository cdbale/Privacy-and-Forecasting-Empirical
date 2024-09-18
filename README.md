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

Table 17 (feature_analysis.R), output from lines 836-860.

Table 18 (`mean_corr_diffs.csv` and `mean_var_corr_diffs.csv`).

