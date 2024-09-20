# Code Directions

This .README file contains instructions for running the project code files. We have a specific structure (order) in which files are run for each data set (M3, M3-rate, M4, M4-rate). We have designed the structure to be consistent across the files for each data set, with the hope that interested parties do not need to run *all* files sequentially, *i.e.*, the files for M3 could be run on a separate machine from the files for M3-rate, and so on, in order to reduce the time to obtain all results.

We have given detailed descriptions for the M3 files, noting that the purposes and naming conventions for the files are consistent across data sets (*e.g.*, the `Forecast_for_Feature_Selection_*.py` file, where `*` is replaced with a data identifier, serves the same purpose for each data set).

We have made a [.zip file](https://byu.box.com/s/958npasrq56vyjag00qjrcn6g2wuns65) with all intermediate (and final) results available at the following link for those who wish to verify certain steps in the analysis without running *all* of the code.

Finally, we note that some deviations in numerical results are expected as a result of the randomness of the RNN, *k*-nTS, *k*-nTS+, and VAR protection processes, and random forest-based feature selection. This can (with small probability) result in significantly worse forecast accuracy for a given model/protection method combination. Re-running the code should rectify this issue. Overall, the small differences in numerical results do not in any way affect the substantive conclusions of the paper.

## Code For Simulations

R files `simulation-accuracy-privacy-weighted-euclidean.R` and `simulation-accuracy-privacy-weighted-euclidean-large-N.R` are used to obtain the simulation results. Simulation Plots used in the paper will be saved to the `Outputs/Figures/Simulation/` folder. The code in the simulation files was written to work with the functions defined in the simulation files. Some of these functions (particularly the knts_alg() function) were re-written in the `knts_helper_functions.R` file to reduce execution time when running on larger data sets in later code files.

Note: due to the randomness of the time series generation and $k$-nTS+ protection processes, the numerical results will vary from run to run. However, the substantive conclusions mentioned in
the paper remain the same.

## Code For Analyzing M3 Data

Create a `Data` folder in the main repository directory (same directory as the `Code` folder).

Create an `M3` folder in the `Data` directory.

Download the M3 data sets (-monthly, -other, -quarterly, -yearly) and place them in the `M3` folder created as a subdirectory of the `Data` folder. The data sets can be found in `.csv` format at the following link: https://drive.google.com/drive/folders/1VG_G2vethsJQLRFv1uNuPZFYe0K-Y_ku?usp=sharing.

All python and R code files are found in the `Code/Analysis` subdirectory. Python files can be run using a command prompt (we used Anaconda Powershell Prompt).

1. Run `Data_Cleaning_M3.py`. This creates the train and test data for the original M3 data set, and stores it in the `/Cleaned/M3/` subdirectory of the `Data` folder.
	Note that there are four files for each data frequency/domain: two training files (one each for forecasting the last and second to last points of each time series) and two test data files containing the actual data for the forecasted periods.

2. Run `Create_AN_DP_M3.py`. This creates protected versions of each of the M3 training data files using additive noise or differential privacy for the parameter values in the paper. Note that this file also creates the file `M3_computation_time.csv` which tracks the computation time of the various steps involved in the k-nTS+ protection process. This file is found at `/Data/Computation_Time/M3_computation_time.csv`.

3. Run `Forecast_for_Feature_Selection_M3.py`. This generates forecasts and measures the series level absolute errors for the original and baseline protected data sets for the last time period assumed available to
	the data owner (h2). The forecast errors will be predicted using the corresponding time series features in the feature selection methodology in *k*-nTS+. The file `Forecast_for_Feature_Selection_M3.py` has a dictionary `forecasting_models` where the various forecasting models and some of their arguments/options are specified. The `use_gpu` option for the RNN model should be set to `True` only if you have an Nvidia gpu with the cuda (https://developer.nvidia.com/cuda-toolkit) and torch (https://pytorch.org/get-started/locally/) packages installed. The default for `use_gpu` is set to `False` (the CPU is used to train the RNN model(s)). **In our opinion, it is worth it to set up the CUDA toolkit - it drastically reduces the training time of the RNN models**.
	
4. Run `k-nts_M3.R` which protects the M3 data using *k*-nTS based on manual feature selection. See the paper for discussion of the chosen features.

5. Run `original_and_baseline_tsfeatures_extraction_M3.R`. This R script calculates the time series features for the unprotectetd and baseline protected M3 data sets. These features will be combined with the forecast errors from step 3. to be used in the machine learning-based feature selection method in *k*-nTS+. Note that all features are saved to the `Data/Features/` folder.

6. Run `k-nts-plus_M3.R` which protects the M3 data using the *k*-nTS+ methodology based on the machine-learning based feature selection method. The runtime of this file in particular benefits from high core-count CPUs as the random forest models are trained in parallel. You shouldn't have to perform any extra steps, as the `ranger` package automatically utilizes parallel cores, when available.

7. Run `k-nts_plus_M3_bounded_M.R` which creates modified versions of the *k*-nTS+ $(k = 3)$ protected data sets by bounding the differences between the protected and unprotected values, as discussed in the paper.

8. Run `Final_Forecasts_M3.py`. This generates forecasts and measures the series level absolute errors for the final unprotected and protected data sets.

9. Run `VAR_weight_forecasts_M3.py` to generate VAR model forecasts based on simulated VAR series and protected lagged values.

10. Run `updated_tsfeatures_extraction_M3.R` to extract time series features for the final unprotected and protected data sets.

11. Run `privacy_assessment_M3.R` to perform the identification disclosure simulation. Results are saved in a `.csv` file `overall_privacy_averages.csv` in the `Outputs/Results/M3/Tables/` directory.

12. Run `forecast_privacy_assessment_M3.R` to perform the forecast identification disclosure simulation. Results are saved as `weighted_fcast_prop_ident.csv` (for all non-VAR-based methods) and `weighted_var_fcast_prop_ident.csv` (for VAR-based methods). These files are saved to the same directory listed in Step 11.

13. Run `magnitude_computation_M3.R` to compute the magnitude of time series based on the test data (actual value from forecasted time period). Results are saved in the `Data/Magnitudes/` subdirectory.

14. Run `results_computation_M3.R` to compute the accuracy results. Results are saved to several files in the `Outputs/Results/M3/Tables` subdirectory.
	- `avg_accuracy_by_protection.csv`: gives the original and protected mean accuracy and the percent change in mean accuracy (across all models and data sets).
	- `var_avg_accuracy_by_protection.csv`: same as above but for the VAR-simulated and VAR protected lag results.
	- `avg_accuracy_by_magnitude_protection.csv`: mean accuracy results broken down by large vs. small magnitude time series for each model and data set.
	- `var_avg_accuracy_by_magnitude_protection.csv`: same as above but for the VAR-simulated and VAR protected lag results.
	- `averages_by_model.csv`: mean accuracy results under bounded k-nTS+ (k=3, M=1.5) across all data sets for each forecasting model
	- `averages_by_data.csv`: mean accuracy results for each data subset and privacy method

15. Run `computation_time_analysis.R` to analyze the computation time of the k-nTS+ process. The regression results used in the paper are printed to the R console, and the plot (Figure 9) is saved to the `Outputs/Figures/M3/` directory.

*The code for the following M3-rate, M4, and M4-rate data sets follows a similar structure to what we describe above. For the explanation of the purpose of any particular file, please see the above explanations*.

## Code For Analyzing M3-Rate Data

1. Run `Data_Cleaning_M3_rate.py`.

2. Run `Create_AN_DP_M3_rate.py`.

3. Run `Forecast_for_Feature_Selection_M3_rate.py`.

4. Run `k-nts_M3_rate.R`.

5. Run `original_and_baseline_tsfeatures_extraction_M3_rate.R`.

6. Run `k-nts-plus_M3_rate.R`.

7. Run `k-nts_plus_M3_rate_bounded_M.R`.

8. Run `Final_Forecasts_M3_rate.py`.

9. Run `VAR_weight_forecasts_M3_rate.py`.

10. Run `updated_tsfeatures_extraction_M3_rate.R`.

11. Run `privacy_assessment_M3_rate.R`.

12. Run `forecast_privacy_assessment_M3_rate.R`.

13. Run `results_computation_M3_rate.R`.

## Code For Analyzing M4 Data ##

Download the M4 data sets. They can be found in `.csv` format at the following link: https://drive.google.com/drive/folders/1m84cDf9IzF4sTtvPBvvDKi-GEAaPfw-7?usp=sharing. 

Create an folder `M4` inside the `Data` directory of the repository. Place the `Train` and `Test` folders from the above link in the `M4` folder.

1. Run `Data_Cleaning_M4.py`.

2. Run `Create_DP_M4.py`.

3. Run `Forecast_for_Feature_Selection_M4.py`.

4. Run `original_and_baseline_tsfeatures_extraction_M4.R`.

5. Run `k-nts_plus_M4.R`.

6. Run `k-nts_plus_M4_bounded_M.R`.

7. Run `Final_Forecasts_M4.py`.

8. Run `privacy_assessment_M4.R`.

9. Run `forecast_privacy_assessment_M4.R`.

10. Run `results_computation_M4.R`.

## Code For Analyzing M4-Rate Data ##

1. Run `Data_Cleaning_M4_rate.py`.

2. Run `Create_DP_M4_rate.py`.

3. Run `Forecast_for_Feature_Selection_M4_rate.py`.

4. Run `original_and_baseline_tsfeatures_extraction_M4_rate.R`.

5. Run `k-nts_plus_M4_rate.R`.

6. Run `k-nts_plus_M4_rate_bounded_M.R`.

7. Run `Final_Forecasts_M4_rate.py`.

8. Run `privacy_assessment_M4_rate.R`.

9. Run `forecast_privacy_assessment_M4_rate.R`.

10. Run `results_computation_M4_rate.R`.