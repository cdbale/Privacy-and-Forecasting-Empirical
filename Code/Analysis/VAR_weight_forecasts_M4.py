from statistics import mean
import subprocess
import os
import pickle
import pandas as pd
import numpy as np
import sktime
import itertools
from sktime.forecasting.base import ForecastingHorizon
from statsmodels.tsa.vector_ar.var_model import VAR
from sktime.performance_metrics.forecasting import mean_absolute_error, median_absolute_error, mean_absolute_percentage_error, MeanAbsoluteError
from data_protection_functions import *
from data_processing_functions import *
from error_distribution_generator import *

# import original data file
data_folder = "M3/"

# forecast horizon
h = 1

# store the cleaned data file names including the baseline protected data sets
cleaned_data_path = "../../Data/Cleaned/" + data_folder
cleaned_files = os.listdir(cleaned_data_path)

# original data files and test files
h1_train_files = [x for x in cleaned_files if "_h1_train" in x and not any(y in x for y in ['k-nts', 'AN_', 'DP_', '.DS_Store'])]
h1_test_files = [x for x in cleaned_files if "_h1_test" in x]

# an protected data files
an_train_files = [x for x in cleaned_files if "_h1_train" in x and "AN_0.25" in x]
knts_train_files = [x for x in cleaned_files if "_h1_train" in x and "k-nts-plus-bounded_3-1.5" in x]

# VAR simulated files
var_file_path = "../../Outputs/VAR Simulated/M3/"
var_sim_files = os.listdir(var_file_path)
var_sim_files = [x for x in var_sim_files if "h1_train" in x and not any(y in x for y in ['rate', 'k-nts', 'AN_', 'DP_', '.DS_Store'])]

forecasts_path = "../../Outputs/Forecasts/" + data_folder
results_path = "../../Outputs/Results/" + data_folder

for train_file in h1_train_files:
    
    if "monthly" in train_file:
        sp = 12
    elif "quarterly" in train_file:
        sp = 4
    else:
        sp = 1
        
    test_file = [x for x in h1_test_files if x[:-9] in train_file]
    [test_file] = test_file
    
    an_file = [x for x in an_train_files if x[9:-9] in train_file]
    [an_file] = an_file
    
    knts_file = [x for x in knts_train_files if x[25:-9] in train_file]
    [knts_file] = knts_file
    
    sim_file = [x for x in var_sim_files if x[:-9] in train_file]
    [sim_file] = sim_file
    
    # import time series and test data
    # ignore header and skip the first row to use integers as column names
    train = pd.read_csv("../../Data/Cleaned/" + data_folder + train_file, header=None, skiprows=1)
    an_train = pd.read_csv("../../Data/Cleaned/" + data_folder + an_file, header=None, skiprows=1)
    knts_train = pd.read_csv("../../Data/Cleaned/" + data_folder + knts_file, header=None, skiprows=1)
    sim_train = pd.read_csv("../../Outputs/VAR Simulated/M3/" + sim_file, header=None, skiprows=1)
    test = pd.read_csv("../../Data/Cleaned/" + data_folder + test_file, header=None, skiprows=1).T

    # convert to a list of series, and drop missing values
    train = [x.dropna() for _, x in train.iterrows()]
    an_train = [x.dropna() for _, x in an_train.iterrows()]
    knts_train = [x.dropna() for _, x in knts_train.iterrows()]
    sim_train = [x.dropna() for _, x in sim_train.iterrows()]

    # preprocess data
    train_processed = pre_process(ts_data=train,
                                  h=1,
                                  truncate=True,
                                  log=True,
                                  mean_normalize=False,
                                  sp=sp)
    
    an_train_processed = pre_process(ts_data=an_train,
                                        h=1,
                                        truncate=True,
                                        log=True,
                                        mean_normalize=False,
                                        sp=sp)
    
    knts_train_processed = pre_process(ts_data=knts_train,
                                        h=1,
                                        truncate=True,
                                        log=True,
                                        mean_normalize=False,
                                        sp=sp)
    
    sim_train_processed = pre_process(ts_data=sim_train,
                                      h=1,
                                      truncate=True,
                                      log=True,
                                      mean_normalize=False,
                                      sp=sp)
    
    train_differenced = difference_to_stationarity(train_processed)
    an_train_differenced = difference_to_stationarity(an_train_processed)
    knts_train_differenced = difference_to_stationarity(knts_train_processed)
    sim_train_differenced = difference_to_stationarity(sim_train_processed)

    num_series = len(train_differenced)

    # get the length of each series
    lengths = [len(y) for y in train_differenced]

    # store the unique length values
    unique_lengths = np.unique(lengths)

    # store the forecasts in an array of all forecasts using the stored series indices
    full_an_forecasts = np.zeros([num_series, h])
    full_knts_forecasts = np.zeros([num_series, h])
    full_sim_forecasts = np.zeros([num_series, h])
    
    for k, l in enumerate(unique_lengths):
        
        # get the indexes of each series with the lth length
        Y_ids = np.nonzero(lengths == l)[0]

        split_ids = split(Y_ids, 5)

        for i, j in enumerate(split_ids):
            
            # store series in a list
            group = [train_differenced[m].reset_index(drop=True) for m in j]
            an_group = [an_train_differenced[m].reset_index(drop=True) for m in j]
            knts_group = [knts_train_differenced[m].reset_index(drop=True) for m in j]
            sim_group = [sim_train_differenced[m].reset_index(drop=True) for m in j]

            # convert list to TxK dataframe
            group = pd.concat(group, axis=1, ignore_index=True)
            an_group = pd.concat(an_group, axis=1, ignore_index=True)
            knts_group = pd.concat(knts_group, axis=1, ignore_index=True)
            sim_group = pd.concat(sim_group, axis=1, ignore_index=True)

            forecaster = VAR(endog=group)
            results = forecaster.fit()

            # number of lags in VAR model
            lag_order = results.k_ar
            
            # forecast simulated series and k-nTS+ series
            # generate forecasts
            if lag_order == 0:
                an_pred = np.repeat(intercepts, h, axis=1).T
                knts_pred = np.repeat(intercepts, h, axis=1).T
                sim_pred = np.repeat(intercepts, h, axis=1).T
            else:
                an_pred = results.forecast(an_group.values[-lag_order:], h)
                knts_pred = results.forecast(knts_group.values[-lag_order:], h)
                sim_pred = results.forecast(sim_group.values[-lag_order:], h)
                # store forecasts in dataframe for all series
                full_an_forecasts[j,:] = an_pred.T
                full_knts_forecasts[j,:] = knts_pred.T
                full_sim_forecasts[j,:] = sim_pred.T

    full_an_forecasts = [pd.Series(full_an_forecasts[i,:]) for i in range(num_series)]
    full_knts_forecasts = [pd.Series(full_knts_forecasts[i,:]) for i in range(num_series)]
    full_sim_forecasts = [pd.Series(full_sim_forecasts[i,:]) for i in range(num_series)]

    for i in range(num_series):
        last_time = train[i].index[-1]
        full_an_forecasts[i].index = np.arange(last_time+1, last_time+1+h)
        full_knts_forecasts[i].index = np.arange(last_time+1, last_time+1+h)
        full_sim_forecasts[i].index = np.arange(last_time+1, last_time+1+h)

    an_processed = reverse_difference_to_stationarity(h, full_an_forecasts, an_train_processed)
    knts_processed = reverse_difference_to_stationarity(h, full_knts_forecasts, knts_train_processed)
    sim_processed = reverse_difference_to_stationarity(h, full_sim_forecasts, sim_train_processed)

    # post-process the forecasts
    an_forecasts = post_process(full_ts_data=an_train,
                                forecasts=an_processed,
                                h=1,
                                truncate=True,
                                log=True,
                                mean_normalize=False,
                                sp=sp)
    
    knts_forecasts = post_process(full_ts_data=knts_train,
                                forecasts=knts_processed,
                                h=1,
                                truncate=True,
                                log=True,
                                mean_normalize=False,
                                sp=sp)
    
    # post-process the forecasts
    sim_forecasts = post_process(full_ts_data=sim_train,
                                 forecasts=sim_processed,
                                 h=1,
                                 truncate=True,
                                 log=True,
                                 mean_normalize=False,
                                 sp=sp)

    # save the forecasts to .csv file
    horizon = train_file.split("_")[-2]
    finfo = train_file[:-9]

    # save the forecasts
    an_forecasts.to_csv(forecasts_path + "VAR_h1_var-an-lag_1_" + finfo + ".csv", index=False)
    knts_forecasts.to_csv(forecasts_path + "VAR_h1_var-knts-lag_1_" + finfo + ".csv", index=False)
    sim_forecasts.to_csv(forecasts_path + "VAR_h1_var-sim-lag_1_" + finfo + ".csv", index=False)

    # calculate the average mean absolute error
    an_mae_global = pd.Series(mean_absolute_error(test, an_forecasts, multioutput="uniform_average"))
    
    knts_mae_global = pd.Series(mean_absolute_error(test, knts_forecasts, multioutput="uniform_average"))
    
    # calculate the average mean absolute error
    sim_mae_global = pd.Series(mean_absolute_error(test, sim_forecasts, multioutput="uniform_average"))

    an_mae_global.to_csv(results_path + "VAR_h1_var-an-lag_1_" + finfo + ".csv", index=False)

    # save the average mean absolute error
    knts_mae_global.to_csv(results_path + "VAR_h1_var-knts-lag_1_" + finfo + ".csv", index=False)
    
    # save the average mean absolute error
    sim_mae_global.to_csv(results_path + "VAR_h1_var-sim-lag_1_" + finfo + ".csv", index=False)

###############################################################################

og_files = [x[:-12] for x in cleaned_files if not any(y in x for y in ['_train', '_h1_', 'k-nts', 'AN_', 'DP_', '.DS_Store'])]

# for each of these, we need to extract the error distributions for all forecasting models and 
# baseline data sets

models = ["SES", "DES", "TES", "ARIMA", "VAR", "LGBM", "RNN"]

protection_methods = {"original": [""],
                      "AN": [0.25, 0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20],
                      "k-nts_": [3, 5, 7, 10, 15],
                      "k-nts-plus_": [3, 5, 7, 10, 15],
                      "k-nts-plus-bounded_": [3, 5, 7, 10, 15]}

# make column to store times for error computation and save since this file
# will be called inside the `error_distribution_generator` function

for f in og_files:
    error_distribution_generator(data_folder=data_folder,
                                  file_string=f,
                                  forecasts_path=forecasts_path, 
                                  results_path=results_path, 
                                  model_list=models, 
                                  protection_method_dict=protection_methods, 
                                  forecast_horizon="h1",
                                  track_comp_time=False,
                                  is_rate=False,
                                  inverse_rate=False)
