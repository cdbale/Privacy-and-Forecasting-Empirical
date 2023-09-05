##### This file contains various functions used to train and forecast using
##### various forecasting models.

##### Author: Cameron Bale

################################################################################

import subprocess
import pickle
import pandas as pd
import numpy as np
import sktime
import itertools
from sktime.forecasting.exp_smoothing import ExponentialSmoothing
from sktime.forecasting.base import ForecastingHorizon
from statsmodels.tsa.vector_ar.var_model import VAR
from sktime.forecasting.arima import AutoARIMA
import lightgbm as lgb
from sktime.performance_metrics.forecasting import mean_absolute_error, median_absolute_error, mean_absolute_percentage_error, MeanAbsoluteError
from data_protection_functions import *
from data_processing_functions import *
from RNN_functions import *
from LGBM_functions import *
from darts import TimeSeries

################################################################################

def generate_and_save_forecasts(train_file, test_file, forecasts_path, results_path, model, model_args, h, file_suffix):

    """

    param train_path: path to training data file.
    param test_path: path to test data file.
    param forecasts_path: path to where forecasts will be saved
    param results_path: path to where accuracy results will be saved
    param model: string model name
    param model_args: model specific arguments
    param h: forecast horizon, default is 1.
    """

    # import time series and test data
    # ignore header and skip the first row to use integers as column names
    train = pd.read_csv("../../Data/Cleaned/" + train_file, header=None, skiprows=1)
    test = pd.read_csv("../../Data/Cleaned/" + test_file, header=None, skiprows=1).T

    # convert to a list of series, and drop missing values
    train = [x.dropna() for _, x in train.iterrows()]

    # generate forecasts
    fcasts = full_forecast_analysis(Y=train,
                                    h=h,
                                    forecasting_model=model,
                                    sp=model_args["sp"],
                                    mean_normalize=model_args["mean_normalize"],
                                    options=model_args["options"],
                                    file_suffix=file_suffix)

    # save the forecasts and fitted values based on confidential data to .csv file
    horizon = train_file.split("_")[-2]
    finfo = train_file[:-9]

    # save the forecasts
    fcasts.to_csv(forecasts_path + model + "_" + horizon + "_" + finfo + ".csv", index=False)

    # calculate the average mean absolute error
    mae_global = pd.Series(mean_absolute_error(test, fcasts, multioutput="uniform_average"))

    # save the average mean absolute error
    mae_global.to_csv(results_path + model + "_" + horizon + "_" + finfo + ".csv", index=False)

    # print the average mean absolute error
    print("MAE for " + model + " on " + finfo + ": " + str(mae_global[0]))

    return None

################################################################################

def full_forecast_analysis(Y, h, forecasting_model, sp=None, mean_normalize=False, options=None, file_suffix=None):

    Y_processed = pre_process(ts_data=Y,
                              h=h,
                              mean_normalize=mean_normalize,
                              sp=sp)

    # train forecasting model and generate forecasts
    forecasts = train_and_forecast(ts_data=Y_processed,
                                   h=h,
                                   forecasting_model=forecasting_model,
                                   sp=sp,
                                   options=options,
                                   file_suffix=file_suffix)

    # post-process the forecasts
    forecasts = post_process(full_ts_data=Y,
                             forecasts=forecasts,
                             h=h,
                             mean_normalize=mean_normalize,
                             sp=sp)

    return forecasts

################################################################################

# general function for training a model and generating forecasts.
def train_and_forecast(ts_data, h, forecasting_model, sp=None, options=None, file_suffix=None):
    """
    Performs model training and forecasting using the supplied model applied to
    the supplied time series data. Model-specific arguments have a default of
    None.

    :param ts_data: pandas dataframe containing EITHER
        - series in the rows and time periods in the columns - used with
            traditional forecasting models.
        - reduced tabular data with target feature in the last column.
    :param forecasting_model: string name of the model to use to perform forecasts.
    :param horizon_length: the number of steps ahead you with to forecast
        relative to the last point in the series.
    :protection_method: string name of the type of data protection applied.
    :param last_window: specific to models that predict using reduced data (i.e.,
        LGBM). last_window contains the final window of available data which is
        used to generate forecasts.
    :return fcasts: a pandas dataframe containing forecasts for all series,
        dimensions are transposed relative to the input data - series are in the
        columns, each row corresponds to a time period.
    """

    # define sktime relative forecasting horizon
    fh = np.arange(1, h+1)

    # if using SES
    if forecasting_model == "SES":
        forecaster = ExponentialSmoothing(use_boxcox=False)
        fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]

    # if using DES
    elif forecasting_model == "DES":
        forecaster = ExponentialSmoothing(trend="additive", use_boxcox=False)
        fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]

    # if using TES
    elif forecasting_model == "TES":
        forecaster = ExponentialSmoothing(trend="additive", seasonal="additive", sp=sp, use_boxcox=False)
        fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]

    # if using ARIMA
    elif forecasting_model == "ARIMA":
        if sp > 1:
            forecaster = AutoARIMA(seasonal=True, maxiter=15, sp=sp, suppress_warnings=True)
            fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]
        else:
            forecaster = AutoARIMA(seasonal=False, maxiter=15, suppress_warnings=True)
            fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]

        # if using VAR
    elif forecasting_model == "VAR":
        fcasts = VAR_forecast(ts_data=ts_data,
                              h=h,
                              save_params=options["save_params"],
                              simulate_series=options["simulate_series"],
                              param_save_file=file_suffix)

    elif forecasting_model == "LGBM":
        lags = options['window_length']
        fcasts = LGBM_forecast(ts_data=ts_data,
                               h=h,
                               lags=lags,
                               max_samples_per_ts=options['max_samples_per_ts'])

    elif forecasting_model == "RNN":
        fcasts = RNN_forecast(ts_data=ts_data,
                              h=h,
                              input_chunk_length=options['input_chunk_length'],
                              training_length=options['training_length'],
                              max_samples_per_ts=options['max_samples_per_ts'],
                              num_ensemble_models=options['num_ensemble_models'])

    return fcasts

################################################################################

# function for forecasting with LSTM-RNN
def RNN_forecast(ts_data, h, input_chunk_length, training_length, max_samples_per_ts, num_ensemble_models, model_save_folder=None):

    series_lengths = [len(x) for x in ts_data]

    unique_lengths = np.unique(series_lengths)

    full_fcasts = {}
    full_fitted_values = {}

    for length in unique_lengths:

        tsd = [ts_data[i] for i,l in enumerate(series_lengths) if l == length]
        tsi = [i for i,l in enumerate(series_lengths) if l == length]

        num_series = len(tsd)

        # convert to type np.float32 to speed up training
        processed = [x.astype(np.float32) for x in tsd]

        # convert index to a RangeIndex
        for i in range(len(processed)):
            processed[i].index = pd.RangeIndex(start=0, stop=processed[i].shape[0])

        # create and remove validation set from processed
        validation = [x.iloc[-h:] for x in processed]
        validation = pd.DataFrame([x.reset_index(drop=True) for x in validation]).T
        processed_noval = [x.iloc[:-h] for x in processed]

        # convert to TimeSeries objects for RNN model
        processed = [TimeSeries.from_series(x) for x in processed]
        processed_noval = [TimeSeries.from_series(x) for x in processed_noval]

        best_params = optimize_RNN(train_data=processed_noval,
                                   validation_data=validation,
                                   h=h,
                                   num_ensemble_models=num_ensemble_models,
                                   input_chunk_length=input_chunk_length,
                                   training_length=training_length,
                                   max_samples_per_ts=max_samples_per_ts)

        fcasts = train_RNN(train_data=processed,
                           h=h,
                           num_ensemble_models=num_ensemble_models,
                           input_chunk_length=input_chunk_length,
                           training_length=training_length,
                           max_samples_per_ts=max_samples_per_ts,
                           learning_rate_=best_params['params']['learning_rate_'],
                           n_rnn_layers_=int(best_params['params']['n_rnn_layers_']),
                           hidden_dim_=int(best_params['params']['hidden_dim_']),
                           batch_size_=int(best_params['params']['batch_size_']),
                           n_epochs_=int(best_params['params']['n_epochs_']),
                           dropout_=best_params['params']['dropout_'],
                           L2_penalty_=best_params['params']['L2_penalty_'])

        fcasts = [pd.Series(fcasts.iloc[:,i]) for i in range(fcasts.shape[1])]

        fcast_indexes = [np.arange(tsd[i].index[-1]+1, tsd[i].index[-1]+h+1) for i in range(num_series)]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

        # store sub-group forecasts and fitted values
        for i,j in enumerate(tsi):
            full_fcasts[j] = fcasts[i]

    return full_fcasts

# function for forecasting with LGBM
def LGBM_forecast(ts_data, h, lags, max_samples_per_ts):

    series_lengths = [len(x) for x in ts_data]

    unique_lengths = np.unique(series_lengths)

    full_fcasts = {}

    for length in unique_lengths:

        tsd = [ts_data[i] for i,l in enumerate(series_lengths) if l == length]
        tsi = [i for i,l in enumerate(series_lengths) if l == length]

        # store number of series
        num_series = len(tsd)

        # convert to type np.float32 to speed up training
        processed = [x.astype(np.float32) for x in tsd]

        # convert index to a RangeIndex
        for i in range(len(processed)):
            processed[i].index = pd.RangeIndex(start=0, stop=processed[i].shape[0])

        # create and remove validation set from processed
        validation = [x.iloc[-h:] for x in processed]
        validation = pd.DataFrame([x.reset_index(drop=True) for x in validation]).T
        processed_noval = [x.iloc[:-h] for x in processed]

        # convert to TimeSeries objects for RNN model
        processed = [TimeSeries.from_series(x) for x in processed]
        processed_noval = [TimeSeries.from_series(x) for x in processed_noval]

        best_params = optimize_LGBM(train_data=processed_noval,
                                    validation_data=validation,
                                    h=h,
                                    lags=lags,
                                    max_samples_per_ts=max_samples_per_ts)

        fcasts = train_LGBM(train_data=processed,
                                   h=h,
                                   lags=lags,
                                   max_samples_per_ts=max_samples_per_ts,
                                   learning_rate_=best_params['params']['learning_rate_'],
                                   num_boost_rounds_=int(best_params['params']['num_boost_rounds_']),
                                   num_leaves_=int(best_params['params']['num_leaves_']),
                                   bagging_freq_=best_params['params']['bagging_freq_'],
                                   bagging_frac_=best_params['params']['bagging_frac_'],
                                   lambda_l2_=best_params['params']['lambda_l2_'],
                                   min_data_in_leaf_=int(best_params['params']['min_data_in_leaf_']))

        fcasts = [pd.Series(fcasts.iloc[:,i]) for i in range(fcasts.shape[1])]

        fcast_indexes = [np.arange(tsd[i].index[-1]+1, tsd[i].index[-1]+h+1) for i in range(num_series)]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

        # store sub-group forecasts in matrix of all forecasts
        for i,j in enumerate(tsi):
            full_fcasts[j] = fcasts[i]

    return full_fcasts

def VAR_forecast(ts_data, h, save_params, simulate_series, param_save_file=None):

    ts = difference_to_stationarity(ts_data)

    num_series = len(ts)

    # get the length of each series
    lengths = [len(y) for y in ts]

    # store the unique length values
    unique_lengths = np.unique(lengths)

    # store the forecasts in an array of all forecasts using the stored series indices
    full_forecasts = np.zeros([num_series, h])

    for k, l in enumerate(unique_lengths):

        # get the indexes of each series with the lth length
        Y_ids = np.nonzero(lengths == l)[0]

        split_ids = split(Y_ids, 5)

        for i, j in enumerate(split_ids):

            # store series in a list
            group = [ts[m].reset_index(drop=True) for m in j]

            # convert list to TxK dataframe
            group = pd.concat(group, axis=1, ignore_index=True)

            forecaster = VAR(endog=group)
            results = forecaster.fit()

            # number of lags in VAR model
            lag_order = results.k_ar

            # extract intercept coefficients
            if save_params:
                intercepts = results.coefs_exog
                pd_intercepts = pd.DataFrame(intercepts)
                newpath = "../../Outputs/VAR Weights/" + param_save_file
                if not os.path.exists(newpath):
                    os.makedirs(newpath)
                pd_intercepts.to_csv(newpath + "intercepts_" + str(k) + "_" + str(i) + ".csv", index=False)

                # extract lag coefficients
                lag_coefs = pd.concat([pd.DataFrame(results.coefs[:,:,i]) for i in range(results.coefs.shape[2])], axis=1)
                lag_coefs.to_csv(newpath + "lag_coefs_" + str(k) + "_" + str(i) + ".csv", index=False)
            
            if simulate_series:
                print('none')

            # generate forecasts
            if lag_order == 0:
                y_pred = np.repeat(intercepts, h, axis=1).T
            else:
                y_pred = results.forecast(group.values[-lag_order:], h)

            # store forecasts in dataframe for all series
            full_forecasts[j,:] = y_pred.T

    full_forecasts = [pd.Series(full_forecasts[i,:]) for i in range(num_series)]

    for i in range(num_series):
        last_time = ts[i].index[-1]
        full_forecasts[i].index = np.arange(last_time+1, last_time+1+h)

    processed = reverse_difference_to_stationarity(h, full_forecasts, ts_data)

    return processed



# function to calculate metrics for the original and protected forecasts
def forecast_results(test_data, original_forecasts, protected_forecasts):
    """
    Compares the accuracy of forecasts from the original confidential data,
    and forecasts from the protected data.

    param: test_data: (horizon, num_series) shaped dataframe with actual test values
    param: original_forecasts: (horizon, num_series) shaped dataframe with values
            forecasted based on the confidential training data
    param: protected_forecasts: (horizon, num_series) shaped dataframe with values
            forecasted based on the protected training data
    return: results_dict: Returns a dictionary of tuples containing various comparisons.
             The first and second values of each tuple are based on MAPE and MdAPE, respectively.
    """

    ############## Comparison 1 ###############
    ##### Global MAPE and MdAPE

    mae_global = mean_absolute_error(test_data, original_forecasts, multioutput="uniform_average")
    mdae_global = median_absolute_error(test_data, original_forecasts, multioutput="uniform_average")

    mae_global_protected = mean_absolute_error(test_data, protected_forecasts, multioutput="uniform_average")
    mdae_global_protected = median_absolute_error(test_data, protected_forecasts, multioutput="uniform_average")

    ############################################################################

    ############## Comparison 2 ###############
    ##### The MAPE and MdAPE for upward vs. downward adjusted forecasts

    # boolean values for whether a forecasted point was adjusted up or down after protection
    adjusted_up = original_forecasts < protected_forecasts
    adjusted_up = pd.concat([row for i, row in adjusted_up.iterrows()], ignore_index=True)
    adjusted_down = original_forecasts > protected_forecasts
    adjusted_down = pd.concat([row for i, row in adjusted_down.iterrows()], ignore_index=True)

    # collapse forecasts and test data for indexing using adjustment directions
    collapsed_original = pd.concat([row for i, row in original_forecasts.iterrows()], ignore_index=True)
    collapsed_protected = pd.concat([row for i, row in protected_forecasts.iterrows()], ignore_index=True)
    collapsed_test = pd.concat([row for i, row in test_data.iterrows()], ignore_index=True)

    # mean absolute error for original forecasts (without adjustment)
    mae_up = mean_absolute_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up])
    mae_down = mean_absolute_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down])

    # mean absolute percentage error for protected forecasts (with adjustment)
    mae_up_protected = mean_absolute_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up])
    mae_down_protected = mean_absolute_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down])

    results_dict = {
        "Original MAE Up": mae_up,
        "Protected MAE Up": mae_up_protected,
        "Original MAE Down": mae_down,
        "Protected MAE Down": mae_down_protected,
        "Original MdAE Up": mdae_up,
        "Protected MdAE Up": mdae_up_protected,
        "Original MdAE Down": mdae_down,
        "Protected MdAE Down": mdae_down_protected,
    }

    results_dict = {k: np.round(v, 4) for k, v in results_dict.items()}

    return results_dict
