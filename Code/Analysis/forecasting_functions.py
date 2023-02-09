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
# from sktime.forecasting.var import VAR
from statsmodels.tsa.vector_ar.var_model import VAR
from sktime.forecasting.arima import AutoARIMA
from sktime.forecasting.model_selection import ForecastingGridSearchCV, ExpandingWindowSplitter
from sktime.forecasting.compose import make_reduction
import lightgbm as lgb
from sktime.performance_metrics.forecasting import mean_absolute_error, median_absolute_error, mean_absolute_percentage_error, MeanAbsoluteError
from data_protection_functions import *
from data_processing_functions import *
from RNN_functions import *
from LGBM_functions import *
from darts import TimeSeries

# function for forecasting with LSTM-RNN
def RNN_forecast(ts_data, h, input_chunk_length, training_length, max_samples_per_ts, num_ensemble_models, model_save_folder=None):

    series_lengths = [len(x) for x in ts_data]

    unique_lengths = np.unique(series_lengths)

    full_fcasts = {}

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
                           L2_penalty_=best_params['params']['L2_penalty_'],
                           save_models=True,
                           model_save_folder=model_save_folder+"_"+str(length))

        fcasts = [pd.Series(fcasts.iloc[:,i]) for i in range(fcasts.shape[1])]

        fcast_indexes = [np.arange(tsd[i].index[-1]+1, tsd[i].index[-1]+h+1) for i in range(num_series)]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

        # store sub-group forecasts in matrix of all forecasts
        for i,j in enumerate(tsi):
            full_fcasts[j] = fcasts[i]

    fcasts = [full_fcasts[i] for i in range(len(ts_data))]

    return fcasts

# function for forecasting with LGBM
def LGBM_forecast(ts_data, h, lags, max_samples_per_ts, model_save_folder=None):

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
                            min_data_in_leaf_=int(best_params['params']['min_data_in_leaf_']),
                            save_models=True,
                            model_save_folder=model_save_folder+"_"+str(length))

        fcasts = [pd.Series(fcasts.iloc[:,i]) for i in range(fcasts.shape[1])]

        fcast_indexes = [np.arange(tsd[i].index[-1]+1, tsd[i].index[-1]+h+1) for i in range(num_series)]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

        # store sub-group forecasts in matrix of all forecasts
        for i,j in enumerate(tsi):
            full_fcasts[j] = fcasts[i]

    fcasts = [full_fcasts[i] for i in range(len(ts_data))]

    return fcasts

# splitting function used in VAR forecast
def split(a, n):
    k, m = divmod(len(a), n)
    return (a[i*n+min(i, m):(i+1)*n+min(i+1, m)] for i in range(k))

def VAR_forecast(ts_data, h, param_save_folder=None):

    num_series = len(ts_data)

    # get the length of each series
    lengths = [len(y) for y in ts_data]

    # store the unique length values
    unique_lengths = np.unique(lengths)

    # store the forecasts in an array of all forecasts using the stored series indices
    full_forecasts = np.zeros([num_series, h])
    full_fitted_values = []

    for k, l in enumerate(unique_lengths):

        # get the indexes of each series with the lth length
        Y_ids = np.nonzero(lengths == l)[0]

        split_ids = split(Y_ids, 5)

        for i, j in enumerate(split_ids):

            # store series in a list
            group = [ts_data[m].reset_index(drop=True) for m in j]

            # convert list to TxK dataframe
            group = pd.concat(group, axis=1, ignore_index=True)

            ####################################################

            forecaster = VAR(endog=group)
            results = forecaster.fit()

            # number of lags in VAR model
            lag_order = results.k_ar

            fv = results.fittedvalues.T

            fv = pd.concat([group.iloc[0:lag_order,:].T, fv], axis=1, ignore_index=True)

            for i in range(fv.shape[0]):
                full_fitted_values.append(fv.iloc[i,:])

            # extract intercept coefficients
            intercepts = results.coefs_exog
            pd_intercepts = pd.DataFrame(intercepts)
            newpath = "../../Outputs/VAR Params/" + param_save_folder + "/"
            if not os.path.exists(newpath):
                os.makedirs(newpath)
            pd_intercepts.to_csv(newpath + "intercepts_" + str(k) + "_" + str(i) + ".csv", index=False)

            # extract lag coefficients
            lag_coefs = pd.concat([pd.DataFrame(results.coefs[:,:,i]) for i in range(results.coefs.shape[2])], axis=1)
            lag_coefs.to_csv(newpath + "lag_coefs_" + str(k) + "_" + str(i) + ".csv", index=False)

            # generate forecasts
            if lag_order == 0:
                y_pred = np.repeat(intercepts, h, axis=1).T
            else:
                y_pred = results.forecast(group.values[-lag_order:], h)

            # store forecasts in dataframe for all series
            full_forecasts[j,:] = y_pred.T

    full_forecasts = [pd.Series(full_forecasts[i,:]) for i in range(num_series)]

    for i in range(num_series):
        last_time = ts_data[i].index[-1]
        full_forecasts[i].index = np.arange(last_time+1, last_time+1+h)

    return full_forecasts, full_fitted_values

# general function for training a model and generating forecasts.
def train_and_forecast(ts_data, h, target_forecast_period, forecasting_model, sp=None, param_grid=None, last_window=None, options=None, metadata=None):
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

    # lists to store forecasts and fitted values
    fcasts = []
    fvals = []

    # if using SES
    if forecasting_model == "SES":
        forecaster = ExponentialSmoothing(use_boxcox=False)
        for x in ts_data:
            # in sample forecast horizon
            in_sample_fh = ForecastingHorizon(x.index, is_relative=False)
            forecaster.fit(x)
            fcasts.append(forecaster.predict(fh))
            fvals.append(forecaster.predict(in_sample_fh))
    # if using DES
    elif forecasting_model == "DES":
        forecaster = ExponentialSmoothing(trend="additive", use_boxcox=False)
        for x in ts_data:
            # in sample forecast horizon
            in_sample_fh = ForecastingHorizon(x.index, is_relative=False)
            forecaster.fit(x)
            fcasts.append(forecaster.predict(fh))
            fvals.append(forecaster.predict(in_sample_fh))
    # if using TES
    elif forecasting_model == "TES":
        forecaster = ExponentialSmoothing(trend="additive", seasonal="additive", sp=sp, use_boxcox=False)
        for x in ts_data:
            # in sample forecast horizon
            in_sample_fh = ForecastingHorizon(x.index, is_relative=False)
            forecaster.fit(x)
            fcasts.append(forecaster.predict(fh))
            fvals.append(forecaster.predict(in_sample_fh))
    # if using VAR
    elif forecasting_model == "VAR":
        fcasts, fvals = VAR_forecast(ts_data=ts_data, h=h,
                                     param_save_folder="h_" + str(h) + "_target_period" + target_forecast_period + "_" + metadata['protection_method'] + "_" + str(metadata['protection_parameter']))
    # if using ARIMA
    elif forecasting_model == "ARIMA":
        forecaster = AutoARIMA(seasonal=True, maxiter=25, sp=sp, suppress_warnings=True)
        for x in ts_data:
            # in sample forecast horizon
            in_sample_fh = ForecastingHorizon(x.index, is_relative=False)
            forecaster.fit(x)
            fcasts.append(forecaster.predict(fh))
            fv = forecaster.predict(in_sample_fh)
            fv = fv.fillna(0)
            fvals.append(fv)
    elif forecasting_model == "Multivariate_LGBM":
        # num_series = len(last_window)
        # best_params = multivariate_lgbm_cv(ts_data=ts_data, param_grid=param_grid)
        # fcasts = multivariate_lgbm_forecast(ts_data=ts_data, h=horizon_length, last_window=last_window, num_series=num_series, best_params=best_params)
        lags = options['window_length']
        fcasts = LGBM_forecast(ts_data=ts_data,
                               h=h,
                               lags=lags,
                               max_samples_per_ts=options['max_samples_per_ts'],
                               model_save_folder="h_" + str(horizon_length) + "_target_period" + target_forecast_period + "_" + metadata['protection_method'] + "_" + str(metadata['protection_parameter']))
    elif forecasting_model == "RNN":
        fcasts = RNN_forecast(ts_data=ts_data,
                              h=h,
                              input_chunk_length=options['input_chunk_length'],
                              training_length=options['training_length'],
                              max_samples_per_ts=options['max_samples_per_ts'],
                              num_ensemble_models=options['num_ensemble_models'],
                              model_save_folder="h_" + str(horizon_length) + "_target_period" + target_forecast_period + metadata['protection_method'] + "_" + str(metadata['protection_parameter']))

    return fcasts, fvals


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

    # # median absolute error for original forecasts (without adjustment)
    # mdae_up = median_absolute_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up])
    # mdae_down = median_absolute_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down])
    #
    # # median absolute error for protected forecasts (with adjustment)
    # mdae_up_protected = median_absolute_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up])
    # mdae_down_protected = median_absolute_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down])

    ############################################################################

    # # point level absolute forecast error using original data
    # absolute_error = np.absolute(test_data - original_forecasts)
    # absolute_error = pd.concat([row for i, row in absolute_error.iterrows()])

    # # point level absolute forecast error using protected data
    # protected_absolute_error = np.absolute(test_data - protected_forecasts)
    # protected_absolute_error = pd.concat([row for i, row in protected_absolute_error.iterrows()])

    # # average absolute error for upward adjusted points prior to adjustment
    # avg_up_prior = np.mean(absolute_error[adjusted_up])
    # # average absolute error for downward adjusted points prior to adjustment
    # avg_down_prior = np.mean(absolute_error[adjusted_down])
    #
    # # average absolute error for upward adjusted points after adjustment
    # avg_up_post = np.mean(protected_absolute_error[adjusted_up])
    # # average absolute error for downward adjusted points after adjustment
    # avg_down_post = np.mean(protected_absolute_error[adjusted_down])
    #
    # ########## Metric Calculations ##########
    # # calculate series level forecast accuracies for confidential training data
    # local_mae = mean_absolute_error(test_data, original_forecasts, multioutput="raw_values")
    # local_rmse = mean_squared_error(test_data, original_forecasts, multioutput="raw_values", square_root=True)
    #
    # # calculate series level forecast accuracies for protected training data
    # protected_local_mae = mean_absolute_error(test_data, protected_forecasts, multioutput="raw_values")
    # protected_local_rmse = mean_squared_error(test_data, protected_forecasts, multioutput="raw_values", square_root=True)
    #
    # # calculate global forecast accuracies for confidential training data
    # global_mae = mean_absolute_error(test_data, original_forecasts, multioutput="uniform_average")
    # global_rmse = mean_squared_error(test_data, original_forecasts, multioutput="uniform_average", square_root=True)
    #
    # # calculate global forecast accuracy for protected training data
    # protected_global_mae = mean_absolute_error(test_data, protected_forecasts, multioutput="uniform_average")
    # protected_global_rmse = mean_squared_error(test_data, protected_forecasts, multioutput="uniform_average", square_root=True)
    #
    # ########## Comparing Accuracies Before and After Protection ##########
    #
    # ## calculated tuples correspond to (MAE, RMSE) comparisons ##
    #
    # # percentage of series for which forecast accuracy improved under protection
    # local_percent_improved = (np.mean(local_mae-protected_local_mae > 0.0),
    #                           np.mean(local_rmse-protected_local_rmse > 0.0))
    #
    # # percentage of series for which forecast accuracy worsened under protection
    # local_percent_reduced = (np.mean(local_mae-protected_local_mae < 0.0),
    #                           np.mean(local_rmse-protected_local_rmse < 0.0))
    #
    # # percentage of series for which forecast accuracy stayed the same under protection
    # # this is really only applicable to the SES model
    # local_percent_equal = (np.mean(local_mae-protected_local_mae == 0.0),
    #                        np.mean(local_rmse-protected_local_rmse == 0.0))
    #
    # # percentage change in global accuracy
    # percent_change_mean_accuracy = ((global_mae-protected_global_mae)/global_mae,
    #                                 (global_rmse-protected_global_rmse)/global_rmse)
    #
    # percent_change_median_accuracy = ((np.median(local_mae)-np.median(protected_local_mae))/np.median(local_mae),
    #                                   (np.median(local_rmse)-np.median(protected_local_rmse))/np.median(local_rmse))
    #
    #
    # # average absolute error for upward adjusted points prior to adjustment
    # avg_up_prior = np.mean(absolute_error[adjusted_up])
    # # average absolute error for downward adjusted points prior to adjustment
    # avg_down_prior = np.mean(absolute_error[adjusted_down])
    #
    # # average absolute error for upward adjusted points after adjustment
    # avg_up_post = np.mean(protected_absolute_error[adjusted_up])
    # # average absolute error for downward adjusted points after adjustment
    # avg_down_post = np.mean(protected_absolute_error[adjusted_down])

    # results_dict = {
    #     "Mean Accuracies": (global_mae, global_rmse),
    #     "Protected Mean Accuracies:": (protected_global_mae, protected_global_rmse),
    #     "% Change Mean accuracy:": percent_change_mean_accuracy,
    #     "% Change Median accuracy:": percent_change_median_accuracy,
    #     "% Forecasted Points adjusted downward:": percent_downward,
    #     "% Forecasted Points adjusted upward:": percent_upward,
    #     "% Series with improved accuracy:": local_percent_improved,
    #     "% Series with reduced accuracy:": local_percent_reduced,
    #     "Original Mean Absolute Error Upward Adjusted:": avg_up_prior,
    #     "Original Mean Absolute Error Downward Adjusted:": avg_down_prior,
    #     "Protected Mean Absolute Error Upward Adjusted:": avg_up_post,
    #     "Protected Mean Absolute Error Downward Adjusted:": avg_down_post
    #     # "% Series with unchanged accuracy:": local_percent_equal,
    # }

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

def full_forecast_analysis(Y, h, target_forecast_period, forecasting_model, make_stationary=False, seasonality_type=None, sp=None, remove_seasonality=False, mean_normalize=False, detrend=False, log=False, param_grid=None, options=None, metadata=None):

    transform_dict = {}

    # if seasonality is to be removed through pre-processing,
    if remove_seasonality:
        transform_dict["deseasonalize"] = {"sp":sp, "seasonality_type":seasonality_type}

    Y_processed, Y_last_window, Y_last_window_trend, pre_detrend = pre_process(ts_data=Y,
                                                                               h=h,
                                                                               mean_normalize=mean_normalize,
                                                                               log=log,
                                                                               detrend=detrend,
                                                                               make_stationary=make_stationary,
                                                                               sp=sp,
                                                                               transform_dict=transform_dict)

    # train forecasting model and generate forecasts
    forecasts, fvals = train_and_forecast(ts_data=Y_processed,
                                          h=h,
                                          target_forecast_period=target_forecast_period,
                                          forecasting_model=forecasting_model,
                                          sp=sp,
                                          last_window=Y_last_window,
                                          param_grid=param_grid,
                                          options=options,
                                          metadata=metadata)

    # post-process the forecasts
    forecasts = post_process(full_ts_data=Y,
                             forecasts=forecasts,
                             h=h,
                             target_forecast_period=target_forecast_period,
                             last_window_with_trend=Y_last_window_trend,
                             pre_detrend=pre_detrend,
                             mean_normalize=mean_normalize,
                             detrend=detrend,
                             log=log,
                             make_stationary=make_stationary,
                             sp=sp,
                             transform_dict=transform_dict)

    # post-process the fitted values
    fvals = post_process(full_ts_data=Y,
                         forecasts=fvals,
                         h=h,
                         target_forecast_period=target_forecast_period,
                         last_window_with_trend=Y_last_window_trend,
                         pre_detrend=pre_detrend,
                         mean_normalize=mean_normalize,
                         detrend=detrend,
                         log=log,
                         make_stationary=make_stationary,
                         sp=sp,
                         is_fitted=True,
                         transform_dict=transform_dict)

    return forecasts, fvals
