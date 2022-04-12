##### This file contains various functions used to train and forecast using
##### various forecasting models.

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
import sktime
import lightgbm as lgb
from sktime.performance_metrics.forecasting import mean_absolute_percentage_error, median_absolute_percentage_error
from data_protection_functions import *
from data_processing_functions import *

# general function for training a model and generating forecasts.
def train_and_forecast(ts_data, forecasting_model, horizon_length, last_window=None):
    """
    Performs model training and forecasting using the supplied model applied to
    the supplied time series data. Model-specific arguments have a default of
    None.

    :param ts_data: pandas dataframe containing EITHER
        - series in the rows and time periods in the columns - used with
            traditional forecasting models.
        - reduced tabular data with target feature in the last column.
    :param forecasting_model: the model to use to perform forecasts. Must have
        .fit and .predict methods.
    :param horizon_length: the number of steps ahead you with to forecast
        relative to the last point in the series.
    :param last_window: specific to models that predict using reduced data (i.e.,
        LGBM). last_window contains the final window of available data which is
        used to generate forecasts.
    :return fcasts: a pandas dataframe containing forecasts for all series,
        dimensions are transposed relative to the input data - series are in the
        columns, each row corresponds to a time period.
    """

    # define sktime relative forecasting horizon
    fh = np.arange(1, horizon_length+1)

    # list to store forecasts
    fcasts = []

    # if using an exponential smoothing model,
    if type(forecasting_model) == sktime.forecasting.exp_smoothing.ExponentialSmoothing:

        # fcasts = ts_data.apply(lambda x: forecasting_model.fit(x).predict(fh), axis=1)
        fcasts = [forecasting_model.fit(x).predict(fh) for x in ts_data]

    # if using an LGBM,
    elif type(forecasting_model) == lgb.sklearn.LGBMRegressor:

        # store the number of series
        num_series = len(last_window)

        # store the time indexes for the forecasts
        fcast_indexes = [np.arange(last_window[i].index[-1]+1, last_window[i].index[-1]+horizon_length+1) for i in range(num_series)]

        # separate training data and target
        X_train, Y_train = ts_data.iloc[:,:-1], ts_data.iloc[:,-1]

        # train global model
        forecasting_model.fit(X_train, Y_train)

        ## generate forecasts ##

        # store forecasted value based on last window of training data
        last_window = np.array(last_window)
        last_prediction = forecasting_model.predict(last_window)

        for i in range(horizon_length):
            # add latest forecast
            fcasts.append(last_prediction)
            # roll window forward one period (places first value in window at the end)
            last_window = np.roll(last_window, -1)
            # replace the end value (used to be first value) with the latest forecast
            last_window[:,-1] = last_prediction
            # forecast for next period
            last_prediction = forecasting_model.predict(last_window)

        # combine forecasts into (num_series, horizon_length) shaped array
        fcasts = np.concatenate([i.reshape(num_series, 1) for i in fcasts], axis=1)
        fcasts = [pd.Series(x) for x in fcasts]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

    return fcasts


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

    mape_global = mean_absolute_percentage_error(test_data, original_forecasts, multioutput="uniform_average", symmetric=False)
    mdape_global = median_absolute_percentage_error(test_data, original_forecasts, multioutput="uniform_average", symmetric=False)

    mape_global_protected = mean_absolute_percentage_error(test_data, protected_forecasts, multioutput="uniform_average", symmetric=False)
    mdape_global_protected = median_absolute_percentage_error(test_data, protected_forecasts, multioutput="uniform_average", symmetric=False)

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

    # mean absolute percentage error for original forecasts (without adjustment)
    mape_up = mean_absolute_percentage_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up], symmetric=False)
    mape_down = mean_absolute_percentage_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down], symmetric=False)

    # mean absolute percentage error for protected forecasts (with adjustment)
    mape_up_protected = mean_absolute_percentage_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up], symmetric=False)
    mape_down_protected = mean_absolute_percentage_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down], symmetric=False)

    # median absolute percentage error for original forecasts (without adjustment)
    mdape_up = median_absolute_percentage_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up], symmetric=False)
    mdape_down = median_absolute_percentage_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down], symmetric=False)

    # median absolute percentage error for protected forecasts (with adjustment)
    mdape_up_protected = median_absolute_percentage_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up], symmetric=False)
    mdape_down_protected = median_absolute_percentage_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down], symmetric=False)

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
        "Global MAPE, MdAPE": (mape_global, mdape_global),
        "Global Protected MAPE, MdAPE": (mape_global_protected, mdape_global_protected),
        "Original MAPE Up, Down": (mape_up, mape_down),
        "Protected MAPE Up, Down": (mape_up_protected, mape_down_protected),
        "Original MdAPE Up, Down": (mdape_up, mdape_down),
        "Protected MdAPE Up, Down": (mdape_up_protected, mdape_down_protected)
    }

    results_dict = {k: np.round(v, 4) for k, v in results_dict.items()}

    return results_dict


def full_forecast_analysis(full_data, forecasting_model, h, sp=None, seasonality_type=None, remove_seasonality=False, truncate=False, mean_normalize=False, log=False, window_length=None, coding_type=None, coding_percentage=None, num_stdev=None, epsilon=None, bias_adjust_forecasts=True):

    # create train and test data
    Y = [x.iloc[:-h] for x in full_data]
    Test = [x.iloc[-h:] for x in full_data]

    # create protected version of Y
    Y_protected = apply_data_protection(sensitive_data=Y, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon)

    transform_dict = {}
    # if reduced data is to be created,
    if window_length is not None:
        transform_dict["windows"] = {"window_length":window_length}
    # if seasonality is to be removed through pre-processing,
    elif remove_seasonality:
        transform_dict["deseasonalize"] = {"sp":sp, "seasonality_type":seasonality_type}

    # perform pre-processing, output the processed time series, the last window
    # of data if using reduction, and the last window with the trend to use when
    # reversing per-window normalization
    Y_processed, Y_last_window, Y_last_window_trend = pre_process(ts_data=Y, truncate=truncate, log=log, transform_dict=transform_dict)
    Y_protected_processed, Y_protected_last_window, Y_protected_last_window_trend = pre_process(Y_protected, truncate=truncate, log=log, transform_dict=transform_dict)

    # train forecasting model and generate forecasts
    forecasts_original = train_and_forecast(ts_data=Y_processed, forecasting_model=forecasting_model, horizon_length=h, last_window=Y_last_window)
    forecasts_protected = train_and_forecast(ts_data=Y_protected_processed, forecasting_model=forecasting_model, horizon_length=h, last_window=Y_protected_last_window)

    # post-process the forecasts
    forecasts_original = post_process(full_ts_data=Y,
                                      forecasts=forecasts_original,
                                      last_window_with_trend=Y_last_window_trend,
                                      truncate=truncate,
                                      mean_normalize=mean_normalize,
                                      log=log,
                                      bias_adjusted=bias_adjust_forecasts,
                                      transform_dict=transform_dict)

    forecasts_protected = post_process(full_ts_data=Y_protected,
                                       forecasts=forecasts_protected,
                                       last_window_with_trend=Y_protected_last_window_trend,
                                       truncate=truncate,
                                       mean_normalize=mean_normalize,
                                       log=log,
                                       bias_adjusted=bias_adjust_forecasts,
                                       transform_dict=transform_dict)

    Test = pd.DataFrame([x.reset_index(drop=True) for x in Test]).T

    results = forecast_results(test_data=Test, original_forecasts=forecasts_original, protected_forecasts=forecasts_protected)

    return results
