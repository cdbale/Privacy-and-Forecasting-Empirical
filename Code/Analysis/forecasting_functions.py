##### This file contains various functions used to train and forecast using
##### various forecasting models.

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
import sktime
from sktime.forecasting.exp_smoothing import ExponentialSmoothing
from sktime.forecasting.var import VAR
from sktime.forecasting.arima import AutoARIMA
from sktime.forecasting.model_selection import ForecastingGridSearchCV, ExpandingWindowSplitter
from sktime.forecasting.compose import make_reduction
import lightgbm as lgb
from sktime.performance_metrics.forecasting import mean_absolute_error, median_absolute_error, mean_absolute_percentage_error, MeanAbsoluteError
from data_protection_functions import *
from data_processing_functions import *


# function to forecast with a univariate LGBM
def univariate_lgbm_forecast(ts_data, h, param_grid):
    fcasts = []
    regressor = lgb.LGBMRegressor()
    forecaster = make_reduction(regressor, window_length=15, strategy="recursive")

    for y in ts_data:
        cv = ExpandingWindowSplitter(fh=h, initial_window=int(np.floor(0.6*len(y))), start_with_window=True)
        gscv = ForecastingGridSearchCV(forecaster, strategy="refit", cv=cv, param_grid=param_grid, scoring=MeanAbsoluteError())
        gscv.fit(y)
        fcasts.append(gscv.predict(h))

    return fcasts

# function to forecast with a VAR model
# define function for forecasting with a VAR model
def VAR_forecast(ts_data, h, param_grid, noisy_protection=False):

    # get the length of each series
    lengths = [len(y) for y in ts_data]

    # store the unique length values
    unique_lengths = np.unique(lengths)

    # store the forecasts in an array of all forecasts using the stored series indices
    full_forecasts = np.zeros([len(ts_data), h])

    for l in unique_lengths:
        # get the indexes of each series with the lth length
        Y_ids = np.nonzero(lengths == l)[0]

        print("Group size: " + str(len(Y_ids)))

        # store series in a list
        group = [ts_data[j].reset_index(drop=True) for j in Y_ids]

        # convert list to TxK dataframe
        group = pd.concat(group, axis=1, ignore_index=True)

        print("Group length: " + str(group.shape[0]))

        if noisy_protection==True:
            forecaster = VAR(maxlags=1)
            forecaster.fit(group)
            y_pred = forecaster.predict(h)
        else:
            forecaster = VAR()
            cv = ExpandingWindowSplitter(fh=h, initial_window=int(np.floor(0.6*group.shape[0])), start_with_window=True)
            gscv = ForecastingGridSearchCV(forecaster, strategy="refit", cv=cv, param_grid=param_grid, scoring=MeanAbsoluteError())
            gscv.fit(group)
            print(gscv.best_params_)
            y_pred = gscv.predict(h)

        # generate forecasts

        # store forecasts in dataframe for all series
        full_forecasts[Y_ids,:] = y_pred.T

    full_forecasts = [pd.Series(full_forecasts[i,:]) for i in range(full_forecasts.shape[0])]

    for i in range(len(full_forecasts)):
        last_time = ts_data[i].index[-1]
        full_forecasts[i].index = np.arange(last_time+1, last_time+1+h)

    return full_forecasts

# general function for training a model and generating forecasts.
def train_and_forecast(ts_data, horizon_length, forecasting_model, param_grid=None, last_window=None, noisy_protection=False):
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
    fh = np.arange(1, horizon_length+1)

    # list to store forecasts
    fcasts = []

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
        forecaster = ExponentialSmoothing(trend="additive", seasonal="additive", sp=12, use_boxcox=False)
        fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]
    # if using VAR
    elif forecasting_model == "VAR":
        fcasts = VAR_forecast(ts_data=ts_data, h=horizon_length, param_grid=param_grid, noisy_protection=noisy_protection)
    # if using ARIMA
    elif forecasting_model == "ARIMA":
        forecaster = AutoARIMA(D=0, seasonal=True, sp=12, maxiter=10, suppress_warnings=True)
        fcasts = [forecaster.fit(x).predict(fh) for x in ts_data]
    # if using univariate LGBM
    elif forecasting_model == "Univariate_LGBM":
        fcasts = univariate_lgbm_forecast(ts_data=ts_data, h=fh, param_grid=param_grid)

################################################################################

    # if using an LGBM,
    elif forecasting_model == "Multivariate_LGBM":

        forecaster = lgb.LGBMRegressor()

        # store the number of series
        num_series = len(last_window)

        # store the time indexes for the forecasts
        fcast_indexes = [np.arange(last_window[i].index[-1]+1, last_window[i].index[-1]+horizon_length+1) for i in range(num_series)]

        # separate training data and target
        X_train, Y_train = ts_data.iloc[:,:-1], ts_data.iloc[:,-1]

        # train global model
        forecaster.fit(X_train, Y_train)

        ## generate forecasts ##

        # store forecasted value based on last window of training data
        last_window = np.array(last_window)
        last_prediction = forecaster.predict(last_window)

        for i in range(horizon_length):
            # add latest forecast
            fcasts.append(last_prediction)
            # roll window forward one period (places first value in window at the end)
            last_window = np.roll(last_window, -1)
            # replace the end value (used to be first value) with the latest forecast
            last_window[:,-1] = last_prediction
            # forecast for next period
            last_prediction = forecaster.predict(last_window)

        # combine forecasts into (num_series, horizon_length) shaped array
        fcasts = np.concatenate([i.reshape(num_series, 1) for i in fcasts], axis=1)
        fcasts = [pd.Series(x) for x in fcasts]

        # add correct time index back to forecasts
        for i in range(num_series):
            fcasts[i].index = fcast_indexes[i]

################################################################################

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

    mape_global = mean_absolute_percentage_error(test_data, original_forecasts, symmetric=False, multioutput="uniform_average")
    mape_global_protected = mean_absolute_percentage_error(test_data, protected_forecasts, symmetric=False, multioutput="uniform_average")

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

    # mean absolute percentage error for original forecasts (without adjustment)
    mae_up = mean_absolute_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up])
    mae_down = mean_absolute_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down])

    # mean absolute percentage error for protected forecasts (with adjustment)
    mae_up_protected = mean_absolute_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up])
    mae_down_protected = mean_absolute_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down])

    # median absolute percentage error for original forecasts (without adjustment)
    mdae_up = median_absolute_error(collapsed_test[adjusted_up], collapsed_original[adjusted_up])
    mdae_down = median_absolute_error(collapsed_test[adjusted_down], collapsed_original[adjusted_down])

    # median absolute percentage error for protected forecasts (with adjustment)
    mdae_up_protected = median_absolute_error(collapsed_test[adjusted_up], collapsed_protected[adjusted_up])
    mdae_down_protected = median_absolute_error(collapsed_test[adjusted_down], collapsed_protected[adjusted_down])

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
        "Global MAPE": mape_global,
        "Global Protected MAPE": mape_global_protected,
        "Global MAE": mae_global,
        "Global Protected MAE": mae_global_protected,
        "Global MdAE": mdae_global,
        "Global Protected MdAE": mdae_global_protected,
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

def full_forecast_analysis(Y, h, forecasting_model, window_length=None, make_stationary=False, seasonality_type=None, sp=None, remove_seasonality=False, mean_normalize=False, log=False, param_grid=None, noisy_protection=False):

    transform_dict = {}
    # if reduced data is to be created,
    if window_length is not None:
        transform_dict["windows"] = {"window_length":window_length}
    # if seasonality is to be removed through pre-processing,
    if remove_seasonality:
        transform_dict["deseasonalize"] = {"sp":sp, "seasonality_type":seasonality_type}

    # perform pre-processing, output the processed time series, the last window
    # of data if using reduction, and the last window with the trend to use when
    # reversing per-window normalization
    Y_processed, Y_last_window, Y_last_window_trend, full_lags = pre_process(ts_data=Y,
                                                                             mean_normalize=mean_normalize,
                                                                             log=log,
                                                                             make_stationary=make_stationary,
                                                                             sp=sp,
                                                                             transform_dict=transform_dict)
                                                                             
    # Y_protected_processed, Y_protected_last_window, Y_protected_last_window_trend = pre_process(ts_data=Y_protected,
    #                                                                                             mean_normalize=mean_normalize,
    #                                                                                             log=log,
    #                                                                                             make_stationary=make_stationary,
    #                                                                                             sp=sp,
    #                                                                                             transform_dict=transform_dict)

    # train forecasting model and generate forecasts
    forecasts = train_and_forecast(ts_data=Y_processed,
                                   horizon_length=h,
                                   forecasting_model=forecasting_model,
                                   last_window=Y_last_window,
                                   param_grid=param_grid,
                                   noisy_protection=noisy_protection)

    # forecasts_protected = train_and_forecast(ts_data=Y_protected_processed,
    #                                          horizon_length=h,
    #                                          forecasting_model=forecasting_model,
    #                                          protection_method=protection_method,
    #                                          last_window=Y_protected_last_window,
    #                                          param_grid=param_grid)

    # post-process the forecasts
    forecasts = post_process(full_ts_data=Y,
                             forecasts=forecasts,
                             last_window_with_trend=Y_last_window_trend,
                             full_lags=full_lags,
                             mean_normalize=mean_normalize,
                             log=log,
                             make_stationary=make_stationary,
                             sp=sp,
                             transform_dict=transform_dict)

    # forecasts_protected = post_process(full_ts_data=Y_protected,
    #                                    forecasts=forecasts_protected,
    #                                    last_window_with_trend=Y_protected_last_window_trend,
    #                                    mean_normalize=mean_normalize,
    #                                    log=log,
    #                                    make_stationary=make_stationary,
    #                                    sp=sp,
    #                                    transform_dict=transform_dict)

    # save the forecasts to a file
    # forecasts_original.to_csv("../../Outputs/Forecasts/" + forecasting_model + "_" + "h" + str(h) + "_original.csv")
    # forecasts_protected.to_csv("../../Outputs/Forecasts/" + forecasting_model + "_" + "h" + str(h) + "_" + protection_method + ".csv")

    # Test = pd.DataFrame([x.reset_index(drop=True) for x in Test]).T
    # results = forecast_results(test_data=Test, original_forecasts=forecasts_original, protected_forecasts=forecasts_protected)

    return forecasts

# # generate results for all forecasting models under one protection method
# def all_models_one_protection(full_data, h, sp=None, window_length=None, coding_type=None, coding_percentage=None, num_stdev=None, epsilon=None, bias_adjust_forecasts=True):
#
#     # create train and test data
#     Y = [x.iloc[:-h] for x in full_data]
#     Test = [x.iloc[-h:] for x in full_data]
#
#     # create protected version of Y
#     Y_protected = apply_data_protection(sensitive_data=Y, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon)
#
#     # define forecasting models
#     ses = ExponentialSmoothing(use_boxcox=False)
#     des = ExponentialSmoothing(trend="additive", use_boxcox=False)
#     tes = ExponentialSmoothing(trend="additive", seasonal="additive", sp=sp, use_boxcox=False)
#     lgbm = lgb.LGBMRegressor()
#
#     ses_results = full_forecast_analysis(Y=Y, Y_protected=Y_protected, Test=Test, forecasting_model=ses, h=h, truncate=True, log=True, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon, bias_adjust_forecasts=bias_adjust_forecasts)
#
#     des_results = full_forecast_analysis(Y=Y, Y_protected=Y_protected, Test=Test, forecasting_model=des, h=h, truncate=True, log=True, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon, bias_adjust_forecasts=bias_adjust_forecasts)
#
#     tes_results = full_forecast_analysis(Y=Y, Y_protected=Y_protected, Test=Test, forecasting_model=tes, h=h, truncate=True, log=True, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon, bias_adjust_forecasts=bias_adjust_forecasts)
#
#     lgbm_results = full_forecast_analysis(Y=Y, Y_protected=Y_protected, Test=Test, forecasting_model=lgbm, h=h, truncate=True, mean_normalize=True, log=True, sp=sp, seasonality_type="additive", remove_seasonality=True, window_length=window_length, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon, bias_adjust_forecasts=bias_adjust_forecasts)
#
#     return [ses_results, des_results, tes_results, lgbm_results]
