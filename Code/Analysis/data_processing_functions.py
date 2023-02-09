##### This file contains various functions used to pre-process
##### time series data.
##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
import pmdarima as pm
from scipy import stats
from sktime.transformations.series.detrend import ConditionalDeseasonalizer, Detrender
from sktime.forecasting.model_selection import SlidingWindowSplitter

# function to conduct first differencing on time series
def difference_to_stationarity(ts_data):

    # list to store differenced series
    differenced_series = [y.diff().dropna() for y in ts_data]

    return differenced_series

# function to reverse first differencing
def reverse_difference_to_stationarity(forecasts, ts_data, is_fitted):

    # list to store reversed forecasts
    reversed_forecasts = []

    if is_fitted:
        for i, f in enumerate(forecasts):
            start_value = ts_data[i].iloc[0]
            reverse_diffed = pd.Series(np.r_[start_value, f].cumsum())
            reverse_diffed.index = np.arange(0, ts_data[i].index[-1]+1)
            reversed_forecasts.append(reverse_diffed)

    elif not is_fitted:
        for i, f in enumerate(forecasts):
            start_value = ts_data[i].iloc[-1]
            reverse_diffed = np.r_[start_value, f].cumsum()
            reverse_diffed = pd.Series(reverse_diffed[-h:])
            reverse_diffed.index = f.index
            reversed_forecasts.append(reverse_diffed)

    return reversed_forecasts

# def reduce_train_test(train_data, window_length, target_forecast_period):
#     cv = SlidingWindowSplitter(window_length=window_length, fh=target_forecast_period, start_with_window=True)
#     X = []
#     target = []
#     for input_window, output_window in cv.split(train_data):
#         X.append(train_data[input_window].reset_index(drop=True))
#         target.append(train_data[output_window].reset_index(drop=True))
#
#     X = np.array(pd.concat(X, axis=1).T)
#     target = np.array(pd.concat(target, axis=0))
#
#     return X, target

# def reduce_train_test_global(train_data, window_length, target_forecast_period):
#     # slightly modified code from the M4 competition
#     # adapted from code in sktime tutorial.
#     """
#     Splits the series into train and test sets.
#
#     :param train_data: a 2-d numpy array with series as rows.
#     :param window_length: window_length.
#     :param h: number of out of sample points (forecast horizon length).
#     :return X_train, Y_train: reduced training datasets.
#     """
#
#     # store number of time series
#     num_series = len(train_data)
#
#     # empty lists for training data
#     X_train = []
#     Y_train = []
#
#     for series in train_data:
#         x_train, y_train = reduce_train_test(series, window_length, target_forecast_period)
#         X_train.append(x_train)
#         Y_train.append(y_train)
#
#     # last_x = [x[-1,:] for x in X_train]
#     # last_y = [y[-1] for y in Y_train]
#
#     # testing this line
#     last_window = [i.iloc[-window_length:] for i in train_data]
#
#     # last_window = [pd.Series(np.concatenate([last_x[i][1:], np.expand_dims(last_y[i], axis=0)])) for i in range(len(last_x))]
#
#     X_train = np.concatenate(X_train)
#     Y_train = np.concatenate(Y_train)
#
#     # concatenate outcome with features
#     combined = pd.DataFrame(np.hstack([X_train, Y_train.reshape(X_train.shape[0],1)]))
#
#     # perform per-window trend normalization (detrending) for training data and
#     # last window
#     detrender = Detrender()
#     last_window_dt = [detrender.fit_transform(x) for x in last_window]
#     combined = combined.apply(lambda x: detrender.fit_transform(x), axis=1)
#
#     # add correct time index back to last_window
# #     for i in range(num_series):
# #         last_window[i].index = train_data[i].index[-window_length:]
# #         last_window_dt[i].index = train_data[i].index[-window_length:]
#
#     return combined, last_window_dt, last_window

# # function to perform reduction on global training data
# def reduce_train_test_global(train_data, window_length):
#     # slightly modified code from the M4 competition
#     # adapted from code in sktime tutorial.
#     """
#     Splits the series into train and test sets.
#
#     :param train_data: a 2-d numpy array with series as rows.
#     :param window_length: window_length.
#     :param h: number of out of sample points (forecast horizon length).
#     :return X_train, Y_train: reduced training datasets.
#     """
#
#     # store number of time series
#     num_series = len(train_data)
#
#     # empty lists for training data
#     X_train = []
#     Y_train = []
#
#     for series in train_data:
#         series = np.array(series)
#         x_train, y_train = series[:-1], np.roll(series, -window_length)[:-window_length]
#
#         x_train = np.reshape(x_train, (-1, 1))
#         temp_train = np.roll(x_train, -1)
#
#         for x in range(1, window_length):
#             x_train = np.concatenate((x_train[:-1], temp_train[:-1]), 1)
#             temp_train = np.roll(temp_train, -1)[:-1]
#
#         X_train.append(x_train)
#         Y_train.append(y_train)
#
#     last_x = [x[-1,:] for x in X_train]
#     last_y = [y[-1] for y in Y_train]
#
#     last_window = [pd.Series(np.concatenate([last_x[i][1:], np.expand_dims(last_y[i], axis=0)])) for i in range(len(last_x))]
#
#     X_train = np.concatenate(X_train)
#     Y_train = np.concatenate(Y_train)
#
#     # concatenate outcome with features
#     combined = pd.DataFrame(np.hstack([X_train, Y_train.reshape(X_train.shape[0],1)]))
#
#     # perform per-window trend normalization (detrending) for training data and
#     # last window
#     detrender = Detrender()
#     last_window_dt = [detrender.fit_transform(x) for x in last_window]
#     combined = combined.apply(lambda x: detrender.fit_transform(x), axis=1)
#
#     # add correct time index back to last_window
#     for i in range(num_series):
#         last_window[i].index = train_data[i].index[-window_length:]
#         last_window_dt[i].index = train_data[i].index[-window_length:]
#
#     return combined, last_window_dt, last_window

# pre-process the data using various pre-processing steps
def pre_process(ts_data, h, target_forecast_period, mean_normalize=False, log=False, standardize=False, detrend=False, make_stationary=False, sp=None, transform_dict={}):
    """
    Performs various pre-processing steps. Data independent steps are implemented
    using boolean values. Data-dependent steps are implemented using a transform_dict
    containing keys with the name of the steps and specific parameters for each
    step.

    :param ts_data: pandas dataframe containing the series in the rows and time
        periods in the columns.
    :param truncate: True or False, whether to truncate all values >=1 (this must
        be done before taking the log).
    :param log: whether to log-transform the time series.
    :param transform_dict: python dictionary, can contain a combination of the
        following transformations. Must supply appropriate parameter values in a
        dictionary for each transformation as follows:
            - "deseasonalize":
                - {"sp":integer for seasonal periodicity,
                   "seasonality_type":"additive" or "multiplicative" seasonality}
            - "windows":
                - {"window_length":number of time periods as features in the window}
    :return processed: pandas dataframe that has been pre-processed with the series
        in the rows and time periods in the columns for statistical models, or a
        reduced tabular dataframe for machine learning/deep learning models (with
        the target as the last column in the dataframe.)
    """

    processed = ts_data
    last_window = None
    last_window_dt = None
    full_lags = None
    pre_detrend = None

    # Step 1: truncate any values less than 1 - this only comes into play for
    # data protected using random noise - we do this because we know the M3
    # monthly micro data is strictly positive
    processed = [pd.Series([i if i >= 1 else 1 for i in x]) for x in processed]

    # Step 2: mean normalize
    if mean_normalize:
        processed = [x.divide(np.mean(x)) for x in processed]

    # Step 3: log transform each series
    if log:
        processed = [np.log(x) for x in processed]

    # Step 4: make stationary series using differencing
    if make_stationary:
        processed = difference_to_stationarity(processed)

    # Step 5: standardize by subtracting the mean and dividing by the standard
    # deviation
    if standardize:
        processed = [(x - np.mean(x))/np.std(x, ddof=1) for x in processed]

    # if there are parameters in the transformation dictionary...
    if len(transform_dict) > 0:

        # Step 5: conditional deseasonalization
        if "deseasonalize" in transform_dict:
            transformer = ConditionalDeseasonalizer(sp=transform_dict["deseasonalize"]["sp"], model=transform_dict["deseasonalize"]["seasonality_type"])
            # deseasonalize each series
            processed = [transformer.fit_transform(row) for row in processed]

        # # Step 6: reduction (rolling window transformation)
        # if "windows" in transform_dict:
        #     # outputs the processed training data, the last window of the
        #     # training data that was detrended, and the last window of the
        #     # training data with the trend left in
        #     processed, last_window_dt, last_window = reduce_train_test_global(train_data=processed, window_length=transform_dict["windows"]["window_length"], target_forecast_period=target_forecast_period)

    # Step 7: detrend the data
    if detrend:
        detrender = Detrender()
        pre_detrend = processed.copy()
        processed = [detrender.fit_transform(row) for row in processed]

    return processed, last_window_dt, last_window, pre_detrend

# post-process the data to reverse the steps performed in pre_processing
def post_process(full_ts_data, forecasts, h, target_forecast_period, last_window_with_trend=None, pre_detrend=None, full_lags=None, mean_normalize=False, detrend=False, log=False, make_stationary=False, sp=None, is_fitted=False, transform_dict={}):

    if forecasts is None:
        return None

    # create processed copy of forecasts, store the number of series
    processed = forecasts
    num_series = len(forecasts)

    # add trend back in if we removed it
    if detrend:
        detrender = Detrender()
        processed = [detrender.fit(pre_detrend[i]).inverse_transform(row) for i, row in enumerate(processed)]

    # if there are transformation dictionary parameters
    if len(transform_dict) > 0:

        # reverse window normalization
        if "windows" in transform_dict:
            detrender = Detrender()
            processed = [detrender.fit(last_window_with_trend[i]).inverse_transform(processed[i]) for i in range(num_series)]

        # add seasonality back in
        if "deseasonalize" in transform_dict:
            ts_data, _, _, _, _ = pre_process(full_ts_data, h, target_forecast_period, mean_normalize=mean_normalize, log=log, transform_dict={})
            deseasonalizer = ConditionalDeseasonalizer(sp=transform_dict["deseasonalize"]["sp"], model=transform_dict["deseasonalize"]["seasonality_type"])
            processed = [deseasonalizer.fit(ts_data[i]).inverse_transform(processed[i]) for i in range(num_series)]

    # reverse seasonal and first differencing
    if make_stationary:
        temp_ts_data, _, _, _ = pre_process(full_ts_data, h, target_forecast_period, log=True)
        processed = reverse_difference_to_stationarity(processed, temp_ts_data, is_fitted)

    # reverse the log
    if log:
        ## Forecasting: Principles and Practice
        # bias adjusted forecasts are y = exp(w)[1 + sigma^2_h/2]
        # where sigma^2_h/2 is the h-step forecast variance
        # bias adjusted forecasts

        # This bias-corrected back-adjustment is inappropriate for MAE - the
        # median minimizes the expected MAE, so we use the median forecast
        # sigma2 = [np.var(x) for x in processed]
        # processed = [np.exp(processed[i])*(1 + sigma2[i]/2) for i in range(num_series)]

        # use the below version if debugging - it allows you to return the original
        # data values when testing the pre-process function
        processed = [np.exp(processed[i]) for i in range(num_series)]

    if mean_normalize:
        # ts_data, _ , _, _ = pre_process(full_ts_data, target_forecast_period, mean_normalize=False, log=False, transform_dict={})
        processed = [x * np.mean(full_ts_data[i]) for i, x in enumerate(processed)]

    # make sure forecasts are non-negative
    processed = [pd.Series([x if x >=0 else 0 for x in y]) for y in processed]

    if is_fitted:
        return pd.DataFrame(processed).T
    else:
        processed = pd.DataFrame([x.reset_index(drop=True) for x in processed]).T
        return processed
