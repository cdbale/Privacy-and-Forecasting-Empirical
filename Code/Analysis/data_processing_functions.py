##### This file contains various functions used to pre-process
##### time series data. Functions include:
#####   -
#####   -

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
from sktime.transformations.series.detrend import ConditionalDeseasonalizer, Detrender

# function to perform reduction on global training data
def reduce_train_test_global(train_data, window_length):
    # slightly modified code from the M4 competition
    # adapted from code in sktime tutorial.
    """
    Splits the series into train and test sets.

    :param train_data: a 2-d numpy array with series as rows.
    :param window_length: window_length.
    :param h: number of out of sample points (forecast horizon length).
    :return X_train, Y_train: reduced training datasets.
    """

    # store number of time series
    num_series = len(train_data)

    # empty lists for training data
    X_train = []
    Y_train = []

    for series in train_data:
        series = np.array(series)
        x_train, y_train = series[:-1], np.roll(series, -window_length)[:-window_length]

        x_train = np.reshape(x_train, (-1, 1))
        temp_train = np.roll(x_train, -1)

        for x in range(1, window_length):
            x_train = np.concatenate((x_train[:-1], temp_train[:-1]), 1)
            temp_train = np.roll(temp_train, -1)[:-1]

        X_train.append(x_train)
        Y_train.append(y_train)

    last_x = [x[-1,:] for x in X_train]
    last_y = [y[-1] for y in Y_train]

    last_window = [pd.Series(np.concatenate([last_x[i][1:], np.expand_dims(last_y[i], axis=0)])) for i in range(len(last_x))]

    X_train = np.concatenate(X_train)
    Y_train = np.concatenate(Y_train)

    # concatenate outcome with features
    combined = pd.DataFrame(np.hstack([X_train, Y_train.reshape(X_train.shape[0],1)]))

    # perform per-window trend normalization (detrending) for training data and
    # last window
    detrender = Detrender()
    last_window_dt = [detrender.fit_transform(x) for x in last_window]
    combined = combined.apply(lambda x: detrender.fit_transform(x), axis=1)

    # add correct time index back to last_window
    for i in range(num_series):
        last_window[i].index = train_data[i].index[-window_length:]
        last_window_dt[i].index = train_data[i].index[-window_length:]

    return combined, last_window_dt, last_window


# pre-process the data using various pre-processing steps
def pre_process(ts_data, truncate=False, mean_normalize=False, log=False, transform_dict={}):
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

    if truncate:
        # Step 1: truncate any values less than 1
        processed = [pd.Series([i if i >= 1 else 1 for i in x]) for x in processed]

    if mean_normalize:
        processed = [x.divide(np.mean(x)) for x in processed]

    if log:
        # step 2: take the log
        processed = [np.log(x) for x in processed]

    if len(transform_dict) > 0:

        if "deseasonalize" in transform_dict:
            # step 3: conditional deseasonalization
            transformer = ConditionalDeseasonalizer(sp=transform_dict["deseasonalize"]["sp"], model=transform_dict["deseasonalize"]["seasonality_type"])
            # fit and transform each series
            processed = [transformer.fit_transform(row) for row in processed]

        if "windows" in transform_dict:
            # steps 4, 5: perform reduction
            processed, last_window_dt, last_window = reduce_train_test_global(train_data=processed, window_length=transform_dict["windows"]["window_length"])

    return processed, last_window_dt, last_window


# post-process the data to reverse the steps performed in pre_processing
def post_process(full_ts_data, forecasts, last_window_with_trend=None, truncate=False, mean_normalize=False, log=False, bias_adjusted=False, transform_dict={}):

    processed = forecasts
    num_series = len(forecasts)

    if len(transform_dict) > 0:

        # reverse window normalization
        if "windows" in transform_dict:
            detrender = Detrender()
            processed = [detrender.fit(last_window_with_trend[i]).inverse_transform(processed[i]) for i in range(num_series)]

        # add seasonality back in
        if "deseasonalize" in transform_dict:
            ts_data, _ , _ = pre_process(full_ts_data, truncate=truncate, mean_normalize=False, log=log, transform_dict={})
            deseasonalizer = ConditionalDeseasonalizer(sp=transform_dict["deseasonalize"]["sp"], model=transform_dict["deseasonalize"]["seasonality_type"])
            processed = [deseasonalizer.fit(ts_data[i]).inverse_transform(processed[i]) for i in range(num_series)]

    # reverse the log (exponentiate - check on bias correction)
    if log:
        if bias_adjusted:
            ## Forecasting: Principles and Practice
            # bias adjusted forecasts are y = exp(w)[1 + sigma^2_h/2]
            # where sigma^2_h/2 is the h-step forecast variance
            # bias adjusted forecasts
            sigma2 = [np.var(x) for x in processed]
            processed = [np.exp(processed[i])*(1 + sigma2[i]/2) for i in range(num_series)]
        else:
            processed = [np.exp(x) for x in processed]

    if mean_normalize:
        ts_data, _ , _ = pre_process(full_ts_data, truncate=truncate, mean_normalize=False, log=False, transform_dict={})
        processed = [x * np.mean(ts_data[i]) for i,x in enumerate(processed)]

    processed = pd.DataFrame([x.reset_index(drop=True) for x in processed]).T

    return processed
