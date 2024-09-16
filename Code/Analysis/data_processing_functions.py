##### This file contains various functions used to pre-process
##### time series data.
##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
import pmdarima as pm
from scipy import stats

# splitting function used in VAR forecast
def split(a, n):
    # how many chunks of size n can we create
    num_chunks = np.floor(len(a) / n)
    return(np.array_split(a, num_chunks))

# function to conduct first differencing on time series
def difference_to_stationarity(ts_data):
    # list to store differenced series
    differenced_series = [y.diff().dropna() for y in ts_data]
    return differenced_series

# function to reverse first differencing
def reverse_difference_to_stationarity(h, forecasts, ts_data, is_simulated=False, lag_orders=None):

    # list to store reversed forecasts
    reversed_forecasts = []

    for i, f in enumerate(forecasts):
        
        if is_simulated:
            start_value = ts_data[i].iloc[lag_orders[i]]
            # start_value = ts_data[i].iloc[0]
            reverse_diffed = np.r_[start_value, f].cumsum()
            reverse_diffed = pd.Series(reverse_diffed[1:])
            reverse_diffed.index = ts_data[i].index[(lag_orders[i]+1):]
            reversed_forecasts.append(reverse_diffed)

        else:
            start_value = ts_data[i].iloc[-1]
            reverse_diffed = np.r_[start_value, f].cumsum()
            reverse_diffed = pd.Series(reverse_diffed[-h:])
            reverse_diffed.index = f.index
            reversed_forecasts.append(reverse_diffed)

    return reversed_forecasts

# pre-process the data using various pre-processing steps
def pre_process(ts_data, truncate, mean_normalize, log):
    """
    Performs various pre-processing steps. 

    :param ts_data: list containing the time series
    :param truncate: True or False, whether to truncate all values >=1 (this must
        be done before taking the log).
    :param mean_normalize: whether to divide the values of each series by the mean of
        all values.
    :param log: whether to log-transform the time series.
    
    :return processed: list containing the pre-processed series
    """

    processed = ts_data

    # Step 1: truncate any values less than 1 - this only comes into play for
    # data protected using random noise - we do this because we know the M-forecasting
    # competition data is strictly positive

    if truncate:
        processed = [pd.Series([i if i >= 1 else 1 for i in x]) for x in processed]

    # Step 2: mean normalize
    if mean_normalize:
        processed = [x.divide(np.mean(x)) if np.mean(x) != 0.0 else x for x in processed]

    # Step 3: log transform each series
    if log:
        processed = [np.log(x) for x in processed]

    return processed

# post-process the data to reverse the steps performed in pre_processing (but applied to the forecasts)
def post_process(full_ts_data, forecasts, log, mean_normalize, truncate, var_sim=False):

    """
    Performs various pre-processing steps. Designed to be the inverse of the pre-processing steps, allowing
        forecasts to be analyzed on the scale of the original time series.

    :param full_ts_data: list containing the unprotected time series
    :param forecasts: list containing the forecast for each time series
    :param log: whether to reverse the log-transform
    :param mean_normalize: whether to reverse the mean_normalization step
    :param truncate: True or False, whether to truncate all values >=1 since we
        know the M-competition data is strictly positive.
    
    :return processed: list containing the post-processed forecasts
    """

    # create processed copy of forecasts, store the number of series
    processed = [pd.Series(x) for x in forecasts]
    num_series = len(forecasts)

    ## Forecasting: Principles and Practice
    # bias adjusted forecasts are y = exp(w)[1 + sigma^2_h/2]
    # where sigma^2_h/2 is the h-step forecast variance
    # bias adjusted forecasts

    # This bias-corrected back-adjustment is inappropriate for MAE - the
    # sigma2 = [np.var(x) for x in processed]
    # processed = [np.exp(processed[i])*(1 + sigma2[i]/2) for i in range(num_series)]

    # median minimizes the expected MAE, so we use the median forecast
    if log:
        processed = [np.exp(processed[i]) for i in range(num_series)]

    if mean_normalize:
        processed = [x * np.mean(full_ts_data[i]) for i, x in enumerate(processed)]

    # make sure forecasts are non-negative
    if truncate:
        processed = [pd.Series([x if x >=0 else 0 for x in y]) for y in processed]

    if var_sim:
        processed = pd.DataFrame([x.reset_index(drop=True) for x in processed])
    else:
        processed = pd.DataFrame([x.reset_index(drop=True) for x in processed]).T

    return processed