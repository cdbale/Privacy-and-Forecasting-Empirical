##### This file contains various functions used to pre-process
##### time series data.
##### Author: Cameron Bale

################################################################################

from re import I
import pandas as pd
import numpy as np
import pmdarima as pm
from scipy import stats

# splitting function used in VAR forecast
# def split(a, n):
#     k, m = divmod(len(a), n)
#     print(k, m)
#     return (a[i*n+min(i, m):(i+1)*n+min(i+1, m)] for i in range(k))

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
def pre_process(ts_data, h, truncate, log, mean_normalize, sp, transform_dict={}):
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

    # Step 1: truncate any values less than 1 - this only comes into play for
    # data protected using random noise - we do this because we know the M3
    # monthly micro data is strictly positive
    if truncate:
        processed = [pd.Series([i if i >= 1 else 1 for i in x]) for x in processed]

    # Step 2: mean normalize
    if mean_normalize:
        processed = [x.divide(np.mean(x)) if np.mean(x) != 0.0 else x for x in processed]

    # Step 3: log transform each series
    if log:
        processed = [np.log(x) for x in processed]

    # Step 4: make stationary series using differencing
    # if make_stationary:
    #     processed = difference_to_stationarity(processed)

    return processed

# post-process the data to reverse the steps performed in pre_processing
def post_process(full_ts_data, forecasts, h, truncate, log, mean_normalize, sp, var_sim=False):

    # create processed copy of forecasts, store the number of series
    processed = [pd.Series(x) for x in forecasts]
    num_series = len(forecasts)

    # reverse seasonal and first differencing
    # if make_stationary:
    #     temp_ts_data = pre_process(full_ts_data, h)
    #     processed = reverse_difference_to_stationarity(h, processed, temp_ts_data)

    # reverse the log

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
