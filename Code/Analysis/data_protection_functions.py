##### This file contains various functions used to protect
##### time series data. Functions include:
#####   - additive_noise_protection (additive noise)
#####   - coding_protection (top and bottom coding)
#####   - DP_protection (differential privacy)
#####   - apply_data_protection (function to apply any of the above methods)

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np

# generic function to protect time series data
def apply_data_protection(sensitive_data, coding_type=None, coding_percentage=None, num_stdev=None, epsilon=None):
    """
    Applies data protection for an assortment of protection methods. Supplying
    method specific parameters will apply the corresponding protection, e.g.,
    supplying a value for Epsilon will apply differential privacy. Available
    methods are:
        - Top coding
        - Bottom coding
        - Additive noise
        - Differential privacy

    :param sensitive_data: pandas dataframe containing confidential time series
        in rows, time periods in columns.
    :param coding_type: whether to perform "Top" or "Bottom" coding.
    :param coding_percentage: the percentage of top or bottom observations to protect.
    :param num_stdev: used for additive noise protection - defines the number of
        data standard deviations to use as standard deviation of random noise.
    :param epsilon: Privacy budget for differential privacy. Larger values = less noise
        and less privacy.
    :return P: pandas dataframe containing protected time series in rows, time periods
        in columns.
    """

    # protect sensitive_data using top or bottom coding
    if (coding_type is not None) and (coding_percentage is not None):
        P = coding_protection(sensitive_data, coding_type, coding_percentage)
        return P

    # protect sensitive_data using additive noise
    elif num_stdev is not None:
        P = additive_noise_protection(sensitive_data, num_stdev)
        return P

    # protect sensitive_data using differential privacy
    elif epsilon is not None:
        P = DP_protection(sensitive_data, epsilon)
        return P

    else:
        print("No protection method selected.")
        return None

# protect time series using top or bottom coding
def coding_protection(sensitive_data, coding_type, coding_percentage):
    """
    Performs top or bottom coding on each time series in a dataset,
    returns the protected time series data.

    :param sensitive_data: pandas dataframe with series in rows and time periods
        in columns.
    :param coding_type: whether to perform "Top" or "Bottom" coding.
    :param coding_percentage: the percentage of top or bottom observations to protect.
    :return P: pandas datafram containing the top or bottom coded time series data.
    """

    if coding_type=="Bottom":
        # calculate coding_percentage quantile for each series
        qs = [np.quantile(x, q=coding_percentage) for x in sensitive_data]

        # apply bottom coding
        P = [pd.Series([val if val > qs[i] else qs[i] for val in row]) for i, row in enumerate(sensitive_data)]

    elif coding_type=="Top":
        # calculate 1-coding_percentage quantile for each series
        qs = [np.quantile(x, q=1-coding_percentage) for x in sensitive_data]

        # apply top coding
        P = [pd.Series([val if val < qs[i] else qs[i] for val in row]) for i, row in enumerate(sensitive_data)]

    return P


# protect time series using additive noise (not differentially private)
def additive_noise_protection(sensitive_data, num_stdev):
    """
    Adds random noise sampled from 0 centered normal distribution with standard
    deviation defined by num_stdev*standard_deviation(y) to each sensitive
    series y.

    :param sensitive_data: pandas dataframe containing series in the rows and time
                periods in the columns.
    :param num_stdev: number of data standard deviations to use as standard deviation of
                random noise.
    :return P: pandas dataframe containing protected series in the rows and time
                periods in the columns.
    """

    # store the number of time series
    num_series = len(sensitive_data)

    # calculate standard deviation of each series
    sigmas = [np.std(x) for x in sensitive_data]

    # random normal draws to create r,
    # add to each y_i
    rng = np.random.default_rng(2022)
    P = [rng.normal(loc=0, scale=sigmas[i]*num_stdev, size=len(row)) + row for i, row in enumerate(sensitive_data)]

    return P


# protect time series using differential privacy
def DP_protection(sensitive_data, epsilon):
    """
    Adds random noise to each sensitive series y to achieve differential privacy,
    defined by privacy budget epsilon and the series-specific sensitivity (GS) approximated
    by the difference between the minimum and maximum values in the series.
    Noise is sampled from a Laplace distribution centered at 0, with scale = GS/epsilon.
    This is the method used by Goncalves, Bessa, & Pinson (2021).

    :param sensitive_data: pandas dataframe containing series in the rows and time
                periods in the columns.
    :param epsilon: Privacy budget. Larger values = less noise and less privacy.
    :return P: pandas dataframe containing protected series in the rows and time
                periods in the columns.
    """

    # store the number of time series
    num_series = len(sensitive_data)

    # calculate global sensitivity for each series
    GS = [x.max() - x.min() for x in sensitive_data]

    # For each series, add random noise sampled from 0-centered laplace
    # distribution with scale parameter = GS/epsilon
    rng = np.random.default_rng(2022)
    P = [rng.laplace(loc=0, scale=GS[i]/epsilon, size=len(row)) + row for i, row in enumerate(sensitive_data)]

    return P
