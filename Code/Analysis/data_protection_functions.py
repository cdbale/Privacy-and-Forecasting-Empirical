##### This file contains various functions used to protect
##### time series data. Functions include:
#####   - additive_noise_protection (additive noise)
#####   - DP_protection (differential privacy)
#####   - k-nTS(+) protection (imports pre-created protected files)
#####   - apply_data_protection (function to apply any of the above methods)

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np

def save_protected_dataset(original_data_path, save_data_path, num_stdev=None, epsilon=None, k=None, plus=False):

    """
    Function to create and save a protected version of a data set.

    :param original_data_path: file path to the original data.
    :param save_data_path: file path to where protected data should be saved.
    :param num_stdev: number of data standard deviations to use as standard deviation of
                random noise in additive noise protection.
    :param epsilon: Privacy budget for differential privacy.
                Larger values = less noise and less privacy.
    :param k: the number of nearest neighbor time series in k-nTS
    :param plus: whether to use the (+) variant of k-nTS

    :return None: nothing is returned, the protected version of the file is
                saved at the save_data_path path.
    """

    # import time series
    # ignore header to use integers as column names
    full_data = pd.read_csv(original_data_path)

    # convert to a list of series
    # drop NA values
    full_data = [x.dropna() for _, x in full_data.iterrows()]

    protected = pd.DataFrame(
                             apply_data_protection(sensitive_data=full_data,
                                                   num_stdev=num_stdev,
                                                   epsilon=epsilon,
                                                   k=k,
                                                   plus=plus)
                            )

    protected.to_csv(save_data_path, index=False)

    return None

################################################################################

# generic function to protect time series data
def apply_data_protection(sensitive_data, num_stdev=None, epsilon=None, k=None, plus=False):
    """
    Applies data protection for an assortment of protection methods. Supplying
    method specific parameters will apply the corresponding protection, e.g.,
    supplying a value for Epsilon will apply differential privacy. Available
    methods are:
        - Additive noise
        - Differential privacy
        - k-nts

    :param sensitive_data: pandas dataframe containing confidential time series
        in rows, time periods in columns.
    :param num_stdev: used for additive noise protection - defines the number of
        data standard deviations to use as standard deviation of random noise.
    :param epsilon: Privacy budget for differential privacy. Larger values = less noise
        and less privacy.
    :return P: pandas dataframe containing protected time series in rows, time periods
        in columns.
    """

    # protect sensitive_data using additive noise
    if num_stdev is not None:
        P = additive_noise_protection(sensitive_data, num_stdev)
        return P

    # protect sensitive_data using differential privacy
    elif epsilon is not None:
        P = DP_protection(sensitive_data, epsilon)
        return P

    elif k is not None:
        P = k_nts_protection(k=k, plus=plus)
        return P

    else:
        print("No protection method selected.")
        return None

################################################################################

def k_nts_protection(k, plus):

    """
    Reads in a data file protected by k-nTS or k-nTS+. The data files would have
    been protected using the R scripts k-nTS.R and k-nTS_plus.R.

    :param k: the number of nearest neighbor time series.
    :param plus: boolean indicating whether plus (+) variant of k-nTS is
    desired.
    :return P: pandas dataframe containing protected time series in rows, time periods
        in columns.
    """

    if plus:
        P = pd.read_csv("../../Data/Train/Clean/h1_k-nts-plus_" + str(k) + ".csv", header=None, skiprows=1)
    else:
        P = pd.read_csv("../../Data/Train/Clean/h1_k-nts_" + str(k) + ".csv", header=None, skiprows=1)

    # convert to a list of series, and drop missing values
    P = [x.dropna() for _, x in P.iterrows()]

    return P

################################################################################

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

################################################################################

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
    GS = [np.abs(x.max() - x.min()) for x in sensitive_data]

    # For each series, add random noise sampled from 0-centered laplace
    # distribution with scale parameter = GS/epsilon
    rng = np.random.default_rng(2022)
    P = [rng.laplace(loc=0, scale=GS[i]/epsilon, size=len(row)) + row for i, row in enumerate(sensitive_data)]

    return P
