##### This file contains various functions used to pre-process
##### time series data. Functions include:
#####   -
#####   -

##### Author: Cameron Bale

################################################################################

import pandas as pd
import numpy as np
from sktime.transformations.series.detrend import ConditionalDeseasonalizer, Detrender

# perform conditional deseasonalization
def deseasonalize(ts_data, sp, seasonality_type):
    """
    Performs deseasonalization conditional on 90% autocorrelation seasonality test.

    :param ts_data: pandas dataframe containing series in rows, time periods in
        columns.
    :param sp: seasonal periodicity.
    :param seasonality_type: what type of model to use to estimate seasonal
        component, either "additive" or "multiplicative".
    :return transformed_data: pandas dataframe containing the transformed series
        in rows, time periods in columns.
    """

    # instantiate conditional deseasonalizer
    transformer = ConditionalDeseasonalizer(sp=sp, model=seasonality_type)

    # fit and transform each series
    transformed_data = pd.concat([transformer.fit_transform(row) for _, row in ts_data.iterrows()], axis=1).T

    return transformed_data

# function to perform reduction on global training data
def reduce_train_test_global(train_data, window_length, h):
    # slightly modified code from the M4 competition
    # adapted from code in sktime tutorial.
    """
    Splits the series into train and test sets.

    :param train_data: a 2-d numpy array with series as rows.
    :param window_length: window_length.
    :param h: number of out of sample points (forecast horizon length).
    :return X_train, Y_train: reduced training datasets.
    """

    X_train = []
    Y_train = []

    for series in train_data:
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

    last_window = pd.DataFrame(np.hstack([np.vstack(last_x), np.vstack(last_y)])[:,1:])

    X_train = np.concatenate(X_train)
    Y_train = np.concatenate(Y_train)

    # concatenate outcome with features
    combined = pd.DataFrame(np.hstack([X_train, Y_train.reshape(X_train.shape[0],1)]))

    # perform per-window trend normalization (detrending) for training data and
    # last window
    detrender = Detrender()
    last_window = last_window.apply(lambda x: detrender.fit_transform(x), axis=1)
    combined = combined.apply(lambda x: detrender.fit_transform(x), axis=1)

    return combined, last_window

# pre-process the data using various pre-processing steps
def pre_process(ts_data, truncate=False, log=False, transform_dict=None):
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
                - {"window_length":number of time periods as features in the window,
                   "h":forecast horizon.}
    :return processed: pandas dataframe that has been pre-processed with the series
        in the rows and time periods in the columns for statistical models, or a
        reduced tabular dataframe for machine learning/deep learning models (with
        the target as the last column in the dataframe.)
    """

    processed = ts_data

    if truncate:
        # Step 1: truncate any values less than 1
        processed = processed.apply(lambda x: [i if i >= 1 else 1 for i in x])

    if log:
        # step 2: take the log
        processed = np.log(processed)

    if transform_dict is not None:

        if "deseasonalize" in transform_dict:
            # step 3: conditional deseasonalization
            processed = deseasonalize(processed, sp=transform_dict["deseasonalize"]["sp"], seasonality_type=transform_dict["deseasonalize"]["seasonality_type"])

        if "windows" in transform_dict:
            # steps 4, 5: perform reduction
            processed, last_window = reduce_train_test_global(train_data=np.array(processed), window_length=transform_dict["windows"]["window_length"], h=transform_dict["windows"]["h"])

    return processed

# post-process the data to reverse the steps performed in pre_processing
def post_process(ts_data, truncate=False, log=False, transform_dict=None):



    return None
