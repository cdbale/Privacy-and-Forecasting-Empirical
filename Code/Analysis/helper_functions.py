# general modules
import numpy as np
import pandas as pd
import lightgbm as lgb
import sktime
from sktime.datatypes._panel._convert import is_nested_dataframe
from sktime.performance_metrics.forecasting import mean_absolute_error, mean_squared_error
from sktime.forecasting.model_selection import temporal_train_test_split
from sktime.datatypes._panel._convert import from_2d_array_to_nested
from sktime.transformations.series.detrend import Detrender

#######################################################
################### Data Processing ###################
#######################################################

def pre_process(train_dataset, test_dataset):
    """
    Performs pre-processing steps on the specified time series dataset, supplied as
    train and test data. Current steps are to normalize each series by its respective mean, and take the log.

    :param train_dataset: pandas dataframe containing training series in the rows, periods in columns.
    :param test_dataset: pandas dataframe containing test series in the rows, periods in the columns.
    :return train_processed, test_processed: the pre-processed time series dataset
    """
    # normalize by the series means
    series_means = train_dataset.apply(np.mean, axis=1)
    train_processed = train_dataset.divide(series_means, axis=0)
    test_processed = test_dataset.divide(series_means, axis=0)
    # add minimum value of a series back to a series if the minimum values is <= 0
    # This avoids problems when taking the log
    ############## This is a temporary fix, need to see how this is handled in practice #######
    # create test_processed first since based on train prior to adding minimums
    test_processed = pd.concat([test_processed.iloc[i,:] + np.abs(np.min(row))+1.0 if np.min(row) <= 0.0 else test_processed.iloc[i,:] for i, row in train_processed.iterrows()], axis=1).T
    train_processed = pd.concat([row + np.abs(np.min(row))+1.0 if np.min(row) <= 0.0 else row for i, row in train_processed.iterrows()], axis=1).T
    # take the log
    test_processed = np.log(test_processed)
    train_processed = np.log(train_processed)
    return train_processed, test_processed

#def post_process(train_dataset, test_dataset, forecasts, means):
    # implement for LGBM

# function to perform reduction on global training data
def reduce_train_test_global(train_data, window_length, h):
    # slightly modified code from the M4 competition
    """
    Splits the series into train and test sets.

    Each step takes multiple points as inputs
    :param train_data: a 2-d numpy array with series as rows
    :param window_length: window_length
    :param h: number of out of sample points (forecast horizon length)
    :return X_train, Y_train: reduced train datasets
    """

    X_train = []
    Y_train = []
    #X_test = []
    #Y_test = []
    #Train, Test = data[:,:-h], data[:,-h:]

    for series in train_data:
        train, test = series[:-h], series[-(h + window_length):]
        x_train, y_train = train[:-1], np.roll(train, -window_length)[:-window_length]
        #x_test, y_test = test[:-1], np.roll(test, -window_length)[:-window_length]

        x_train = np.reshape(x_train, (-1, 1))
        temp_train = np.roll(x_train, -1)
        #x_test = np.reshape(x_test, (-1, 1))
        #temp_test = np.roll(x_test, -1)

        for x in range(1, window_length):
            x_train = np.concatenate((x_train[:-1], temp_train[:-1]), 1)
            temp_train = np.roll(temp_train, -1)[:-1]
            #x_test = np.concatenate((x_test[:-1], temp_test[:-1]), 1)
            #temp_test = np.roll(temp_test, -1)[:-1]

        X_train.append(x_train)
        Y_train.append(y_train)
        #X_test.append(x_test)
        #Y_test.append(y_test)

    X_train = np.concatenate(X_train)
    Y_train = np.concatenate(Y_train)
    #X_test = np.concatenate(X_test)
    #Y_test = np.concatenate(Y_test)

    return X_train, Y_train#, X_test, Y_test

# function to reverse time series transformations manually
def reverse_transformation(forecasts, original_training_data, transformation):
    """
    Used to reverse time series transformations such as detrending or deseasonalizing.

    :param forecasts: a list of pandas series, each with appropriate time indexes,
                i.e., must be indexes occurring after the last index of training data
    :param original_training_data: pandas dataframe with series in rows, periods in columns
    :param transformation: the desired transformation:
                - "Add Trend"
    :return fcasts: the transformed forecasts
    """

    if transformation=="Add Trend":
        # uses polynomial trend forecaster with degree 1 by default
        detrender = Detrender()
        # list for transformed forecasts
        fcasts = []
        # for each series,
        for i in range(len(forecasts)):
            detrender.fit(original_training_data.iloc[i,:])
            fcasts.append(detrender.inverse_transform(forecasts[i]))
        fcasts = pd.concat(fcasts, axis=1).T
    # return transformed forecasts
    return fcasts

###########################################################################
################### Fit a model and generate forecasts ####################
###########################################################################

def train_and_forecast(forecasting_model, horizon_length, training_data, window_length=None):
    """
    Performs model training and forecasting for the specified time series dataset.

    :param forecasting_model: the model to use to perform forecasts. Must have .fit and .predict methods
    :param horizon_length: the number of steps ahead you with to forecast relative to the last point in the series
    :param training_data: nested pandas dataframe with one column containing a series in each cell
    :return fcasts: a dataframe containing forecasts for all series
    """

    # define sktime relative forecasting horizon
    fh = np.arange(1, horizon_length+1)

    # list to store forecasts
    fcasts = []

    # if nested sktime compatible dataframe,
    if is_nested_dataframe(training_data):
        # for each series,
        for _, series in training_data.iterrows():

            # extract series
            s = series[0]

            # fit model
            forecasting_model.fit(s)

            # generate forecasts
            y_pred = forecasting_model.predict(fh)

            # store forecasts
            fcasts.append(y_pred)

        fcasts = pd.concat(fcasts, axis=1)

    # if training_data contains two dataframes, i.e., reduced train
    # and test for a global model
    else:
        # store number of time series and number of time periods
        num_series, num_periods = training_data.shape
        # reduced train and test data
        X_train, Y_train = reduce_train_test_global(train_data=np.array(training_data), window_length=window_length, h=horizon_length)
        # train global model
        forecasting_model.fit(X_train, Y_train)
        # select last window of training data
        ## this is equivalent to the last row of x_train rolled forward to include the last value of Y_train
        last_window = training_data.iloc[:,-window_length:]

        ## generate forecasts ##

        # store forecasted value based on last window of training data
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

        # convert forecasts into a list of pandas series
        fcasts = [pd.Series(i, index=np.arange(num_periods, num_periods+horizon_length)) for i in fcasts]

    return fcasts

##########################################################
################### Protection Methods ###################
##########################################################

# custom function to perform top or bottom coding for a univariate series
def coding_protection(sensitive_series, coding_type, percent_protected):
    """
    Performs top or bottom coding on a sensitive time series, returns the protected series.

    :param sensitive_series: confidential time series
    :param coding_type: whether to perform "Top" or "Bottom" coding
    :param percent_protected: the percentage of top or bottom observations to protect
    :return: protected_series: the series with top or bottom coding applied
    """
    if coding_type=="Bottom":
        q = np.quantile(sensitive_series, q=percent_protected)
        protected_series = pd.Series([i if i > q else q for i in sensitive_series])
    elif coding_type=="Top":
        q = np.quantile(sensitive_series, q=1-percent_protected)
        protected_series = pd.Series([i if i < q else q for i in sensitive_series])
    return protected_series

# custom function to perform additive noise protection (not differentially private)
def additive_noise_protection(sensitive_data, num_stdev):
    """
    Adds random noise to each sensitive series y sampled from 0 centered normal distribution,
    with standard deviation defined by num_stdev*standard_deviation(y).

    param: sensitive_data: pandas dataframe containing series in the rows and time
                periods in the columns.
    param: num_stdev: number of data standard deviations to use as standard deviation of
                random noise.
    return: P: pandas dataframe containing protected series.
    """

    # number of time series and number of time periods
    num_series, num_periods = sensitive_data.shape

    # calculate standard deviation of each series
    sigmas = np.std(sensitive_data, axis=1)

    # random normal draws to create r,
    # add to each y_j
    P = [(np.random.normal(loc=0, scale=sigmas[i]*num_stdev, size=num_periods) + sensitive_data.iloc[i,:]) for i in range(num_series)]

    # concatenate into dataframe
    P = pd.concat(P, axis=1).T

    return P

def DP_protection(sensitive_data, epsilon):
    """
    Adds random noise to each sensitive series y to achieve differential privacy,
    defined by privacy budget epsilon and the series-specific sensitivity approximated
    by the difference between the minimum and maximum values in the series.
    Noise is sampled from a Laplace distribution centered at 0, with scale = GS/epsilon.

    param: sensitive_data: pandas dataframe containing series in the rows and time
                periods in the columns.
    param: epsilon: Privacy budget. Larger values = less noise and less privacy.
    return: P: pandas dataframe containing protected series.
    """
    num_series, num_periods = sensitive_data.shape
    GS = (sensitive_data.max(axis=1) - sensitive_data.min(axis=1))/epsilon
    P = [np.random.laplace(loc=0, scale=GS[i], size=num_periods) + sensitive_data.iloc[i,:] for i in range(num_series)]
    P = pd.concat(P, axis=1).T
    return P

###################################################################
################### Results/Accuracy Comparison ###################
###################################################################

def forecast_accuracy_results(test_data, original_forecasts, protected_forecasts):
    """
    Compares the forecast accuracies of forecasts from the original confidential data,
    and forecasts from the protected data.

    param: test_data: (horizon, num_series) shaped dataframe with actual test values
    param: original_forecasts: (horizon, num_series) shaped dataframe with values
            forecasted based on the confidential training data
    param: protected_forecasts: (horizon, num_series) shaped dataframe with values
            forecasted based on the protected training data
    return: results_dict: Returns a dictionary of tuples containing various comparisons.
             The first and second values of each tuple are based on MAE and RMSE, respectively.
        "% Series with improved accuracy:"
        "% Series with worsened accuracy:"
        "% Series with unchanged accuracy:"
        "% Change global accuracy:"
    """

    ########## Comparisons of interest ##########
    # percentage of forecasts in each time step that were adjusted downward or upward
    percent_downward = ((original_forecasts-protected_forecasts)>0.0).mean().mean()
    percent_upward = ((original_forecasts-protected_forecasts)<0.0).mean().mean()

    # boolean values for whether a forecasted point was adjusted up or down after protection
    adjusted_up = original_forecasts < protected_forecasts
    adjusted_up = pd.concat([row for i, row in adjusted_up.iterrows()])
    adjusted_down = original_forecasts > protected_forecasts
    adjusted_down = pd.concat([row for i, row in adjusted_down.iterrows()])

    # point level absolute forecast error using original data
    absolute_error = np.absolute(test_data - original_forecasts)
    absolute_error = pd.concat([row for i, row in absolute_error.iterrows()])

    # point level absolute forecast error using protected data
    protected_absolute_error = np.absolute(test_data - protected_forecasts)
    protected_absolute_error = pd.concat([row for i, row in protected_absolute_error.iterrows()])

    # average absolute error for upward adjusted points prior to adjustment
    avg_up_prior = np.mean(absolute_error[adjusted_up])
    # average absolute error for downward adjusted points prior to adjustment
    avg_down_prior = np.mean(absolute_error[adjusted_down])

    # average absolute error for upward adjusted points after adjustment
    avg_up_post = np.mean(protected_absolute_error[adjusted_up])
    # average absolute error for downward adjusted points after adjustment
    avg_down_post = np.mean(protected_absolute_error[adjusted_down])

    ########## Metric Calculations ##########
    # calculate series level forecast accuracies for confidential training data
    local_mae = mean_absolute_error(test_data, original_forecasts, multioutput="raw_values")
    local_rmse = mean_squared_error(test_data, original_forecasts, multioutput="raw_values", square_root=True)

    # calculate series level forecast accuracies for protected training data
    protected_local_mae = mean_absolute_error(test_data, protected_forecasts, multioutput="raw_values")
    protected_local_rmse = mean_squared_error(test_data, protected_forecasts, multioutput="raw_values", square_root=True)

    # calculate global forecast accuracies for confidential training data
    global_mae = mean_absolute_error(test_data, original_forecasts, multioutput="uniform_average")
    global_rmse = mean_squared_error(test_data, original_forecasts, multioutput="uniform_average", square_root=True)

    # calculate global forecast accuracy for protected training data
    protected_global_mae = mean_absolute_error(test_data, protected_forecasts, multioutput="uniform_average")
    protected_global_rmse = mean_squared_error(test_data, protected_forecasts, multioutput="uniform_average", square_root=True)

    ########## Comparing Accuracies Before and After Protection ##########

    ## calculated tuples correspond to (MAE, RMSE) comparisons ##

    # percentage of series for which forecast accuracy improved under protection
    local_percent_improved = (np.mean(local_mae-protected_local_mae > 0.0),
                              np.mean(local_rmse-protected_local_rmse > 0.0))

    # percentage of series for which forecast accuracy worsened under protection
    local_percent_reduced = (np.mean(local_mae-protected_local_mae < 0.0),
                              np.mean(local_rmse-protected_local_rmse < 0.0))

    # percentage of series for which forecast accuracy stayed the same under protection
    # this is really only applicable to the SES model
    local_percent_equal = (np.mean(local_mae-protected_local_mae == 0.0),
                           np.mean(local_rmse-protected_local_rmse == 0.0))

    # percentage change in global accuracy
    percent_change_mean_accuracy = ((global_mae-protected_global_mae)/global_mae,
                                    (global_rmse-protected_global_rmse)/global_rmse)

    percent_change_median_accuracy = ((np.median(local_mae)-np.median(protected_local_mae))/np.median(local_mae),
                                      (np.median(local_rmse)-np.median(protected_local_rmse))/np.median(local_rmse))


    # average absolute error for upward adjusted points prior to adjustment
    avg_up_prior = np.mean(absolute_error[adjusted_up])
    # average absolute error for downward adjusted points prior to adjustment
    avg_down_prior = np.mean(absolute_error[adjusted_down])

    # average absolute error for upward adjusted points after adjustment
    avg_up_post = np.mean(protected_absolute_error[adjusted_up])
    # average absolute error for downward adjusted points after adjustment
    avg_down_post = np.mean(protected_absolute_error[adjusted_down])

    results_dict = {
        "Mean Accuracies": (global_mae, global_rmse),
        "Protected Mean Accuracies:": (protected_global_mae, protected_global_rmse),
        "% Change Mean accuracy:": percent_change_mean_accuracy,
        "% Change Median accuracy:": percent_change_median_accuracy,
        "% Forecasted Points adjusted downward:": percent_downward,
        "% Forecasted Points adjusted upward:": percent_upward,
        "% Series with improved accuracy:": local_percent_improved,
        "% Series with reduced accuracy:": local_percent_reduced,
        "Original Mean Absolute Error Upward Adjusted:": avg_up_prior,
        "Original Mean Absolute Error Downward Adjusted:": avg_down_prior,
        "Protected Mean Absolute Error Upward Adjusted:": avg_up_post,
        "Protected Mean Absolute Error Downward Adjusted:": avg_down_post
        # "% Series with unchanged accuracy:": local_percent_equal,
    }

    results_dict = {k: np.round(v, 4)*100 for k, v in results_dict.items()}

    return results_dict

###################################################################
################### Full Analysis Functions #######################
###################################################################

# these functions combine multiple previous functions to perform
# train-test split, data protection, forecasting, and accuracy Comparisons
# in one go

# perform full analysis for top or bottom coding
def full_coding_analysis(time_series_data, forecasting_model, forecast_horizon, coding_type=None, coding_percentage=None, num_stdev=None, window_length=None, epsilon=None):
    """
    Perform train-test split, data protection using top or bottom coding,
    forecasting, and accuracy comparisons in one go.

    param: time_series_data: (num_series, num_periods) shaped dataframe with time series data
    param: forecasting_model: the model to use for forecasting - must have .train() and .fit() methods
    param: forecast_horizon: how many time periods to forecast (not a rolling horizon) - this dictates
            how many periods are reserved from the end of each series for 'test' data
    param: coding_type: whether to perform top or bottom coding
    param: coding_percentage: what percentage of values should be top or bottom coded
    return: results_dict: Returns a dictionary of tuples containing various comparisons
             of the forecast accuracy using the unprotected vs. protected data.
             The first and second values of each tuple are based on MAE and RMSE, respectively.
        "% Series with improved accuracy:"
        "% Series with worsened accuracy:"
        "% Series with unchanged accuracy:"
        "% Change global accuracy:"
    """

    # create train-test split - we assume the 'test' values are unobserved
    # transpose the input data because `temporal_train_test_split` splits on rows
    Train, Test = temporal_train_test_split(time_series_data.T, test_size=forecast_horizon)
    # transpose back
    Train = Train.T
    Test = Test.T

    # generate protected training data
    if coding_type is not None:
        Train_protected = Train.apply(coding_protection, axis=1, args=(coding_type, coding_percentage))
    elif num_stdev is not None:
        Train_protected = additive_noise_protection(Train, num_stdev=num_stdev)
    elif epsilon is not None:
        Train_protected = DP_protection(Train, epsilon=epsilon)


    Train, Test = pre_process(Train, Test)
    Train_protected, _ = pre_process(Train_protected, Test)

    if type(forecasting_model) == lgb.sklearn.LGBMRegressor:
        #Train, Test = pre_process(Train, Test)
        #Train_protected, _ = pre_process(Train_protected, Test)
        # construct detrender
        detrender = Detrender()
        detrended_series = [detrender.fit_transform(series) for _ , series in Train_protected.iterrows()]
        Train_protected_orig = Train_protected.copy()
        Train_protected = pd.concat(detrended_series, axis=1).T
        detrended_series = [detrender.fit_transform(series) for _ , series in Train.iterrows()]
        Train_orig = Train.copy()
        Train = pd.concat(detrended_series, axis=1).T

    if type(forecasting_model) == sktime.forecasting.compose._pipeline.TransformedTargetForecaster:
        # create nested dataframes to use with sktime functions
        Train = from_2d_array_to_nested(Train)
        Train_protected = from_2d_array_to_nested(Train_protected)

    # forecasts from model trained on original data
    fcasts = train_and_forecast(forecasting_model=forecasting_model, horizon_length=forecast_horizon, training_data=Train, window_length=window_length)

    # forecasts from model trained on protected data
    fcasts_protected = train_and_forecast(forecasting_model=forecasting_model, horizon_length=forecast_horizon, training_data=Train_protected, window_length=window_length)

    if type(forecasting_model) == lgb.sklearn.LGBMRegressor:
        fcasts = reverse_transformation(fcasts, Train_orig, "Add Trend").T
        fcasts_protected = reverse_transformation(fcasts_protected, Train_protected_orig, "Add Trend").T

    # forecast accuracy results
    results_dict = forecast_accuracy_results(Test.T, fcasts, fcasts_protected)

    return results_dict, Test.T, fcasts, fcasts_protected
