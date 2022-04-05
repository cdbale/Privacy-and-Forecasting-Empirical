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
