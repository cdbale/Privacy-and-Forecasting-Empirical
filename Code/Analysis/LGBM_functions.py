from bayes_opt import BayesianOptimization
from torch import nn
from darts.models.forecasting.gradient_boosted_model import LightGBMModel
import numpy as np
import pandas as pd
from sktime.performance_metrics.forecasting import mean_absolute_error

# make versions of these for LGBM

def train_LGBM(train_data,
               h,
               lags,
               max_samples_per_ts,
               learning_rate_,
               num_boost_rounds_):

    num_series = len(train_data)

    # set model parameters
    params = {"objective": "mae",
              "metrics": "mae",
              "force_col_wise": "true",
              "learning_rate": learning_rate_}

    # instantiate the model
    LGBM = LightGBMModel(lags=lags, learning_rate=learning_rate_, n_estimators=int(num_boost_rounds_))

    # fit the model
    LGBM.fit(series=train_data, max_samples_per_ts=max_samples_per_ts)

    # generate forecasts
    fcasts = LGBM.predict(n=h, series=train_data)

    # convert to series
    fcasts = [x.pd_series().reset_index(drop=True) for x in fcasts]

    fcasts = pd.DataFrame(fcasts).T

    return fcasts

def optimize_LGBM(train_data,
                  validation_data,
                  h,
                  lags,
                  max_samples_per_ts):

    def evaluate_LGBM(learning_rate_, num_boost_rounds_):

        fcasts = train_LGBM(train_data=train_data,
                            h=h,
                            lags=lags,
                            max_samples_per_ts=max_samples_per_ts,
                            learning_rate_=learning_rate_,
                            num_boost_rounds_=int(num_boost_rounds_))

        # compute MAE
        return -mean_absolute_error(validation_data, fcasts)

    optimizer = BayesianOptimization(
        f=evaluate_LGBM,
        pbounds={
            "learning_rate_": (0.025, 0.1),
            "num_boost_rounds_": (50, 500.99)
        },
        random_state=1234,
        verbose=1)

    optimizer.maximize(alpha=1e-1)

    print("Final Result: ", optimizer.max)

    return optimizer.max
