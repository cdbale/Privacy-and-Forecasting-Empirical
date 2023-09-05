from bayes_opt import BayesianOptimization
from torch import nn
from darts.models.forecasting.lgbm import LightGBMModel
import numpy as np
import pandas as pd
import os
from sktime.performance_metrics.forecasting import mean_absolute_error
import joblib

# make versions of these for LGBM

def train_LGBM(train_data,
               h,
               lags,
               max_samples_per_ts,
               learning_rate_,
               num_boost_rounds_,
               num_leaves_,
               bagging_freq_,
               bagging_frac_,
               lambda_l2_,
               min_data_in_leaf_):

    num_series = len(train_data)

    # instantiate the model
    LGBM = LightGBMModel(lags=lags,
                         objective="mae",
                         metrics="mae",
                         learning_rate=learning_rate_,
                         n_estimators=int(num_boost_rounds_),
                         num_leaves=num_leaves_,
                         subsample_freq=int(bagging_freq_),
                         subsample=bagging_frac_,
                         reg_lambda=lambda_l2_,
                         min_child_samples=min_data_in_leaf_)

    # fit the model
    LGBM.fit(series=train_data, max_samples_per_ts=max_samples_per_ts)

    # if save_models:
    #     newpath = "../../Outputs/LGBM_models/" + model_save_folder + "/"
    #     if not os.path.exists(newpath):
    #         os.makedirs(newpath)
    #     joblib.dump(LGBM, newpath + "lgbm_mod_" + ".pkl")

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

    def evaluate_LGBM(learning_rate_, num_boost_rounds_, num_leaves_, bagging_freq_, bagging_frac_, lambda_l2_, min_data_in_leaf_):

        fcasts = train_LGBM(train_data=train_data,
                            h=h,
                            lags=lags,
                            max_samples_per_ts=max_samples_per_ts,
                            learning_rate_=learning_rate_,
                            num_boost_rounds_=int(num_boost_rounds_),
                            num_leaves_=int(num_leaves_),
                            bagging_freq_=bagging_freq_,
                            bagging_frac_=bagging_frac_,
                            lambda_l2_=lambda_l2_,
                            min_data_in_leaf_=int(min_data_in_leaf_))

        # compute MAE
        return -mean_absolute_error(validation_data, fcasts)

    optimizer = BayesianOptimization(
        f=evaluate_LGBM,
        pbounds={
            "learning_rate_": (0.01, 0.1),
            "num_boost_rounds_": (50, 1000.99),
            "num_leaves_": (2, 100.99),
            "bagging_freq_": (1, 5.99),
            "bagging_frac_": (0.01, 1),
            "lambda_l2_": (0, 0.5),
            "min_data_in_leaf_": (3, 100.99)
        },
        random_state=1234,
        verbose=1)

    optimizer.maximize(n_iter=15)

    print("Final Result: ", optimizer.max)

    return optimizer.max
