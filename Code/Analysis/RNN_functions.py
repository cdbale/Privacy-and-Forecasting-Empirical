# Functions for training an RNN model using the darts package

# train_RNN() trains a RNN and generates forecasts for the provided train_data
# optimize_RNN() uses Bayesian optimization to choose optimized values for
#   many of the RNN hyperparameters

# Author: Cameron Bale

from bayes_opt import BayesianOptimization
from torch import nn
from darts.models.forecasting.rnn_model import RNNModel
import numpy as np
import pandas as pd
from sktime.performance_metrics.forecasting import mean_absolute_error
import os

def train_RNN(train_data,
              h,
              num_ensemble_models,
              input_chunk_length,
              training_length,
              max_samples_per_ts,
              learning_rate_,
              n_rnn_layers_,
              hidden_dim_,
              batch_size_,
              n_epochs_,
              dropout_,
              L2_penalty_,
              save_models=False,
              model_save_folder=None,
              use_gpu=False):

    num_series = len(train_data)

    full_forecasts = np.zeros([h, num_series, num_ensemble_models])

    for m in range(num_ensemble_models):

        # optimizer kwargs
        optimizer_kwargs = {'lr': learning_rate_,
                            'weight_decay': L2_penalty_}

        # instantiate the model
        if use_gpu:
            RNN = RNNModel(
                input_chunk_length=input_chunk_length,
                training_length=training_length,
                model="LSTM",
                loss_fn=nn.L1Loss(),
                optimizer_kwargs=optimizer_kwargs,
                n_rnn_layers=n_rnn_layers_,
                hidden_dim=hidden_dim_,
                batch_size=batch_size_,
                n_epochs=n_epochs_,
                dropout=dropout_,
                force_reset=True,
                pl_trainer_kwargs={
                    "enable_progress_bar": False,
                    "accelerator": "gpu",
                    "precision": "32-true",
                    "devices": [0]
                })

        else:
            RNN = RNNModel(
                input_chunk_length=input_chunk_length,
                training_length=training_length,
                model="LSTM",
                loss_fn=nn.L1Loss(),
                optimizer_kwargs=optimizer_kwargs,
                n_rnn_layers=n_rnn_layers_,
                hidden_dim=hidden_dim_,
                batch_size=batch_size_,
                n_epochs=n_epochs_,
                dropout=dropout_,
                force_reset=True,
                pl_trainer_kwargs={
                    "accelerator": "cpu",
                    "enable_progress_bar": False
                })

        # fit the model
        RNN.fit(series=train_data, max_samples_per_ts=max_samples_per_ts)

        if save_models:
            newpath = "../../Outputs/RNN_models/" + model_save_folder + "/"
            if not os.path.exists(newpath):
                os.makedirs(newpath)
            RNN.save(newpath + "rnn_mod_" + str(m) + "_.pth.tar")

        # generate forecasts
        fcasts = RNN.predict(n=h, series=train_data)

        # convert to series
        fcasts = [x.pd_series().reset_index(drop=True) for x in fcasts]

        fcasts = pd.DataFrame(fcasts).T

        # store forecasts in appropriate indexes
        full_forecasts[:,:,m] = fcasts

    # take the median of the forecasts
    median_forecasts = pd.DataFrame(np.median(full_forecasts, axis=2))

    return median_forecasts


def optimize_RNN(train_data,
                 validation_data,
                 h,
                 num_ensemble_models,
                 input_chunk_length,
                 training_length,
                 max_samples_per_ts,
                 use_gpu=False):

    def evaluate_RNN(learning_rate_, n_rnn_layers_, hidden_dim_, batch_size_, n_epochs_, dropout_, L2_penalty_):

        fcasts = train_RNN(train_data=train_data,
                           h=h,
                           num_ensemble_models=num_ensemble_models,
                           input_chunk_length=input_chunk_length,
                           training_length=training_length,
                           max_samples_per_ts=max_samples_per_ts,
                           n_rnn_layers_=int(n_rnn_layers_),
                           learning_rate_=learning_rate_,
                           hidden_dim_=int(hidden_dim_),
                           batch_size_=int(batch_size_),
                           n_epochs_=int(n_epochs_),
                           dropout_=dropout_,
                           L2_penalty_=L2_penalty_,
                           use_gpu=use_gpu)

        # compute MAE
        return -mean_absolute_error(validation_data, fcasts)


    optimizer = BayesianOptimization(
        f=evaluate_RNN,
        pbounds={
            "learning_rate_": (0.001, 0.1),
            "n_rnn_layers_": (1, 2.99),
            "hidden_dim_": (20, 50.99),
            "batch_size_": (200, 700.99),
            "n_epochs_": (3, 30.99),
            "dropout_": (0.1, 0.5),
            "L2_penalty_": (0.0001, 0.0008)
        },
        random_state=1234,
        verbose=1)

    optimizer.maximize(n_iter=15)

    print("Final Result: ", optimizer.max)

    return optimizer.max
