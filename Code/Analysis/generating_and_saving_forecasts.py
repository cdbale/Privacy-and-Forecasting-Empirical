##### This file generates and saves forecasts to .csv files for all models of
##### interest.

##### Author: Cameron Bale

################################################################################

from forecasting_functions import *
import csv
import os

# whether to forecast using original data
forecast_for_confidential = True

# forecast horizons
horizons = [1, 18]

# dictionary containing string name for each protection method and list of
# parameters for each method
protection_methods = {"Top": [0.10, 0.20, 0.40],
                      "Bottom": [0.10, 0.20, 0.40],
                      "AN": [0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}

# dictionary containing string names of forecasting models and sub-dictionaries
# of model-specific parameters
forecasting_models = {# "SES": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None},
                      # "DES": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None},
                      # "TES": {"make_stationary":False, "seasonality_type":None, "sp":12, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None}}
                      # "ARIMA": {"make_stationary":False, "seasonality_type":None, "sp":12, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None}}
                      # "Multivariate_LGBM": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":True, "log":True, "detrend":False, "param_grid": None, "options": {'max_samples_per_ts': None, 'window_length': 25}},
                      "VAR": {"make_stationary":True, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "standardize":True, "detrend":False, "param_grid": None, "options": {"pre_processing": False}}}
                      # "Multivariate_LGBM": {"make_stationary":False, "seasonality_type":"additive", "sp":7, "remove_seasonality":True, "mean_normalize":True, "log":True, "detrend":False, "param_grid": {'learning_rate': [0.025, 0.05, 0.075, 0.10], 'num_boost_round': [100, 500, 1000, 1200, 1500]}, "rnn_options":None},
                      # "RNN": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":True, "log":True, "detrend":False, "param_grid":None, "options": {'input_chunk_length': 25, 'training_length': 30, 'max_samples_per_ts': 10, 'num_ensemble_models': 10}}}

forecasts_path = "../../Outputs/Forecasts/"
results_path = "../../Outputs/Results/"

# import M3 monthly micro series
# ignore header and skip the first row to use integers as column names
full_data = pd.read_csv("../../Data/Train/Clean/full_m3_monthly_micro_clean.csv", header=None, skiprows=1)

# convert to a list of series, and drop missing values
full_data = [x.dropna() for _, x in full_data.iterrows()]

###### for testing only ######
# full_data = full_data[:10]

# loop over horizons
for H in horizons:
    # create training data excluding test horizon
    Y = [x.iloc[:-H] for x in full_data]
    # create test data
    Test = [x.iloc[-H:] for x in full_data]
    Test = pd.DataFrame([x.reset_index(drop=True) for x in Test]).T

    # save test data
    Test.to_csv(forecasts_path + "Test_h" + str(H) + ".csv", index=False)

    # for each forecasting model, generate and save forecasts using the original
    # data.

    if forecast_for_confidential:

        metadata = {'protection_method': "original", 'protection_parameter': "none"}

        for m in forecasting_models.items():
            # if multivariate LGBM, we train one model for each step in the horizon
            if False: # m[0] == "Multivariate_LGBM":
                fcasts_original = [full_forecast_analysis(Y=Y,
                                                          h=i,
                                                          forecasting_model=m[0],
                                                          window_length=m[1]["window_length"],
                                                          make_stationary=m[1]["make_stationary"],
                                                          seasonality_type=m[1]["seasonality_type"],
                                                          sp=m[1]["sp"],
                                                          remove_seasonality=m[1]["remove_seasonality"],
                                                          mean_normalize=m[1]["mean_normalize"],
                                                          log=m[1]["log"],
                                                          detrend=m[1]["detrend"],
                                                          param_grid=m[1]["param_grid"],
                                                          options=m[1]["options"],
                                                          metadata=metadata) for i in range(1, H+1)]

                # combine fcast dataframes into one
                fcasts_original = pd.concat(fcasts_original, axis=0)

            else:
                fcasts_original = full_forecast_analysis(Y=Y,
                                                         h=H,
                                                         forecasting_model=m[0],
                                                         make_stationary=m[1]["make_stationary"],
                                                         seasonality_type=m[1]["seasonality_type"],
                                                         sp=m[1]["sp"],
                                                         remove_seasonality=m[1]["remove_seasonality"],
                                                         mean_normalize=m[1]["mean_normalize"],
                                                         log=m[1]["log"],
                                                         detrend=m[1]["detrend"],
                                                         param_grid=m[1]["param_grid"],
                                                         options=m[1]["options"],
                                                         metadata=metadata)

            if fcasts_original is None:
                continue

            # save the forecasts based on confidential data to .csv file
            fcasts_original.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_original.csv", index=False)

            # calculate the average mean absolute error
            mae_global = pd.Series(mean_absolute_error(Test, fcasts_original, multioutput="uniform_average"))

            # save the average mean absolute error
            mae_global.to_csv(results_path + m[0] + "_h" + str(H) + "_original.csv", index=False)

            # print the average mean absolute error
            print("MAE for " + str(m[0]) + ": " + str(mae_global[0]))

    # generate forecasts for each model and protection method combination
    for p in protection_methods.items():

        # list to store MAEs
        protected_mae_vals = []
        # store the parameter list for the protection method
        params = p[1]
        # for each parameter, apply the data protection to Y
        for param in params:
            print(param)
            protection_method = p[0] + "_" + str(param)
            metadata = {'protection_method': p[0], 'protection_parameter': str(param)}
            if p[0] == "Top":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
            elif p[0] == "Bottom":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
            elif p[0] == "AN":
                Y_protected = apply_data_protection(Y, num_stdev=param)
            elif p[0] == "DP":
                Y_protected = apply_data_protection(Y, epsilon=param)

            # forecast using each model on the current protected data
            for m in forecasting_models.items():
                if False: #m[0] == "Multivariate_LGBM":
                    fcasts_protected = [full_forecast_analysis(Y=Y_protected,
                                                               h=i,
                                                               forecasting_model=m[0],
                                                               make_stationary=m[1]["make_stationary"],
                                                               seasonality_type=m[1]["seasonality_type"],
                                                               sp=m[1]["sp"],
                                                               remove_seasonality=m[1]["remove_seasonality"],
                                                               mean_normalize=m[1]["mean_normalize"],
                                                               log=m[1]["log"],
                                                               param_grid=m[1]["param_grid"],
                                                               options=m[1]["options"],
                                                               metadata=metadata) for i in range(1, H+1)]

                    # combine fcast dataframes into one
                    fcasts_protected = pd.concat(fcasts_protected, axis=0)

                else:
                    fcasts_protected = full_forecast_analysis(Y=Y_protected,
                                                              h=H,
                                                              forecasting_model=m[0],
                                                              make_stationary=m[1]["make_stationary"],
                                                              seasonality_type=m[1]["seasonality_type"],
                                                              sp=m[1]["sp"],
                                                              remove_seasonality=m[1]["remove_seasonality"],
                                                              mean_normalize=m[1]["mean_normalize"],
                                                              log=m[1]["log"],
                                                              detrend=m[1]["detrend"],
                                                              param_grid=m[1]["param_grid"],
                                                              options=m[1]["options"],
                                                              metadata=metadata)

                if fcasts_protected is None:
                    continue

                # save protected forecasts to .csv
                fcasts_protected.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)

                # calculate average MAE
                mae_global_protected = pd.Series(mean_absolute_error(Test, fcasts_protected, multioutput="uniform_average"))

                # save average MAE to .csv
                mae_global_protected.to_csv(results_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)

                # print the average MAE for the current model and protection method
                print("MAE under " + protection_method + " for " + m[0] + ": " + str(mae_global_protected[0]))
