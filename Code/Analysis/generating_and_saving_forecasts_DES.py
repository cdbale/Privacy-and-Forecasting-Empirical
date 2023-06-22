##### This file generates and saves forecasts to .csv files for all models of
##### interest.

##### Author: Cameron Bale

################################################################################

from forecasting_functions import *
import csv
import os

specific_model = "DES"

# whether to forecast using original data
forecast_for_confidential = False

# forecast horizons - index corresponds to the period we
# are generating a forecast for
target_forecast_period = [1]

# dictionary containing string name for each protection method and list of
# parameters for each method
protection_methods = {# "Top": [0.10, 0.20, 0.40],
                      # "Bottom": [0.10, 0.20, 0.40],
                      # "AN": [0.25, 0.5, 1, 1.5, 2],
                      # "DP": [0.1, 1, 4.6, 10, 20],
                      # "k_nts": [3, 5, 7, 10, 15],
                      "k_nts_plus": [3, 5, 7, 10, 15]}

# protection_methods = {}

# dictionary containing string names of forecasting models and sub-dictionaries
# of model-specific parameters
forecasting_models = {# "SES": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None},
                      "DES": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None}}
                      # "TES": {"make_stationary":False, "seasonality_type":None, "sp":12, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None},
                      # "ARIMA": {"make_stationary":False, "seasonality_type":None, "sp":12, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "options":None},
                      # "VAR": {"make_stationary":True, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid": None, "options": None},
                      # "Multivariate_LGBM": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":True, "log":True, "detrend":False, "param_grid": None, "options": {'max_samples_per_ts': None, 'window_length': 25}},
                      # "RNN": {"make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":True, "log":True, "detrend":False, "param_grid":None, "options": {'input_chunk_length': 25, 'training_length': 30, 'max_samples_per_ts': 10, 'num_ensemble_models': 10}}}

forecasts_path = "../../Outputs/Forecasts/"
results_path = "../../Outputs/Results/"

# import M3 monthly micro series
# ignore header and skip the first row to use integers as column names
full_data = pd.read_csv("../../Data/Train/Clean/full_m3_monthly_micro_clean.csv", header=None, skiprows=1)

# convert to a list of series, and drop missing values
full_data = [x.dropna() for _, x in full_data.iterrows()]

###### for testing only ######
# full_data = full_data[:15]

# loop over target forecast periods (assume a one-step horizon)
for H in target_forecast_period:

    # create training data excluding test horizon
    Y = [x.iloc[:-H] for x in full_data]
    # create test data
    if H == 1:
        Test = [x.iloc[-H:] for x in full_data]
    else:
        Test = [x.iloc[-H:-(H-1)] for x in full_data]

    Test = pd.DataFrame([x.reset_index(drop=True) for x in Test]).T

    # save test data
    Test.to_csv(forecasts_path + "Test_h" + str(H) + ".csv", index=False)

    # for each forecasting model, generate and save forecasts using the original
    # data.

    if forecast_for_confidential:

        metadata = {'protection_method': "original", 'protection_parameter': "none"}

        for m in forecasting_models.items():

            fcasts_original, fitted_values = full_forecast_analysis(Y=Y,
                                                                    h=1,
                                                                    target_forecast_period=H,
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

            # save the forecasts and fitted values based on confidential data to .csv file
            fcasts_original.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_original.csv", index=False)
            fitted_values.T.to_csv(forecasts_path + "fitted_values/" + m[0] + "_h" + str(H) + "_original.csv", index=False)

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
            elif p[0] == "k_nts":
                Y_protected = apply_data_protection(Y, k=param)
            elif p[0] == "k_nts_plus":
                Y_protected = apply_data_protection(Y, k=param, plus=True, model=specific_model)

            # forecast using each model on the current protected data
            for m in forecasting_models.items():
                fcasts_protected, fitted_values = full_forecast_analysis(Y=Y_protected,
                                                                         h=1,
                                                                         target_forecast_period=H,
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

                if specific_model != None:

                    # save protected forecasts to .csv
                    fcasts_protected.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_" + "model_specific_" + specific_model + "_" + protection_method + ".csv", index=False)
                    fitted_values.T.to_csv(forecasts_path + "fitted_values/" + m[0] + "_h" + str(H) + "_" + "model_specific_" + specific_model + "_" +  protection_method + ".csv", index=False)

                    # calculate average MAE
                    mae_global_protected = pd.Series(mean_absolute_error(Test, fcasts_protected, multioutput="uniform_average"))

                    # save average MAE to .csv
                    mae_global_protected.to_csv(results_path + m[0] + "_h" + str(H) + "_" + "model_specific_" + specific_model + "_" +  protection_method + ".csv", index=False)

                    # print the average MAE for the current model and protection method
                    print("MAE under " + protection_method + " for " + m[0] + ": " + str(mae_global_protected[0]))

                else:
                    # save protected forecasts to .csv
                    fcasts_protected.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)
                    fitted_values.T.to_csv(forecasts_path + "fitted_values/" + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)

                    # calculate average MAE
                    mae_global_protected = pd.Series(mean_absolute_error(Test, fcasts_protected, multioutput="uniform_average"))

                    # save average MAE to .csv
                    mae_global_protected.to_csv(results_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)

                    # print the average MAE for the current model and protection method
                    print("MAE under " + protection_method + " for " + m[0] + ": " + str(mae_global_protected[0]))
