##### This file generates and saves forecasts to .csv files for all models of
##### interest.

##### Author: Cameron Bale

################################################################################

from forecasting_functions import *
import csv
import os

# change this option to True for the wikipedia data
round = False

horizons = [1, 18]

protection_methods = {"AN": [1]}

# protection_methods = {"Top": [0.10, 0.20, 0.40],
#                       "Bottom": [0.10, 0.20, 0.40],
#                       "AN": [0.5, 1, 1.5, 2],
#                       "DP": [0.1, 1, 4.6, 10, 20]}

forecasting_models = {# "SES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "add_one":True, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "rnn_options":None},
                      # "DES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "add_one":True, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "rnn_options":None},
                      # "TES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "add_one":True, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "rnn_options":None},
                      # "Multivariate_LGBM": {"window_length":28, "make_stationary":False, "seasonality_type":"additive", "sp":7, "remove_seasonality":True, "add_one":True, "mean_normalize":True, "log":True, "detrend":False, "param_grid": {'learning_rate': [0.025, 0.05, 0.075, 0.10], 'num_boost_round': [100, 500, 1000, 1200, 1500]}, "rnn_options":None}}
                      # "ARIMA": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":12, "remove_seasonality":False, "add_one":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid":None, "rnn_options":None}}
                      # "Univariate_LGBM": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "add_one":True, "mean_normalize":False, "log":True, "detrend":False, "param_grid": {'estimator__learning_rate': [0.025, 0.05, 0.075, 0.10], 'estimator__n_estimators': [100, 250, 500]}, "rnn_options":None},
                      "VAR": {"window_length":None, "make_stationary":True, "seasonality_type":None, "sp":12, "remove_seasonality":False, "add_one":False, "mean_normalize":False, "log":True, "detrend":False, "param_grid": None, "rnn_options": None}}
                      # "RNN": {"window_length":None, "make_stationary":False, "seasonality_type":"additive", "sp":7, "remove_seasonality":False, "add_one":True, "mean_normalize":True, "log":True, "detrend":True, "param_grid":None, "rnn_options": {'input_chunk_length': 28, 'training_length': 35, 'max_samples_per_ts': 10, 'num_ensemble_models': 10}}}

# forecasts_path = "../../Outputs/Forecasts/Wikipedia/"
# results_path = "../../Outputs/Results/Wikipedia/"

forecasts_path = "../../Outputs/Forecasts/"
results_path = "../../Outputs/Results/"

# import M3 monthly micro series
# ignore header and skip the first row to use integers as column names
# full_data = pd.read_csv("../../Data/Train/Clean/full_wikipedia_clean.csv", header=None, skiprows=1).iloc[:,-150:]
full_data = pd.read_csv("../../Data/Train/Clean/full_m3_monthly_micro_clean.csv", header=None, skiprows=1)

# convert to a list of series, potentially with different lengths
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

    Test.to_csv(forecasts_path + "Test_h" + str(H) + ".csv", index=False)

    # for each forecasting model, generate and save forecasts using the original
    # data.

    # original_mae_vals = []

    for m in forecasting_models.items():
        if False: # m[0] == "Multivariate_LGBM":
            temp = None
            # fcasts_original = [full_forecast_analysis(Y=Y,
            #                                           h=i,
            #                                           forecasting_model=m[0],
            #                                           window_length=m[1]["window_length"],
            #                                           make_stationary=m[1]["make_stationary"],
            #                                           seasonality_type=m[1]["seasonality_type"],
            #                                           sp=m[1]["sp"],
            #                                           remove_seasonality=m[1]["remove_seasonality"],
            #                                           add_one=m[1]["add_one"],
            #                                           mean_normalize=m[1]["mean_normalize"],
            #                                           log=m[1]["log"],
            #                                           detrend=m[1]["detrend"],
            #                                           param_grid=m[1]["param_grid"],
            #                                           round=round) for i in range(1, H+1)]
            #
            # # combine fcast dataframes into one
            # fcasts_original = pd.concat(fcasts_original, axis=0)

            # code to combine the forecasts in fcasts into one dataframe
            # then implement the same thing after the data protection methods

        else:
            fcasts_original = full_forecast_analysis(Y=Y,
                                                     h=H,
                                                     forecasting_model=m[0],
                                                     window_length=m[1]["window_length"],
                                                     make_stationary=m[1]["make_stationary"],
                                                     seasonality_type=m[1]["seasonality_type"],
                                                     sp=m[1]["sp"],
                                                     remove_seasonality=m[1]["remove_seasonality"],
                                                     add_one=m[1]["add_one"],
                                                     mean_normalize=m[1]["mean_normalize"],
                                                     log=m[1]["log"],
                                                     detrend=m[1]["detrend"],
                                                     param_grid=m[1]["param_grid"],
                                                     rnn_options=m[1]["rnn_options"],
                                                     round=round)

        fcasts_original.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_original.csv", index=False)

        mae_global = pd.Series(mean_absolute_error(Test, fcasts_original, multioutput="uniform_average"))

        mae_global.to_csv(results_path + m[0] + "_h" + str(H) + "_original.csv", index=False)

        # original_file_name = "../../Outputs/Results/original_" + m[0] + "_h" + str(H) + ".csv"

        # append_write = 'a' # append if already exists or create if not
        #
        # with open(original_file_name, append_write) as f:
        #     writer = csv.writer(f)
        #     writer.writerow([m[0], 'MAE', mae_global])

        print("MAE for " + str(m[0]) + ": " + str(mae_global[0]))

        # original_mae_vals.append(mae_global)

    for p in protection_methods.items():

        protected_mae_vals = []
        params = p[1]
        for param in params:
            print(param)
            protection_method = p[0] + "_" + str(param)
            if p[0] == "Top":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
                noisy_protection=False
            elif p[0] == "Bottom":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
                noisy_protection=False
            elif p[0] == "AN":
                Y_protected = apply_data_protection(Y, num_stdev=param)
                noisy_protection=True
            elif p[0] == "DP":
                Y_protected = apply_data_protection(Y, epsilon=param)
                noisy_protection=True
            for m in forecasting_models.items():
                if False: #m[0] == "Multivariate_LGBM":
                    temp = None
                    # fcasts_protected = [full_forecast_analysis(Y=Y_protected,
                    #                                            h=i,
                    #                                            forecasting_model=m[0],
                    #                                            window_length=m[1]["window_length"],
                    #                                            make_stationary=m[1]["make_stationary"],
                    #                                            seasonality_type=m[1]["seasonality_type"],
                    #                                            sp=m[1]["sp"],
                    #                                            remove_seasonality=m[1]["remove_seasonality"],
                    #                                            add_one=m[1]["add_one"],
                    #                                            mean_normalize=m[1]["mean_normalize"],
                    #                                            log=m[1]["log"],
                    #                                            param_grid=m[1]["param_grid"],
                    #                                            round=round) for i in range(1, H+1)]
                    #
                    # # combine fcast dataframes into one
                    # fcasts_protected = pd.concat(fcasts_protected, axis=0)

                else:
                    fcasts_protected = full_forecast_analysis(Y=Y_protected,
                                                              h=H,
                                                              forecasting_model=m[0],
                                                              window_length=m[1]["window_length"],
                                                              make_stationary=m[1]["make_stationary"],
                                                              seasonality_type=m[1]["seasonality_type"],
                                                              sp=m[1]["sp"],
                                                              remove_seasonality=m[1]["remove_seasonality"],
                                                              add_one=m[1]["add_one"],
                                                              mean_normalize=m[1]["mean_normalize"],
                                                              log=m[1]["log"],
                                                              detrend=m[1]["detrend"],
                                                              param_grid=m[1]["param_grid"],
                                                              noisy_protection=noisy_protection,
                                                              rnn_options=m[1]["rnn_options"],
                                                              round=round)

                fcasts_protected.to_csv(forecasts_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)

                mae_global_protected = pd.Series(mean_absolute_error(Test, fcasts_protected, multioutput="uniform_average"))

                mae_global_protected.to_csv(results_path + m[0] + "_h" + str(H) + "_" + protection_method + ".csv", index=False)





                # fcasts_protected.to_csv("../../Outputs/Forecasts/" + str(m[0]) + "_" + "h" + str(H) + "_" + protection_method + ".csv", index=False)

                print("MAE under " + protection_method + " for " + m[0] + ": " + str(mae_global_protected[0]))

                # protected_mae_vals.append(mae_global_protected)

        # full_params = params.copy()
        # full_params.insert(0, "original")
        # mae_vals = original_mae_vals + protected_mae_vals
        # param_col = np.repeat(full_params, len(forecasting_models))
        # model_col = list(forecasting_models.keys())*len(full_params)
        #
        # results = pd.DataFrame({"parameter": param_col, "model": model_col, "MAPE": mae_vals})
        #
        # results.to_csv("../../Outputs/Results/" + "h" + str(H) + "_" + p[0] + ".csv", index=False)
