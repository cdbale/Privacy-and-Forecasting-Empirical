##### This file generates and saves forecasts to .csv files for all models of
##### interest.

##### Author: Cameron Bale

################################################################################

from forecasting_functions import *

horizons = [1, 18]

protection_methods = {"Top": [0.10],
                      "Bottom": [0.10],
                      "AN": [1],
                      "DP": [4.6]}

# protection_methods = {"Top": [0.10, 0.20, 0.40],
#                       "Bottom": [0.10, 0.20, 0.40],
#                       "AN": [0.5, 1, 2],
#                       "DP": [0.1, 1, 4.6, 10, 20]}

forecasting_models = {"SES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid":None},
                      "DES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid":None},
                      "TES": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid":None},
                      "Multivariate_LGBM": {"window_length":24, "make_stationary":False, "seasonality_type":"additive", "sp":12, "remove_seasonality":True, "mean_normalize":True, "log":True, "param_grid":None},
                      "ARIMA": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid":None},
                      "Univariate_LGBM": {"window_length":None, "make_stationary":False, "seasonality_type":None, "sp":None, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid": {"window_length": [6, 12, 24]}},
                      "VAR": {"window_length":None, "make_stationary":True, "seasonality_type":None, "sp":12, "remove_seasonality":False, "mean_normalize":False, "log":True, "param_grid": {"maxlags": [1, 2, 3]}}}

# import M3 monthly micro series
# ignore header and skip the first row to use integers as column names
full_data = pd.read_csv("../../Data/Train/Clean/full_m3_monthly_micro_clean.csv", header=None, skiprows=1)

# convert to a list of series, potentially with different lengths
full_data = [x.dropna() for _, x in full_data.iterrows()]

# loop over horizons
for h in horizons:
    # create training data excluding test horizon
    Y = [x.iloc[:-h] for x in full_data]
    # create test data
    Test = [x.iloc[-h:] for x in full_data]
    Test = pd.DataFrame([x.reset_index(drop=True) for x in Test]).T

    Test.to_csv("../../Outputs/Forecasts/Test_h" + str(h) + ".csv", index=False)

    # for each forecasting model, generate and save forecasts using the original
    # data.

    original_mae_vals = []

    for m in forecasting_models.items():
        fcasts_original = full_forecast_analysis(Y=Y,
                                                 h=h,
                                                 forecasting_model=m[0],
                                                 window_length=m[1]["window_length"],
                                                 make_stationary=m[1]["make_stationary"],
                                                 seasonality_type=m[1]["seasonality_type"],
                                                 sp=m[1]["sp"],
                                                 remove_seasonality=m[1]["remove_seasonality"],
                                                 mean_normalize=m[1]["mean_normalize"],
                                                 log=m[1]["log"],
                                                 param_grid=m[1]["param_grid"])

        fcasts_original.to_csv("../../Outputs/Forecasts/" + m[0] + "_" + "h" + str(h) + "_original.csv", index=False)

        mae_global = mean_absolute_error(Test, fcasts_original, multioutput="uniform_average")

        print("MAE for " + str(m[0]) + ": " + str(mae_global))

        original_mae_vals.append(mae_global)

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
                fcasts_protected = full_forecast_analysis(Y=Y_protected,
                                                          h=h,
                                                          forecasting_model=m[0],
                                                          window_length=m[1]["window_length"],
                                                          make_stationary=m[1]["make_stationary"],
                                                          seasonality_type=m[1]["seasonality_type"],
                                                          sp=m[1]["sp"],
                                                          remove_seasonality=m[1]["remove_seasonality"],
                                                          mean_normalize=m[1]["mean_normalize"],
                                                          log=m[1]["log"],
                                                          param_grid=m[1]["param_grid"],
                                                          noisy_protection=noisy_protection)

                fcasts_protected.to_csv("../../Outputs/Forecasts/" + str(m[0]) + "_" + "h" + str(h) + "_" + protection_method + ".csv", index=False)

                mae_global_protected = mean_absolute_error(Test, fcasts_protected, multioutput="uniform_average")

                print("MAE under " + protection_method + " for " + str(m[0]) + ": " + str(mae_global_protected))

                protected_mae_vals.append(mae_global_protected)

        full_params = params.copy()
        full_params.insert(0, "original")
        mae_vals = original_mae_vals + protected_mae_vals
        param_col = np.repeat(full_params, len(forecasting_models))
        model_col = list(forecasting_models.keys())*len(full_params)

        results = pd.DataFrame({"parameter": param_col, "model": model_col, "MAPE": mae_vals})

        results.to_csv("../../Outputs/Results/" + "h" + str(h) + "_" + p[0] + ".csv", index=False)
