### results that relate our forecasts to judgmental forecasting literature
# Author: Cameron Bale

from forecasting_functions import forecast_results
from sktime.performance_metrics.forecasting import mean_absolute_error
import pandas as pd
import numpy as np
import os

def AvgRelMAE(test_series, original_fcasts, protected_fcasts, t):
    r = [mean_absolute_error(test_series[i], protected_fcasts[i])/mean_absolute_error(test_series[i], original_fcasts[i]) for i, _ in enumerate(test_series)]
    n = [len(j) for i, j in enumerate(protected_fcasts)]
    nlogr = [n[i]*np.log(r[i]) for i, _ in enumerate(r)]
    nlogr.sort()
    trim_val = int(t*len(nlogr))
    nlogr = nlogr[trim_val:]
    nlogr = nlogr[:-trim_val]
    # return np.exp((1/np.sum(n))*np.sum(nlogr))
    return (1 - np.exp((1/np.sum(n))*np.sum(nlogr))) * 100

# results file path
results_path = "../../Outputs/Results/"
# forecasts file path
forecasts_path = "../../Outputs/Forecasts/"
# names of forecast files
fcast_files = os.listdir(forecasts_path)
# list of models
models = ["SES", "DES", "TES", "ARIMA", "VAR", "Multivariate_LGBM", "RNN"]
horizons = ["h1", "h18"]
# horizons = ["h1"]

# protection methods dictionary
protection_methods = {"Top": [0.10, 0.20, 0.40],
                      "Bottom": [0.10, 0.20, 0.40],
                      "AN": [0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}
# protection_methods = {"Top": [0.10],
#                       "Bottom": [0.10]}

data_dict = {}
data_dict["Values"] = ["AvgRelMAE_None", "AvgRelMAE_Up", "AvgRelMAE_Down", "Prop_Up", "Prop_Down", "Magnitude_Up", "Magnitude_Down"]

for m in models:

    for h in horizons:

        test_data = pd.read_csv(forecasts_path + "Test_" + h + ".csv")
        original_forecasts = pd.read_csv(forecasts_path + m + "_" + h + "_original.csv")

        # split into list
        test_data = [test_data[col] for col in test_data]
        original_forecasts = [original_forecasts[col] for col in original_forecasts]

        for p in protection_methods.items():

            for param in p[1]:

                vals = []

                fstr = m + "_" + h + "_" + p[0] + "_" + str(param) + ".csv"

                files = [f for f in fcast_files if fstr in f]

                protected_forecasts = pd.read_csv(forecasts_path+files[0])

                # split into list
                protected_forecasts = [protected_forecasts[col] for col in protected_forecasts]

                vals.append(AvgRelMAE(test_data, original_forecasts, protected_forecasts, 0.05))

                # booleans for adjustment direction
                adjusted_up = [protected_forecasts[i] > original_forecasts[i] for i, _ in enumerate(protected_forecasts)]
                adjusted_down = [protected_forecasts[i] < original_forecasts[i] for i, _ in enumerate(protected_forecasts)]

                ### Calculate AvgRelMAE for Positive Adjusted Forecasts
                test_up = [test_data[i][j] for i, j in enumerate(adjusted_up) if len(test_data[i][j]) > 0]

                orig_up = [original_forecasts[i][j] for i, j in enumerate(adjusted_up) if len(original_forecasts[i][j]) > 0]

                protected_up = [protected_forecasts[i][j] for i, j in enumerate(adjusted_up) if len(protected_forecasts[i][j]) > 0]

                if len(test_up) > 0:
                    vals.append(AvgRelMAE(test_up, orig_up, protected_up, 0.05))
                else:
                    vals.append(0.0)

                ### Calculate AvgRelMAE for Negative Adjusted Forecasts
                test_down = [test_data[i][j] for i, j in enumerate(adjusted_down) if len(test_data[i][j]) > 0]

                orig_down = [original_forecasts[i][j] for i, j in enumerate(adjusted_down) if len(original_forecasts[i][j]) > 0]

                protected_down = [protected_forecasts[i][j] for i, j in enumerate(adjusted_down) if len(protected_forecasts[i][j]) > 0]

                if len(test_down) > 0:
                    vals.append(AvgRelMAE(test_down, orig_down, protected_down, 0.05))
                else:
                    vals.append(0.0)

                ### Proportion of Forecasts Adjusted in Either Direction
                vals.append(np.mean(adjusted_up))
                vals.append(np.mean(adjusted_down))

                ### Magnitude of Adjustments
                absolute_size_up = [np.abs(orig_up[i] - protected_up[i]) for i, _ in enumerate(orig_up)]
                if len(absolute_size_up) > 0:
                    vals.append(np.mean(np.concatenate(absolute_size_up)))
                else:
                    vals.append(0)

                absolute_size_down = [np.abs(orig_down[i] - protected_down[i]) for i, _ in enumerate(orig_down)]
                if len(absolute_size_down) > 0:
                    vals.append(np.mean(np.concatenate(absolute_size_down)))
                else:
                    vals.append(0)

                data_dict[m + "_" + h + "_" + p[0] + "_" + str(param)] = vals

    print('Done with results for model: ' + m)

data = pd.DataFrame(data_dict)
data.to_csv("../../Outputs/Tables/adjustment_results.csv", index=False)

    # data_dict[m+"_original"]
#
#
# # loop over each protection method making a table for each
# for p in protection_methods.items():
#
#     # list to store data for table
#     data_dict = {"Model": models}
#
#     # add H1 original accuracy measures to table
#     originals_h1 = [f for f in result_files if "h1_original" in f]
#     h1_accuracies = []
#
#     # create list of file names of original forecasts for each model
#     original_fcast_names = [f for f in fcast_files if "h1_original" in f]
#
#     ################################ Here #####################################
#
#     for m in models:
#         # calculate MAE for each model
#         m_file = [f for f in originals_h1 if m in f]
#         data = pd.read_csv(results_path + m_file[0])
#         h1_accuracies.append(data.iloc[0,0])
#
#     data_dict["Original_1"] = np.round(h1_accuracies, 2)
#
#     ###################################################################
#
#     # add H18 original accuracy measures to table
#     originals_h18 = [f for f in result_files if "h18_original" in f]
#     h18_accuracies = []
#
#     for m in models:
#         m_file = [f for f in originals_h18 if m in f]
#         data = pd.read_csv(results_path + m_file[0])
#         h18_accuracies.append(data.iloc[0,0])
#
#     data_dict["Original_18"] = np.round(h18_accuracies, 2)
#
#     ##########################################################
#
#     for param in p[1]:
#
#         # add accuracy measures for h1 using protected data
#         protected_h1 = [f for f in result_files if "h1_"+p[0]+"_"+str(param) in f]
#         h1_accuracies = []
#         # add accuracy measures for h18 using protected data
#         protected_h18 = [f for f in result_files if "h18_"+p[0]+"_"+str(param) in f]
#         h18_accuracies = []
#
#         for m in models:
#             m_file_h1 = [f for f in protected_h1 if m in f]
#             m_file_h18 = [f for f in protected_h18 if m in f]
#             data_h1 = pd.read_csv(results_path + m_file_h1[0])
#             data_h18 = pd.read_csv(results_path + m_file_h18[0])
#             h1_accuracies.append(data_h1.iloc[0,0])
#             h18_accuracies.append(data_h18.iloc[0,0])
#
#         data_dict["h1"+"_"+str(param)] = np.round(h1_accuracies, 2)
#         data_dict["h18"+"_"+str(param)] = np.round(h18_accuracies, 2)
#
#     ###############################################################
#
#     data = pd.DataFrame(data_dict)
#
#     data.to_csv("../../Outputs/Tables/" + p[0] + ".csv", index=False)
