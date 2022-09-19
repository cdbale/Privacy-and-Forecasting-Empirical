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
