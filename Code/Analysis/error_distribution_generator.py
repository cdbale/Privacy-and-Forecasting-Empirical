# File to generate distributions of forecast errors for each model and
# protection method.

import pandas as pd
import numpy as np
import os

from sktime.performance_metrics.forecasting import mean_absolute_error

# forecasts file path
forecasts_path = "../../Outputs/Forecasts/"

# results file path
results_path = "../../Outputs/Results/"

# names of forecast files
fcast_files = os.listdir(forecasts_path)

# list of forecasting models
models = ["SES", "DES", "TES", "ARIMA", "VAR", "Multivariate_LGBM", "RNN"]

# protection methods dictionary
protection_methods = {"original": [""],
                      "k_nts": [5, 10, 15],
                      "Top": [0.10, 0.20, 0.40],
                      "Bottom": [0.10, 0.20, 0.40],
                      "AN": [0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}

# forecast horizons
# H = [1, 18]
H = [1]

distribution_dict = {}

for m in models:

    for h in H:

        test_data = pd.read_csv(forecasts_path + "Test_h" + str(h) + ".csv")

        for p in protection_methods.items():

            for param in p[1]:

                if p[0] == "original":

                    fname = m + "_h" + str(h) + "_" + p[0] + ".csv"

                else:

                    fname = m + "_h" + str(h) + "_" + p[0] + "_" + str(param) + ".csv"

                fcasts = pd.read_csv(forecasts_path + fname, header=0)

                mae_vals = pd.DataFrame(mean_absolute_error(test_data, fcasts, multioutput="raw_values"))

                distribution_dict[fname] = mae_vals.squeeze()

                mae_vals.to_csv(results_path + "Error_Distributions/" "error_distribution_" + fname, index=False)

        print("Distribution calculated for " + m + " for horizon " + str(h))

data = pd.DataFrame(distribution_dict)
data.to_csv(results_path + "Error_Distributions/all_distributions.csv", index=False)
# for h in H:
#
#         horizon_fcasts = [f for f in fcast_files if str(h)+"_" in f]
#
#         for m in models:
#
#             model_fcasts = [f for f in horizon_fcasts if m in f]
#
#             for p in protection_methods.items():
#
#                 protection_method = p[0]
#
#                 protection_parameters = p[1]
#
#                 method_fcasts = [f for f in model_fcasts if protection_method in f]
#
#                 for param in p[1]:
#
#                     param_fcasts = [f for f in method_fcasts if str(param) in f]
#
#                     print(param_fcasts)
