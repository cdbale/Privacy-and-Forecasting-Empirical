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
protection_methods = {# "Top": [0.10, 0.20, 0.40],
                      # "Bottom": [0.10, 0.20, 0.40],
                      "original": [""],
                      "AN": [0.25, 0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20],
                      "k_nts": [3, 5, 7, 10, 15],
                      "k_nts_plus": [3, 5, 7, 10, 15]}

# forecast horizons
H = [1]
# H = [2]

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

data.to_csv(results_path + "Error_Distributions/all_distributions_h" + str(h) + ".csv", index=False)
