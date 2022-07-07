### File to compile results into tables

# Author: Cameron Bale

import pandas as pd
import numpy as np
import os

# results file path
results_path = "../../Outputs/Results/Wikipedia/"

# forecasts file path
forecasts_path = "../../Outputs/Forecasts/Wikipedia/"

# names of results files
result_files = os.listdir(results_path)

# names of forecast files
fcast_files = os.listdir(forecasts_path)

models = ["SES", "DES", "TES", "ARIMA", "VAR", "Multivariate_LGBM", "RNN"]

# protection methods dictionary
protection_methods = {"Top": [0.10, 0.20, 0.40],
                      "Bottom": [0.10, 0.20, 0.40],
                      "AN": [0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}

# loop over each protection method making a table for each
for p in protection_methods.items():

    # list to store data for table
    data_dict = {"Model": models}

    # add H1 original accuracy measures to table
    originals_h1 = [f for f in result_files if "h1_original" in f]
    h1_accuracies = []

    # create list of file names of original forecasts for each model
    original_fcast_names = [f for f in fcast_files if "h1_original" in f]

    ################################ Here #####################################

    for m in models:
        # calculate MAE for each model
        m_file = [f for f in originals_h1 if m in f]
        data = pd.read_csv(results_path + m_file[0])
        h1_accuracies.append(data.iloc[0,0])

    data_dict["Original_1"] = np.round(h1_accuracies, 2)

    ###################################################################

    # add H18 original accuracy measures to table
    # originals_h18 = [f for f in result_files if "h18_original" in f]
    # h18_accuracies = []

    # for m in models:
    #     m_file = [f for f in originals_h18 if m in f]
    #     data = pd.read_csv(results_path + m_file[0])
    #     h18_accuracies.append(data.iloc[0,0])
    #
    # data_dict["Original_18"] = np.round(h18_accuracies, 2)

    ##########################################################

    for param in p[1]:

        # add accuracy measures for h1 using protected data
        protected_h1 = [f for f in result_files if "h1_"+p[0]+"_"+str(param)+".csv" in f]
        h1_accuracies = []
        # add accuracy measures for h18 using protected data
        # protected_h18 = [f for f in result_files if "h18_"+p[0]+"_"+str(param)+".csv" in f]
        # h18_accuracies = []

        for m in models:
            m_file_h1 = [f for f in protected_h1 if m in f]
            # m_file_h18 = [f for f in protected_h18 if m in f]
            data_h1 = pd.read_csv(results_path + m_file_h1[0])
            # data_h18 = pd.read_csv(results_path + m_file_h18[0])
            h1_accuracies.append(data_h1.iloc[0,0])
            # h18_accuracies.append(data_h18.iloc[0,0])

        data_dict["h1"+"_"+str(param)] = np.round(h1_accuracies, 2)
        # data_dict["h18"+"_"+str(param)] = np.round(h18_accuracies, 2)

    ###############################################################

    data = pd.DataFrame(data_dict)

    data.to_csv("../../Outputs/Tables/Wikipedia/" + p[0] + ".csv", index=False)