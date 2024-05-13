### File to compile time series level accuracies for plotting with PCA

# Author: Cameron Bale

import pandas as pd
import numpy as np
import os

from sktime.performance_metrics.forecasting import mean_absolute_percentage_error

# results file path
results_path = "../../Outputs/Tables/"

# forecasts file path
forecasts_path = "../../Outputs/Forecasts/"

# names of forecast files
fcast_files = os.listdir(forecasts_path)

models = ["SES", "DES", "TES", "ARIMA", "VAR", "Multivariate_LGBM", "RNN"]

# protection methods dictionary
protection_methods = {"Top": [0.10, 0.20, 0.40],
                      "Bottom": [0.10, 0.20, 0.40],
                      "AN": [0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}

horizons = ["h1", "h18"]

for h in horizons:

    # get test data for horizon h
    test = pd.read_csv(forecasts_path + "Test_" + h + ".csv")

    # create list of file names of original forecasts for each model
    original_fcast_names = [f for f in fcast_files if h + "_original" in f]

    smape_original = []

    # get original forecast accuracies for each model
    for m in models:

        # calculate original sMAPE for each model
        [ofc] = [f for f in original_fcast_names if m in f]

        original_fcasts = pd.read_csv(forecasts_path + ofc)

        # calculate sMAPE for model M using original data
        smape_m_original = mean_absolute_percentage_error(test, original_fcasts, symmetric=True)

        # store sMAPE
        smape_original.append(smape_m_original*100)

    # loop over each protection method
    for p in protection_methods.items():

        data_dict = {"Model": models, "Original"+"_"+h: smape_original}

        # create list of file names for protected forecasts for each model
        protected_fcast_names = [f for f in fcast_files if h + "_" + p[0] in f]

        for param in p[1]:

            smape_protected = []

            # get file names specific to that parameter for all models
            param_specific_files = [f for f in protected_fcast_names if p[0] + "_" + str(param) + ".csv" in f]

            for m in models:

                # calculate series level MAE for each model
                [pfc] = [f for f in param_specific_files if m in f]

                protected_fcasts = pd.read_csv(forecasts_path + pfc)

                # calculate protected sMAPE
                smape_m_protected = mean_absolute_percentage_error(test, protected_fcasts, symmetric=True)

                # save protected sMAPE
                smape_protected.append(smape_m_protected*100)

            data_dict[p[0]+"_"+str(param)] = smape_protected

        dd = pd.DataFrame(data_dict)

        dd.to_csv(results_path + p[0] + "_" + h + "_sMAPE.csv", index=False)

        #     # original forecasts
        #     fcasts_orig = pd.read_csv(forecasts_path + original_fcasts_n[0])
        #     # original forecasts accuracies
        #     data_dict[m+"_original"] = mean_absolute_error(test, fcasts_orig, multioutput="raw_values")
        #
        #     for param in p[1]:
        #         param_n = [f for f in protected_fcasts_n if str(param) in f]
        #         fcasts_protected = pd.read_csv(forecasts_path + param_n[0])
        #
        #         data_dict[m+"_"+p[0]+"_"+str(param)] = mean_absolute_error(test, fcasts_protected, multioutput="raw_values")
        #
        # data = pd.DataFrame(data_dict)
        #
        # data.to_csv("../../Outputs/Tables/" + p[0] + "_ts_specific"+ ".csv", index=False)
    ###################################################################

    # # add H18 original accuracy measures to table
    # originals_h18 = [f for f in result_files if "h18_original" in f]
    # h18_accuracies = []
    #
    # for m in models:
    #     m_file = [f for f in originals_h18 if m in f]
    #     data = pd.read_csv(results_path + m_file[0])
    #     h18_accuracies.append(data.iloc[0,0])
    #
    # data_dict["Original_18"] = np.round(h18_accuracies, 2)
    #
    # ##########################################################
    #
    # for param in p[1]:
    #
    #     # add accuracy measures for h1 using protected data
    #     protected_h1 = [f for f in result_files if "h1_"+p[0]+"_"+str(param) in f]
    #     h1_accuracies = []
    #     # add accuracy measures for h18 using protected data
    #     protected_h18 = [f for f in result_files if "h18_"+p[0]+"_"+str(param) in f]
    #     h18_accuracies = []
    #
    #     for m in models:
    #         m_file_h1 = [f for f in protected_h1 if m in f]
    #         m_file_h18 = [f for f in protected_h18 if m in f]
    #         data_h1 = pd.read_csv(results_path + m_file_h1[0])
    #         data_h18 = pd.read_csv(results_path + m_file_h18[0])
    #         h1_accuracies.append(data_h1.iloc[0,0])
    #         h18_accuracies.append(data_h18.iloc[0,0])
    #
    #     data_dict["h1"+"_"+str(param)] = np.round(h1_accuracies, 2)
    #     data_dict["h18"+"_"+str(param)] = np.round(h18_accuracies, 2)
    #
    # ###############################################################
    #
    # data = pd.DataFrame(data_dict)
    #
    # data.to_csv("../../Outputs/Tables/" + p[0] + ".csv", index=False)
