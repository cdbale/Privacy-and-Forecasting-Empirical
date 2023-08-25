#### File for all analysis in IJF paper. ####

#### Author: Cameron Bale ####

import os, time
from data_protection_functions import *
from forecasting_functions import *

#################### Step 0: Clean and prep data. See `Data Cleaning (M3).ipynb`

########### Step 1: Create protected data sets for forecasting second to last
########### time period. Used for feature selection process prior to protecting
########### and forecasting for the last period.

# # we only create baseline protected data sets using additive noise and
# # differential privacy
# protection_methods = {"AN": [0.25, 0.5, 1, 1.5, 2],
#                       "DP": [0.1, 1, 4.6, 10, 20]}
#
# # for the h2 horizon, apply the data protection methods and save the protected
# # data sets
#
# # path to cleaned original data
cleaned_data_path = "../../Data/Cleaned/"
# cleaned_files = os.listdir(cleaned_data_path)
#
# # training data for feature selection
# h2_train_files = [x for x in cleaned_files if "_h2_train" in x]
#
# baseline_protection_computation_time = {}
#
# # for each training file, create additive noise protected data
# for f in h2_train_files:
#     # for each protection method (AN and DP)
#     for p in protection_methods.items():
#         # vector of privacy parameters
#         params = p[1]
#         # for each protection method parameter
#         for param in params:
#             protection_method = p[0] + "_" + str(param)
#
#             if p[0] == "AN":
#                 start = time.time()
#                 temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
#                                               save_data_path=cleaned_data_path + protection_method + "_" + f,
#                                               num_stdev=param)
#                 stop = time.time()
#                 # track computation time of baseline protection
#                 baseline_protection_computation_time[protection_method + "_" + f] = stop-start
#
#             elif p[0] == "DP":
#                 start = time.time()
#                 temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
#                                               save_data_path=cleaned_data_path + protection_method + "_" + f,
#                                               epsilon=param)
#                 stop = time.time()
#                 # track computation time of baseline protection
#                 baseline_protection_computation_time[protection_method + "_" + f] = stop-start
#
# # save the computation time for baseline protected data sets
# baseline_protection_computation_time = pd.DataFrame(baseline_protection_computation_time.items(), columns=['File', 'Baseline Protection Time'])
# baseline_protection_computation_time.to_csv("../../Data/Computation Results/computation_time.csv", index=False)

################################################################################

################ Step 2: Forecast for the second to last period using the
################ original and baseline protected datasets.

# use a function that accepts a forecasting model dictionary, a training
# data file, a testing data file, and generates a forecast and forecast accuracy
# measure.

# now we store all the cleaned data file names including the baseline
# protected data sets
cleaned_files = os.listdir(cleaned_data_path)

# training data for feature selection
h2_train_files = [x for x in cleaned_files if "_h2_train" in x]
h2_test_files = [x for x in cleaned_files if "_h2_test" in x]

# train_file = "monthly-MICRO_h1_train.csv"
# test_file = "monthly-MICRO_h1_test.csv"
forecasts_path = "../../Outputs/Forecasts/"
results_path = "../../Outputs/Results/"

for f in h2_train_files:
    test_file = [x for x in h2_test_files if x[:-9] in f]
    [test_file] = test_file

    generate_and_save_forecasts(train_file=f,
                                test_file=test_file,
                                forecasts_path=forecasts_path,
                                results_path=results_path,
                                model="SES",
                                model_args= {"make_stationary":False, "sp":None, "mean_normalize":False, "log":True, "options":None},
                                h=1)

print('Done.')
# loop over forecasting models and training data files

# evaluate forecast accuracy using test data files

#
