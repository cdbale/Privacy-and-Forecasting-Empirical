#### File for all analysis in IJF paper. ####

#### Author: Cameron Bale ####

import time
from data_protection_functions import *
from forecasting_functions import *
from error_distribution_generator import *

data_folder = "M4/"

################# Step 0: Clean and prep data. See `Data Cleaning (M4).ipynb`

######## Step 1: Create protected data sets for forecasting the second to last period
######## and the last period.

# # create protected data sets using additive noise and differential privacy
# protection_methods = {"AN": [0.25, 0.5, 1, 1.5, 2],
#                       "DP": [0.1, 1, 4.6, 10, 20]}

# # for the h1 and h2 horizon, apply the data protection methods and save the protected
# # data sets

# # path to cleaned original data
# cleaned_data_path = "../../Data/Cleaned/M4/"

# cleaned_files = os.listdir(cleaned_data_path)

# # training data for feature selection

# # separate the h2 training files since we need to track computation time for these
# h2_train_files = [x for x in cleaned_files if "_h2_train" in x]

# h1_train_files = [x for x in cleaned_files if "_h1_train" in x]

# # computation_time = {}

# # # for each training file, create additive noise protected data
# # for i, f in enumerate(h2_train_files):
    
# #     # for each protection method (AN and DP)
# #     for p in protection_methods.items():
# #         # vector of privacy parameters
# #         params = p[1]
# #         # for each protection method parameter
# #         for param in params:
            
# #             protection_method = p[0] + "_" + str(param)

# #             if p[0] == "AN":
# #                 start = time.time()
# #                 time.sleep(0.01)
# #                 temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
# #                                               save_data_path=cleaned_data_path + protection_method + "_" + f,
# #                                               num_stdev=param)
# #                 stop = time.time()
# #                 # track computation time of baseline protection
# #                 computation_time[protection_method + "_" + f] = stop-start-0.01
                
# #                 temp = save_protected_dataset(original_data_path=cleaned_data_path + h1_train_files[i],
# #                                               save_data_path=cleaned_data_path + protection_method + "_" + h1_train_files[i],
# #                                               num_stdev=param)

# #             elif p[0] == "DP":
# #                 start = time.time()
# #                 time.sleep(0.01)
# #                 temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
# #                                               save_data_path=cleaned_data_path + protection_method + "_" + f,
# #                                               epsilon=param)
# #                 stop = time.time()
# #                 # track computation time of baseline protection
# #                 computation_time[protection_method + "_" + f] = stop-start-0.01
                
# #                 temp = save_protected_dataset(original_data_path=cleaned_data_path + h1_train_files[i],
# #                                               save_data_path=cleaned_data_path + protection_method + "_" + h1_train_files[i],
# #                                               epsilon=param)

# # # save the computation time for baseline protected data sets
# # computation_time = pd.DataFrame(computation_time.items(), columns=['File', 'Baseline Protection Time'])
# # computation_time.to_csv("../../Data/Computation Results/M4_computation_time.csv", index=False)

computation_time = pd.read_csv("../../Data/Computation Results/M4_computation_time.csv")

# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################
# ###############################################################################

# ############### Step 2: Forecast for the second to last period using the
# ############### original and baseline protected datasets.

# # now we store all the cleaned data file names including the baseline
# # protected data sets
cleaned_data_path = "../../Data/Cleaned/M4/"
cleaned_files = os.listdir(cleaned_data_path)

# # training data for feature selection
# h2_train_files = [x for x in cleaned_files if "_h2_train" in x]
# h2_test_files = [x for x in cleaned_files if "_h2_test" in x]

forecasts_path = "../../Outputs/Forecasts/M4/"
results_path = "../../Outputs/Results/M4/"

# # loop over forecasting models and training data files

# # for seasonal models, we have to account for some data not having a seasonal period.
# # for TES, we only apply it to monthly and quarterly data

# forecasting_models = {"SES": {"sp":None, "mean_normalize":False, "options":None},
#                       "DES": {"sp":None, "mean_normalize":False, "options":None},
#                       "TES": {"sp":None, "mean_normalize":False, "options":None}}
#                       #"ARIMA": {"sp":None, "mean_normalize":False, "options":None},
#                       #"VAR": {"sp":None, "mean_normalize":False, "options": {'save_params': False, 'simulate_series': False}}}
#                       #"LGBM": {"sp":None, "mean_normalize":True, "options": {'max_samples_per_ts': None, 'window_length': None}},
#                       #"RNN": {"sp":None, "mean_normalize":True, "options": {'input_chunk_length': None, 'training_length': None, 'max_samples_per_ts': 10, 'num_ensemble_models': 5}}}

# for m in forecasting_models.items():
    
#     model = m[0]
#     model_args = m[1]

#     computation_time[model + "_" + "baseline"] = np.zeros(computation_time.shape[0])

#     for f in h2_train_files:
        
#         if "Monthly" in f:
#             model_args["sp"] = 12
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 25
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 25
#                 model_args["options"]["training_length"] = 30
#         elif "Quarterly" in f:
#             model_args["sp"] = 4
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 11
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 11
#                 model_args["options"]["training_length"] = 13
#         else:
#             if model == "TES":
#                 continue
#             model_args["sp"] = 1
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 11
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 11
#                 model_args["options"]["training_length"] = 13
            
#         test_file = [x for x in h2_test_files if x[:-9] in f]
#         [test_file] = test_file

#         start = time.time()
#         generate_and_save_forecasts(data_folder=data_folder,
#                                     train_file=f,
#                                     test_file=test_file,
#                                     forecasts_path=forecasts_path,
#                                     results_path=results_path,
#                                     model=model,
#                                     model_args=model_args,
#                                     h=1,
#                                     file_suffix=f)
#         stop = time.time()
        
#         computation_time.loc[computation_time.File == f, model + "_" + "baseline"] = stop-start
        
#         computation_time.to_csv("../../Data/Computation Results/M4_computation_time.csv", index=False)


# print('Done.')

# computation_time.to_csv("../../Data/Computation Results/M4_computation_time.csv", index=False)

# generate error distributions for forecasts made for second to last period - these are
# used in k-nTS+

# create a list of the original file names
# og_files = os.listdir(cleaned_data_path)
# og_files = [x[:-12] for x in og_files if not any(y in x for y in ['_train', '_h1_', 'k-nts', 'AN_', 'DP_'])]

# print(og_files)

# # for each of these, we need to extract the error distributions for all forecasting models and 
# # baseline data sets

# models = ["SES", "DES", "TES"] #, "ARIMA", "VAR", "LGBM", "RNN"]

# protection_methods = {"original": [""],
#                       "AN": [0.25, 0.5, 1, 1.5, 2],
#                       "DP": [0.1, 1, 4.6, 10, 20]}

# computation_time["error_computation"] = np.zeros(computation_time.shape[0])

# computation_time.to_csv("../../Data/Computation Results/M4_computation_time.csv", index=False)

# for f in og_files:
#     error_distribution_generator(data_folder=data_folder,
#                                  file_string=f,
#                                  forecasts_path=forecasts_path, 
#                                  results_path=results_path, 
#                                  model_list=models, 
#                                  protection_method_dict=protection_methods, 
#                                  forecast_horizon="h2",
#                                  track_comp_time=True)

#############################

# ### run tsfeatures extraction on original and baseline protected datasets
# # run code in R file 'original_and_baseline_tsfeatures_extraction.R'

# ### generate k-nTS protected data sets
# # run code in R file 'k-nTS.R', which protects up to the last period

## run k-nTS+ protection files using generated error distributions

#############################
#############################
#############################

# forecasting for all protected data sets

# now we store all the cleaned data file names including the baseline
# protected data sets
# cleaned_data_path = "../../Data/Cleaned/" + data_folder
# cleaned_files = os.listdir(cleaned_data_path)

# # # # training data for feature selection
# h1_train_files = [x for x in cleaned_files if "_h1_train" in x and "Monthly" in x]
# h1_test_files = [x for x in cleaned_files if "_h1_test" in x]

# forecasts_path = "../../Outputs/Forecasts/" + data_folder
# results_path = "../../Outputs/Results/" + data_folder

# # loop over forecasting models and training data files

# # for seasonal models, we have to account for some data not having a seasonal period.
# # for TES, we only apply it to monthly and quarterly data

# forecasting_models = {"SES": {"sp":None, "mean_normalize":False, "options":None},
#                       "DES": {"sp":None, "mean_normalize":False, "options":None},
#                       "TES": {"sp":None, "mean_normalize":False, "options":None}}
#                       # "ARIMA": {"sp":None, "mean_normalize":False, "options":None},
#                       # "VAR": {"sp":None, "mean_normalize":False, "options": {'save_params': False, 'simulate_series': False}},
#                       # "LGBM": {"sp":None, "mean_normalize":True, "options": {'max_samples_per_ts': None, 'window_length': None}},
#                       # "RNN": {"sp":None, "mean_normalize":True, "options": {'input_chunk_length': None, 'training_length': None, 'max_samples_per_ts': 10, 'num_ensemble_models': 5}}}

# for m in forecasting_models.items():
    
#     model = m[0]
#     model_args = m[1]

#     for f in h1_train_files:
        
#         if "monthly" in f:
#             model_args["sp"] = 12
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 25
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 25
#                 model_args["options"]["training_length"] = 30
#         elif "quarterly" in f:
#             model_args["sp"] = 4
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 11
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 11
#                 model_args["options"]["training_length"] = 13
#         else:
#             if model == "TES":
#                 continue
#             model_args["sp"] = 1
#             if model == "LGBM":
#                 model_args["options"]["window_length"] = 11
#             elif model == "RNN":
#                 model_args["options"]["input_chunk_length"] = 11
#                 model_args["options"]["training_length"] = 13
            
#         test_file = [x for x in h1_test_files if x[:-9] in f]
#         [test_file] = test_file

#         generate_and_save_forecasts(data_folder=data_folder,
#                                     train_file=f,
#                                     test_file=test_file,
#                                     forecasts_path=forecasts_path,
#                                     results_path=results_path,
#                                     model=model,
#                                     model_args=model_args,
#                                     h=1,
#                                     file_suffix=f)

# print('Done.')

#########################################################################################
#########################################################################################
#########################################################################################

# Forecasting is done, now we compile results.

## start with generating the series level absolute errors for all models and data sets
cleaned_data_path = "../../Data/Cleaned/" + data_folder
forecasts_path = "../../Outputs/Forecasts/" + data_folder
results_path = "../../Outputs/Results/" + data_folder

# create a list of the original file names
files = os.listdir(cleaned_data_path)
files = [x[:-12] for x in files if not any(y in x for y in ['Augmented', '_train', '_h2_', 'k-nts', 'AN_', 'DP_'])]

print(files)

# for each of these, we need to extract the error distributions for all forecasting models and 
# baseline data sets

################
################
# *********** RNN is left out ************* #

models = ["SES", "DES", "TES"] #, "ARIMA", "VAR", "LGBM", "RNN"]

# protection methods dictionary
protection_methods = {"AN": [0.25, 0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20],
                      # "k-nts_": [3, 5, 7, 10, 15],
                      # "k-nts-corr_": [3, 5, 7, 10, 15],
                      # "gratis-full-k-nts-plus_": [3, 5, 7, 10, 15],
                      "k-nts-plus_": [3]}
                      # "k-nts-plus-corr_": [3, 5, 7, 10, 15],
                      # "k-nts-plus-scaled_": [3, 5, 7, 10, 15],
                      # "k-nts-plus-corr-scaled_": [3, 5, 7, 10, 15],
                      # "preprocess-k-nts-plus_": [3, 5, 7, 10, 15],
                      # "preprocess-lw-k-nts-plus_": [3, 5, 7, 10, 15]}

for f in files:
    error_distribution_generator(data_folder=data_folder,
                                 file_string=f,
                                 forecasts_path=forecasts_path, 
                                 results_path=results_path, 
                                 model_list=models, 
                                 protection_method_dict=protection_methods, 
                                 forecast_horizon="h1")

############################

# run R file 'results_computation.R' to compute average results across data sets, models, protection methods, etc.

############################

############################

### run tsfeatures extraction on k-nts protected data sets
# run code in R file 'k-nts_tsfeatures_extraction.R'