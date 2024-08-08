## Forecast for the original and baseline protected (additive noise and differential privacy)
## data sets for the second to last time period (h2), i.e., the last time period that is available
## to the data owner. The forecast error MAE is computed and will be predicted using time
## series features in the feature selection methodology for k-nTS+.

# Author: Cameron Bale

import time
import os
from forecasting_functions import *
from error_distribution_generator import *

data_folder = "M4_rate/"

computation_time = pd.read_csv("../../Data/Computation_Time/M4_rate_computation_time.csv")

# store the cleaned data file names including the baseline protected data sets
cleaned_data_path = "../../Data/Cleaned/" + data_folder
cleaned_files = os.listdir(cleaned_data_path)

h2_train_files = [x for x in cleaned_files if "_h2_train" in x]
h2_test_files = [x for x in cleaned_files if "_h2_test" in x]

os.makedirs("../../Outputs/Forecasts/" + data_folder, exist_ok=True)
os.makedirs("../../Outputs/Results/" + data_folder, exist_ok=True)

forecasts_path = "../../Outputs/Forecasts/" + data_folder
results_path = "../../Outputs/Results/" + data_folder

# loop over forecasting models and training data files

# for seasonal models, we have to account for some data not having a seasonal period.
# for TES, we only apply it to monthly and quarterly data

# we applied the log prior to the rate transformation, and we no longer want to truncate
# since rates can be negative

forecasting_models = {"SES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},
                      "DES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},
                      # "TES": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},
                      # "ARIMA": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options":None},
                      "VAR": {"sp":None, "truncate":False, "log":False, "mean_normalize":False, "options": {'save_params': False, 'simulate_series': False}},
                      "LGBM": {"sp":None, "truncate":False, "log":False, "mean_normalize":True, "options": {'max_samples_per_ts': None, 'window_length': None}}}
                      # "RNN": {"sp":None, "truncate":False, "log":False, "mean_normalize":True, "options": {'input_chunk_length': None, 'training_length': None, 'max_samples_per_ts': 10, 'num_ensemble_models': 10}}}

for m in forecasting_models.items():
    
    model = m[0]
    model_args = m[1]

    computation_time[model + "_" + "baseline"] = np.zeros(computation_time.shape[0])

    for f in h2_train_files:
        
        if "Monthly" in f:
            model_args["sp"] = 12
            if model == "LGBM":
                model_args["options"]["window_length"] = 25
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 25
                model_args["options"]["training_length"] = 30
        elif "Quarterly" in f:
            model_args["sp"] = 4
            if model == "LGBM":
                model_args["options"]["window_length"] = 10
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 9
                model_args["options"]["training_length"] = 10
        elif "Weekly" in f:
            model_args["sp"] = 52
            if model == "LGBM":
                model_args["options"]["window_length"] = 10
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 9
                model_args["options"]["training_length"] = 10
        elif "Hourly" in f:
            model_args["sp"] = 24
            if model == "LGBM":
                model_args["options"]["window_length"] = 25
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 25
                model_args["options"]["training_length"] = 30
        elif "Daily" in f:
            model_args["sp"] = 7
            if model == "LGBM":
                model_args["options"]["window_length"] = 10
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 9
                model_args["options"]["training_length"] = 10
        else:
            if model == "TES":
                continue
            model_args["sp"] = 1
            if model == "LGBM":
                model_args["options"]["window_length"] = 10
            elif model == "RNN":
                model_args["options"]["input_chunk_length"] = 9
                model_args["options"]["training_length"] = 10
            
        test_file = [x for x in h2_test_files if x[:-9] in f]
        [test_file] = test_file

        start = time.time()
        generate_and_save_forecasts(data_folder=data_folder,
                                    train_file=f,
                                    test_file=test_file,
                                    forecasts_path=forecasts_path,
                                    results_path=results_path,
                                    model=model,
                                    model_args=model_args,
                                    h=1,
                                    file_suffix=f)
        stop = time.time()
        
        computation_time.loc[computation_time.File == f, model + "_" + "baseline"] = stop-start
        
        computation_time.to_csv("../../Data/Computation_Time/M4_rate_computation_time.csv", index=False)

print('Forecasting done.')

#############################################################################################################

## generate error distributions for forecasts made for second to last period - these are
## used in k-nTS+

# create a list of the original file names
og_files = os.listdir(cleaned_data_path)
og_files = [x[:-12] for x in og_files if not any(y in x for y in ['_train', '_h1_', 'k-nts', 'AN_', 'DP_'])]

# for each of these, we need to extract the error distributions for all forecasting models and 
# baseline data sets

models = ["SES", "DES", "VAR", "LGBM"]

protection_methods = {"original": [""],
                      "DP": [0.1, 1, 4.6, 10, 20]}

# make column to store times for error computation and save since this file
# will be called inside the `error_distribution_generator` function
computation_time["error_computation"] = np.zeros(computation_time.shape[0])
computation_time.to_csv("../../Data/Computation_Time/M4_rate_computation_time.csv", index=False)

for f in og_files:
    error_distribution_generator(data_folder=data_folder,
                                 file_string=f,
                                 forecasts_path=forecasts_path, 
                                 results_path=results_path, 
                                 model_list=models, 
                                 protection_method_dict=protection_methods, 
                                 forecast_horizon="h2",
                                 track_comp_time=True,
                                 is_rate=True)
