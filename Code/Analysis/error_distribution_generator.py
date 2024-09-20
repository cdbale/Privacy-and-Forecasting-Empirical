# File to compute the forecast errors for each model and
# protection method.

# Author: Cameron Bale

import os, time
import pandas as pd
import numpy as np
from sktime.performance_metrics.forecasting import mean_absolute_error

## function below is applied for each training data file
## want to make sure we end up with one file containing the
## errors for the given training file from all
## forecasting models and baseline data sets

def error_distribution_generator(data_folder, file_string, forecasts_path, results_path, model_list, protection_method_dict, forecast_horizon, is_rate=False, track_comp_time=False, inverse_rate=False):
    
    test_files = os.listdir("../../Data/Cleaned/" + data_folder)
    
    if is_rate:

        test_file = [x for x in test_files if forecast_horizon + "_test" in x and file_string in x]
        
    else:
        
        test_file = [x for x in test_files if forecast_horizon + "_test" in x and file_string in x and "rate" not in x]

    [test_file] = test_file
    
    test_data = pd.read_csv("../../Data/Cleaned/" + data_folder + test_file).T

    distribution_dict = {}
    
    forecast_files = os.listdir(forecasts_path)
    
    forecast_files = [x for x in forecast_files if file_string in x and forecast_horizon in x]
    
    # track computation time
    
    if track_comp_time:
        computation_time = pd.read_csv("../../Data/Computation_Time/" + data_folder[:-1] + "_computation_time.csv")

    for f in forecast_files:
        
        split_f = f.split("_")[2:]
        
        combined_f = "_".join(split_f)[:-4]
            
        start = time.time()
        time.sleep(0.01)
            
        fcasts = pd.read_csv(forecasts_path + f)
        
        mae_vals = pd.DataFrame(mean_absolute_error(test_data, fcasts, multioutput="raw_values"))
        
        distribution_dict[f] = mae_vals.squeeze()
        
        path = results_path + "Error_Distributions/" 
        if not os.path.exists(path):
            os.makedirs(path)
        mae_vals.to_csv(path + "error_distribution_" + f, index=False)

        stop = time.time()
            
        if track_comp_time:
            where_to = np.where(pd.Series([combined_f in x for x in computation_time.File]))[0]
            
            computation_time.loc[where_to, "error_computation"] += stop-start-0.01
            
            computation_time.loc[where_to, "num_series"] = mae_vals.shape[0]
        
            computation_time.to_csv("../../Data/Computation_Time/" + data_folder[:-1] + "_computation_time.csv", index=False)

    ddf = pd.DataFrame(distribution_dict)

    ddf.to_csv(path + file_string + "_all_distributions_" + forecast_horizon + ".csv", index=False)
    
    if inverse_rate:
        
        inverse_rate_distribution_dict = {}
        
        df = data_folder[:2]
        
        test_files = os.listdir("../../Data/Cleaned/" + df + "/")   
        
        if "M3" in data_folder:
            test_file = [x for x in test_files if forecast_horizon + "_test" in x and file_string[5:] in x and "rate" not in x]
        elif "M4" in data_folder:
            test_file = [x for x in test_files if forecast_horizon + "_test" in x and file_string in x and "rate" not in x]

        [test_file] = test_file

        test_data = pd.read_csv("../../Data/Cleaned/" + df + "/" + test_file).T

        # import original data file
        og_files = os.listdir("../../Data/Cleaned/" + df + "/")
        if "M3" in data_folder:
            train_file = [x for x in og_files if not any(y in x for y in ['k-nts', 'AN_', 'DP_']) and "_h1_train" in x and file_string[5:] in x]
        elif "M4" in data_folder:
            train_file = [x for x in og_files if not any(y in x for y in ['k-nts', 'AN_', 'DP_']) and "_h1_train" in x and file_string in x]
        [train_file] = train_file

        train_data = pd.read_csv("../../Data/Cleaned/" + df + "/" + train_file)
        # convert to a list of series, and drop missing values
        train_data = [x.dropna() for _, x in train_data.iterrows()]

        last_values = [x[-1] for x in train_data]

        # use original data file and forecasts to convert to standard forecast
        for f in forecast_files:
            print(f)
            if "DP_0.1" in f or "DP_1" in f:
                continue
            split_f = f.split("_")[2:]
            combined_f = "_".join(split_f)[:-4]
            fcasts = pd.read_csv(forecasts_path + f).T
            fcasts = [x for i, x in fcasts.iterrows()]
            inverse_fcasts = [np.log(last_values[i]) * (1 + 0.5*x) / (1 - 0.5*x) for i, x in enumerate(fcasts)]
            inverse_fcasts = [pd.Series([np.exp(i) if i >= 0 else 1 for i in x]) for x in inverse_fcasts]
            inverse_fcasts = pd.DataFrame(np.array(inverse_fcasts).reshape(1, len(fcasts)))
            # compare standard forecast to original test file

            mae_vals = pd.DataFrame(mean_absolute_error(test_data, inverse_fcasts, multioutput="raw_values"))

            inverse_rate_distribution_dict[f] = mae_vals.squeeze()
            path = results_path + "Error_Distributions/" 
            mae_vals.to_csv(path + "ir_error_distribution_" + f, index=False)

        ddf = pd.DataFrame(inverse_rate_distribution_dict)

        ddf.to_csv(path + file_string + "_ir_all_distributions_" + forecast_horizon + ".csv", index=False)
