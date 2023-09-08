# File to generate distributions of forecast errors for each model and
# protection method.

# Author: Cameron Bale

import os, time
import pandas as pd
import numpy as np
from sktime.performance_metrics.forecasting import mean_absolute_error

## function below is applied for each training data file
## want to make sure we end up with one file containing the
## error distributions for the given training file from all
## forecasting models and baseline data sets

### need the file string to just be the beginning e.g., "monthly-demographic"

def error_distribution_generator(file_string, forecasts_path, results_path, model_list, protection_method_dict, forecast_horizon):
    
    test_files = os.listdir("../../Data/Cleaned/")
    
    test_file = [x for x in test_files if forecast_horizon + "_test" in x and file_string in x]
    
    [test_file] = test_file
    
    test_data = pd.read_csv("../../Data/Cleaned/" + test_file).T

    distribution_dict = {}
    
    forecast_files = os.listdir(forecasts_path)
    
    forecast_files = [x for x in forecast_files if file_string in x and forecast_horizon in x]
    
    # track computation time
    
    computation_time = pd.read_csv("../../Data/Computation Results/computation_time.csv")
    
    for f in forecast_files:
        
        split_f = f.split("_")[2:]
        
        combined_f = "_".join(split_f)[:-4]
        
        if "AN_" in combined_f or "DP_" in combined_f:
            
            start = time.time()
            
            fcasts = pd.read_csv(forecasts_path + f)
        
            mae_vals = pd.DataFrame(mean_absolute_error(test_data, fcasts, multioutput="raw_values"))
        
            distribution_dict[f] = mae_vals.squeeze()
        
            path = results_path + "Error_Distributions/" 
            if not os.path.exists(path):
                os.makedirs(path)
            mae_vals.to_csv(path + "error_distribution_" + f, index=False)

            stop = time.time()
            
            where_to = np.where(pd.Series([combined_f in x for x in computation_time.File]))[0]
            
            computation_time.loc[where_to, "error_computation"] += stop-start
        
            computation_time.to_csv("../../Data/Computation Results/computation_time.csv", index=False)
        
        else:
            
            fcasts = pd.read_csv(forecasts_path + f)
        
            mae_vals = pd.DataFrame(mean_absolute_error(test_data, fcasts, multioutput="raw_values"))
        
            distribution_dict[f] = mae_vals.squeeze()
        
            path = results_path + "Error_Distributions/" 
            if not os.path.exists(path):
                os.makedirs(path)
            mae_vals.to_csv(path + "error_distribution_" + f, index=False)

    ddf = pd.DataFrame(distribution_dict)

    ddf.to_csv(path + file_string + "_all_distributions_" + forecast_horizon + ".csv", index=False)
    
