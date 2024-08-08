## Create protected data sets using additive noise and differential privacy.
## These privacy methods are applied to all training data sets (h1 and h2 versions).
## The h2 versions are used in the k-nTS+ framework, and the forecasting results
## on the h1 versions are compared to k-nTS+ in the paper.

# Author: Cameron Bale

import time
import os
from data_protection_functions import *

data_folder = "M4/"

# # create protected data sets using differential privacy
protection_methods = {"DP": [1, 4.6, 10, 20]}

# for the h1 and h2 horizon, apply the data protection methods and save the protected
# data sets

# path to cleaned original data
cleaned_data_path = "../../Data/Cleaned/" + data_folder
cleaned_files = os.listdir(cleaned_data_path)

# separate the h2 training files since we need to track computation time for these
h2_train_files = [x for x in cleaned_files if "_h2_train" in x and not any(y in x for y in ["DP_", "AN_"])]

h1_train_files = [x for x in cleaned_files if "_h1_train" in x and not any(y in x for y in ["DP_", "AN_"])]

computation_time = {}

# for each training file, create additive noise and differential privacy protected data
for i, f in enumerate(h2_train_files):
    
    # for each protection method (AN and DP)
    for p in protection_methods.items():
        # vector of privacy parameters
        params = p[1]
        # for each protection method parameter
        for param in params:
            
            protection_method = p[0] + "_" + str(param)

            if p[0] == "AN":
                start = time.time()
                time.sleep(0.01)
                temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
                                              save_data_path=cleaned_data_path + protection_method + "_" + f,
                                              num_stdev=param)
                stop = time.time()
                # track computation time of baseline protection
                computation_time[protection_method + "_" + f] = stop-start-0.01
                
                temp = save_protected_dataset(original_data_path=cleaned_data_path + h1_train_files[i],
                                              save_data_path=cleaned_data_path + protection_method + "_" + h1_train_files[i],
                                              num_stdev=param)
                
                print("Created AN s = " + str(param) + " for " + f[:-13])

            elif p[0] == "DP":
                start = time.time()
                time.sleep(0.01)
                temp = save_protected_dataset(original_data_path=cleaned_data_path + f,
                                              save_data_path=cleaned_data_path + protection_method + "_" + f,
                                              epsilon=param)
                stop = time.time()
                # track computation time of baseline protection
                computation_time[protection_method + "_" + f] = stop-start-0.01
                
                temp = save_protected_dataset(original_data_path=cleaned_data_path + h1_train_files[i],
                                              save_data_path=cleaned_data_path + protection_method + "_" + h1_train_files[i],
                                              epsilon=param)
                
                print("Created DP eps = " + str(param) + " for " + f[:-13])

# save the computation time for baseline protected data sets
computation_time = pd.DataFrame(computation_time.items(), columns=['File', 'Baseline Protection Time'])
os.makedirs("../../Data/Computation_Time/", exist_ok=True)
computation_time.to_csv("../../Data/Computation_Time/M4_computation_time.csv", index=False)
