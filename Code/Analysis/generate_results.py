##### Results File #####
# This file generates all results presented in the paper.
# Author: Cameron Bale

################################################################################
############################### Preliminaries ##################################
################################################################################

## Import modules
from forecasting_functions import *

### Set parameters

## Protection parameters
epsilon = 4.6
coding_type = "Top"
coding_percentage = 0.10
num_stdev = 1

## Forecasting parameters
# forecast horizon
h = 1
# LGBM window length
window_length = 20
# seasonal periodicity
sp = 52

################################################################################
############################### Import Data ####################################
################################################################################

# ignore header and skip the first row to use integers as column names
full_data = pd.read_csv("../../Data/Train/Clean/full_m4_weekly_finance_clean.csv", header=None, skiprows=1)

# convert to a list of series
full_data = [x.dropna() for _, x in full_data.iterrows()]

# create train and test data
Y = [x.iloc[:-h] for x in full_data]
Test = [x.iloc[-h:] for x in full_data]

# create protected version of Y
Y_protected = apply_data_protection(sensitive_data=Y, coding_type=coding_type, coding_percentage=coding_percentage, num_stdev=num_stdev, epsilon=epsilon)

################################################################################
################### Protect Data and Generate Forecasts ########################
################################################################################

### Obtain results for each combination of protection method and forecasting model
dp_results = all_models_one_protection(full_data=full_data, h=h, sp=sp, window_length=window_length, epsilon=epsilon)
add_noise_results = all_models_one_protection(full_data=full_data, h=h, sp=sp, window_length=window_length, num_stdev=num_stdev)
top_results = all_models_one_protection(full_data=full_data, h=h, sp=sp, window_length=window_length, epsilon=epsilon)
bottom_results = all_models_one_protection(full_data=full_data, h=h, sp=sp, window_length=window_length, epsilon=epsilon)

################################################################################
############################### Save Results ###################################
################################################################################

colnames = ["Error Metric",
            "Forecasting Model",
            "Original",
            "DP" + " (epsilon=" + str(epsilon) + ")",
            "Add. Noise (" + str(num_stdev) + " SD)",
            "Top Coding (" + str(int(coding_percentage*100)) + "%)",
            "Bottom Coding (" + str(int(coding_percentage*100)) + "%)"]

models = ["SES", "DES", "TES", "LGBM"]

dp_results = [dp_ses, dp_des, dp_tes, dp_lgbm]

original_mape = []
original_mdape = []
dp_mape = []
dp_mdape = []

for i in dp_results:
    # original accuracy
    original_mape.append(i['Global MAPE, MdAPE'][0])
    original_mdape.append(i['Global MAPE, MdAPE'][1])
    # accuracy under differential privacy
    dp_mape.append(i['Global Protected MAPE, MdAPE'][0])
    dp_mdape.append(i['Global Protected MAPE, MdAPE'][1])
