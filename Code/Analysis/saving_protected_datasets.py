## File to save copies of protected datasets.

# Author: Cameron Bale

from data_protection_functions import *
import pandas as pd

# import monthly micro time series
# ignore header and skip the first row to use integers as column names
full_data = pd.read_csv("../../Data/Train/Clean/full_m3_monthly_micro_clean.csv", header=None, skiprows=1)

# convert to a list of series
# drop NA values
full_data = [x.dropna() for _, x in full_data.iterrows()]

# path to where data should be saved
data_path = "../../Data/Train/Clean/"

# horizons to consider
h = [1, 2]

# protection methods and parameters
protection_methods = {"AN": [0.25, 0.5, 1, 1.5, 2],
                      "DP": [0.1, 1, 4.6, 10, 20]}

# for each horizon, apply the data protection methods and save the protected
# data sets
for H in h:
    # create training data excluding test horizon
    Y = [x.iloc[:-H] for x in full_data]

    Y_df = pd.DataFrame(Y)

    Y_df.to_csv(data_path + "m3_monthly_micro_h" + str(H) + ".csv", index=False)

    # for each protection method
    for p in protection_methods.items():

        params = p[1]

        # for each protection method parameter
        for param in params:
            protection_method = p[0] + "_" + str(param)
            if p[0] == "Top":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
            elif p[0] == "Bottom":
                Y_protected = apply_data_protection(Y, coding_type=p[0], coding_percentage=param)
            elif p[0] == "AN":
                Y_protected = apply_data_protection(Y, num_stdev=param)
            elif p[0] == "DP":
                Y_protected = apply_data_protection(Y, epsilon=param)

            Y_protected = pd.DataFrame(Y_protected)

            Y_protected.to_csv(data_path + "protected_m3_monthly_micro_h" + str(H) + "_" + protection_method + ".csv", index=False)
