## Import and prep M3 rate data for protection and forecasting.

# Author: Cameron Bale

import pandas as pd
import numpy as np
import os

# function to convert a time series in a rate using the equation from the paper
def rate_conversion(time_series):
    
    # assumes time_series is a pandas series
    
    rate_series = pd.Series(np.repeat(np.nan, time_series.shape[0]))
    
    for i in range(time_series.shape[0]):
        if i == 0:
            rate_series.iloc[i] = 0.0
        else:
            rate_series.iloc[i] = (time_series.iloc[i] - time_series.iloc[i-1])/np.mean([time_series.iloc[i], time_series.iloc[i-1]])
            
    return rate_series

def import_split_save(time_series_frequency, min_number_series):
    
    '''
    Function to import a subset of the M3 data defined by the frequency, i.e., yearly, quarterly, monthly, other.
    Split the subset into further subsets defined by the category, e.g., MICRO, FINANCE, etc.
    
    Args:
    
    time_series_frequency (str): frequency corresponding to the desired file (accepts yearly, quarterly, monthly, other)
    min_number_series (int): minimum number of series that must have the same length to be kept in the data. Allows us to
        use global forecasting models and swap values between nearest neighbor time series. Select a value equal to (k + 1) 
        where k is the largest value you will consider for swapping.
    '''
    
    # import time series file
    temp = pd.read_csv("../../Data/M3/M3-" + time_series_frequency + ".csv")
    
    # find the index of the column of the first time period
    start = np.where(temp.columns == '1')[0][0] 
    
    # strip the whitespace and store the categories (e.g., MICRO)
    temp.Category = temp.Category.str.strip()
    categories = np.unique(temp.Category)
    
    os.makedirs("../../Data/Cleaned/M3_rate/", exist_ok=True)
    
    ## for each category, sort the data by length of series and save in a separate csv file
    for cat in categories:
        
        # filter for the desired category and sort by series length
        temp_cat = temp.loc[temp.Category == cat].sort_values("N")
        
        # count how many series have each length
        length_counts = temp_cat.N.value_counts()
        
        # indicate which lengths to keep based on which have at least 4 series
        length_keep = length_counts.index[length_counts >= min_number_series]
        
        # filter for the series with the appropriate lengths
        temp_cat = temp_cat.loc[temp_cat.N.isin(length_keep),:]
        
        num_series = temp_cat.shape[0]
        
        # if there will be no time series for a given frequency and category, skip to the next
        if num_series == 0:
            print(time_series_frequency + " " + cat + ": " + str(num_series) + " series.")
            continue
        
        print(time_series_frequency + " " + cat + ": " + str(num_series) + " series.")
        
        # remove all columns except time series values and split time series into a list
        Y = [x.dropna() for _, x in temp_cat.iloc[:,start:].iterrows()]
        
        # ensure all values are positive
        Y = [pd.Series([i if i >= 1 else 1 for i in x]) for x in Y]
        
        # apply log transformation
        Y = [np.log(x) for x in Y]
        
        # convert to rates
        Y = [rate_conversion(x) for x in Y]
        
        ## create training data excluding test horizon
        ## the number indicates how many periods are excluded from the end of the series for testing
        
        # used for machine learning feature selection for k-nTS+
        Y_train_2, Y_test_2 = pd.DataFrame([x.iloc[:-2] for x in Y]), pd.DataFrame([x.iloc[-2] for x in Y])
        
        # used for assessing final forecast accuracy
        Y_train_1, Y_test_1 = pd.DataFrame([x.iloc[:-1] for x in Y]), pd.DataFrame([x.iloc[-1] for x in Y])
        
        Y_train_1.columns = np.arange(Y_train_1.shape[1])
        Y_train_2.columns = np.arange(Y_train_2.shape[1])
        Y_test_1.columns = np.arange(Y_test_1.shape[1])
        Y_test_2.columns = np.arange(Y_test_2.shape[1])
        
        # save files
        Y_train_2.to_csv("../../Data/Cleaned/M3_rate/rate_" + time_series_frequency + "-" + cat + "_h2_train" + ".csv", index=False)
        Y_train_1.to_csv("../../Data/Cleaned/M3_rate/rate_" + time_series_frequency + "-" + cat + "_h1_train" + ".csv", index=False)
        Y_test_2.to_csv("../../Data/Cleaned/M3_rate/rate_" + time_series_frequency + "-" + cat + "_h2_test" + ".csv", index=False)
        Y_test_1.to_csv("../../Data/Cleaned/M3_rate/rate_" + time_series_frequency + "-" + cat + "_h1_test" + ".csv", index=False)
        
temp = [import_split_save(freq, 16) for freq in ["yearly", "quarterly", "monthly", "other"]]