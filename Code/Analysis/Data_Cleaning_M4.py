## Import and prep M3 data for protection and forecasting.

# Author: Cameron Bale

import pandas as pd
import numpy as np
import os

def import_split_save(time_series_frequency, min_number_series):
    
    '''
    Function to import a subset of the M4 data. Filter out series that do not have at least min_number_series
    of the same length.
    
    Args:
    
    time_series_frequency (str): frequency corresponding to the desired file (accepts yearly, quarterly, monthly, other)
    min_number_series (int): minimum number of series that must have the same length to be kept in the data. Allows us to
        use global forecasting models and swap values between nearest neighbor time series. Select a value equal to (k + 1) 
        where k is the largest value you will consider for swapping.
    '''
    
    os.makedirs("../../Data/Cleaned/M4/", exist_ok=True)
    
    # import train and test data
    temp_train = pd.read_csv("../../Data/M4/Train/" + time_series_frequency + "-train.csv").iloc[:,1:]
    temp_test = pd.read_csv("../../Data/M4/Test/" + time_series_frequency + "-test.csv").iloc[:,1:]
    
    # remove the missing values from the ends of series
    temp_train = [x.dropna() for i, x in temp_train.iterrows()]
    temp_test = [x.dropna() for i, x in temp_test.iterrows()]
    
    # compute the length of each training series
    train_lengths = pd.Series([len(x) for x in temp_train])
    
    # count how many series have each length
    length_counts = train_lengths.value_counts()

    # indicate which lengths to keep based on which have at least min_number_series series
    length_keep = length_counts.index[length_counts >= min_number_series]
    
    # logical vector - which series are we keeping based on length
    to_keep = train_lengths.isin(length_keep)
    
    # restrict train and test data to the series with the appropriate lengths
    temp_train = [x for i,x in enumerate(temp_train) if to_keep.iloc[i]]
    temp_test = [x for i,x in enumerate(temp_test) if to_keep.iloc[i]]
    
    # now, sort based on length so identical length series are grouped together
    sort_ids = np.argsort([len(x) for x in temp_train])
    temp_train = [temp_train[x] for x in sort_ids]
    temp_test = [temp_test[x] for x in sort_ids]
    
    # used for machine learning feature selection for k-nTS+
    train_2, test_2 = pd.DataFrame([x.iloc[:-1] for x in temp_train]), pd.DataFrame([x.iloc[-1] for x in temp_train])
        
    # used for assessing final forecast accuracy
    train_1 = pd.DataFrame(temp_train)
    test_1 = pd.DataFrame([x.iloc[0] for x in temp_test])

    # now save all data sets
    train_2.to_csv("../../Data/Cleaned/M4/" + time_series_frequency + "_h2_train" + ".csv", index=False)
    train_1.to_csv("../../Data/Cleaned/M4/" + time_series_frequency + "_h1_train" + ".csv", index=False)
    test_2.to_csv("../../Data/Cleaned/M4/" + time_series_frequency + "_h2_test" + ".csv", index=False)
    test_1.to_csv("../../Data/Cleaned/M4/" + time_series_frequency + "_h1_test" + ".csv", index=False)
    
temp = [import_split_save(freq, 16) for freq in ["Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"]]