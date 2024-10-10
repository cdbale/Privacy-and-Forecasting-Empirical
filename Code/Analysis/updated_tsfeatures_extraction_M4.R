##### Code to extract time series features for the original 
##### and baseline protected data sets for the h1 horizon

# Author: Cameron Bale

## NOTE: this file extracts features up through time period T-2 from the 
# original and baseline protected data sets. These features are used to predict
# the forecast accuracy to perform feature selection for k-nTS+.

## Author: Cameron Bale

library(e1071)
library(tidyverse)
library(tsfeatures)
library(forecast)

source('custom_feature_functions.R')

data_folder <- "M4/"

file_path <- paste0("../../Data/Cleaned/", data_folder)

# import names of original and baseline protected data files
file_names <- grep("_h1_train", list.files(file_path), value=TRUE)

file_names

#### compengine includes features in:
# autocorr_features
# pred_features
# station_features
# dist_features
# scal_features --> can't use this, some time series are too short
# also exclude arch.lm and arch_r2, they often return NA values

# vector of feature names to calculate
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

### Perform feature extraction for all original and baseline data sets.

################################################################################
################################################################################
################################################################################
################################################################################

for (f in file_names){
  data_set <- read.csv(paste0(file_path, f))
  sp_l <- ifelse(grepl("Daily", f), 7,
                 ifelse(grepl("Hourly", f), 24,
                        ifelse(grepl("Monthly", f), 12,
                               ifelse(grepl("Quarterly", f), 4,
                                      ifelse(grepl("Weekly", f), 52, 1)))))
  

  features <- extract_features(data_set, sp=sp_l, feature_vector=fv, truncate=TRUE, take_log=TRUE, calculate_cross_correlations=FALSE)

  features <- features %>% select(-nperiods, -seasonal_period)

  write.csv(features, file=paste0("../../Data/Features/", data_folder, "features_", f), row.names=FALSE)
  print(f)
}
