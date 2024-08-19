##### Code to extract time series features.

# Author: Cameron Bale

## NOTE: this file extracts features up through time period T-2 from the 
# original and baseline protected data sets. These features are used to predict
# the forecast accuracy to perform feature selection for k-nTS+.

## Author: Cameron Bale

data_folder <- "M4_rate/"

library(e1071)
library(tidyverse)
library(tsfeatures)
library(forecast)

source('custom_feature_functions.R')

file_path <- paste0("../../Data/Cleaned/", data_folder)

# check if sub directory exists 
if (file.exists(paste0("../../Data/Features/", data_folder))){
  NULL
} else {
  # create a new sub directory for storing time series features
  dir.create(file.path(paste0("../../Data/Features/", data_folder)))
}

# import names of original and baseline protected data files
file_names <- grep("_h2_train", list.files(file_path), value=TRUE)

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

# import file with computation times
computation_time <- read.csv("../../Data/Computation_Time/M4_rate_computation_time.csv")

for (f in file_names){
  data_set <- read.csv(paste0(file_path, f))
  sp_l <- ifelse(grepl("Daily", f), 7,
                 ifelse(grepl("Hourly", f), 24,
                        ifelse(grepl("Monthly", f), 12, 
                               ifelse(grepl("Quarterly", f), 4,
                                      ifelse(grepl("Weekly", f), 52, 1)))))
  start <- Sys.time()
  features <- extract_features(data_set, sp=sp_l, feature_vector=fv, truncate=FALSE, take_log=FALSE, calculate_cross_correlations=FALSE)
  stop <- Sys.time()
  features <- features %>% select(-nperiods, -seasonal_period)
  computation_time[computation_time$File==f, "feature_extraction"] <- difftime(stop, start, units="secs")
  write.csv(features, file=paste0("../../Data/Features/", data_folder, "features_", f), row.names=FALSE)
}

write.csv(computation_time, file="../../Data/Computation_Time/M4_rate_computation_time.csv", row.names=FALSE)
