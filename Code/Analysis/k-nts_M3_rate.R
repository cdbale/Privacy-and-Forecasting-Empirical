### Perform k-NTS Data Protection (without the machine learning feature selection)
# i.e., using manually selected features

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tsfeatures)
library(e1071)
library(forecast)

source("custom_feature_functions.R")
source("k-nts_helper_functions.R")

data_folder <- "M3_rate/"

# paths to the data files
fp <- paste0("../../Data/Cleaned/", data_folder)

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

# features to calculate
fv <- c("entropy_c", "hurst", "stl_features",
        "series_mean", "series_variance",
        "skewness", "kurtosis")

# vector of features to select
sf <- c("entropy", "e_acf1", "trend", "seasonal_strength",
        "skewness", "kurtosis", "hurst",
        "series_mean", "series_variance")

### perform the protection ###
kvals <- c(3, 5, 7, 10, 15)

### perform the protection ###

for (f in file_names){
  
  # determine sp
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # window length is the maximum of 2*sp + 1 and 12, so
  # 25 for monthly data and 11 for quarterly/yearly data
  window_length <- max(c(2*sp + 1, 12))
  
  # reassign selected features
  sft <- sf
  
  if (sp == 1) {
    sft <- sf[!sf == "seasonal_strength"]
  }
  
  X_knts <- perform_knts(ts_file=f,
                         ts_file_path=fp,
                         seasonal_period=sp,
                         window_length=window_length,
                         kvals=kvals,
                         features_to_calculate=fv,
                         selected_features=sft,
                         is_rate=TRUE)
    
  for(i in seq_along(X_knts)){
    write.csv(X_knts[[i]], file=paste0(fp, "k-nts_", kvals[i], "_", f), row.names=FALSE)
  }
}
