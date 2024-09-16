### Perform k-NTS Data Protection (without the machine learning feature selection)
# i.e., using manually selected features

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tsfeatures)
library(e1071)
library(forecast)

source("custom_feature_functions.R")

data_folder <- "M3_rate/"

# paths to the data files
fp <- paste0("../../Data/Cleaned/", data_folder)

# # function to import and process series
# import_data_rate <- function(file_name, file_path, sp){
#   
#   ###
#   # Takes the name file_name of a time series data set and the seasonal period
#   # of that time series data. Imports the data, pre-processes and converts 
#   # to a timeseries object, and returns the data.
#   ###
#   
#   # import data and convert to a list of series
#   ts_data <- as.list(as.data.frame(t(read.csv(paste0(file_path, file_name)))))
#   
#   # remove NA values from the end of each series
#   ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
#   
#   # convert each series to a TS object with appropriate seasonal frequency
#   ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
#   
#   return(ts_data)
# }

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









knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features){
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
  
  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
  
  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
  ## allow to remove a constant column if it exists
  C <- C[, apply(C, 2, var) != 0]
  
  # normalize features and convert C to a c x J matrix (num features by num series)
  C <- t(as.data.frame(scale(C)))
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  
  # for each time period in the initial window
  for (j in 1:num_series){
    
    # sort the distances in the jth column smallest to largest
    # select from index 2 to k+1 since first index corresponds to the series itself
    K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
    
    # for each series
    for (t in 1:window_length){
      
      # sample an index and replace the value
      X_new[t,j] <- time_series[[sample(K, size=1)]][t]
      
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
    ## allow to remove a constant column if it exists
    C <- C[, apply(C, 2, var) != 0]
    
    # normalize features and transpose to a c x J matrix (num features by num series)
    C <- t(as.data.frame(scale(C)))
    
    ## Calculate the feature distance matrix D
    D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    
    for (j in 1:num_series){
      
      # sort the distances in the jth column smallest to largest
      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
      
      # sample an index and replace the value
      X_new[t,j] <- time_series[[sample(K, size=1)]][t]
      
    }
  }
  
  # attempt to remove outliers using tsoutliers
  X_new <- as.list(as.data.frame(X_new))
  
  # convert each series to a TS object with appropriate seasonal frequency
  X_new <- lapply(X_new, function(x) ts(x, frequency=sp))
  
  X_new <- lapply(X_new, outlier_removal)
  
  X_new <- as.matrix(do.call(cbind, X_new))
  
  return(X_new)
  
}

perform_knts <- function(ts_file, ts_file_path, seasonal_period, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
  
  # read in time series
  X <- import_data_rate(file_name=ts_file, file_path=ts_file_path, sp=seasonal_period)
  
  # split X into separate datasets, one for each series length
  Xs <- list()
  unique_lengths <- unique(sapply(X, length))
  lengths <- sapply(X, length)
  for (l in seq_along(unique_lengths)){
    ids <- lengths==unique_lengths[l]
    Xs[[l]] <- X[ids]
  }
  
  X_k <- lapply(Xs, function(x) knts_alg(x, sp=seasonal_period, window_length=window_length, k=k, features_to_calculate=features_to_calculate, selected_features=selected_features))
  
  X_k <- lapply(X_k, function(x) as.data.frame(t(x)))
  
  X_k <- do.call(rbind.fill, X_k)
  
  return(X_k)
  
}

### perform the protection ###

for (f in file_names){
  
  # determine sp
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # window length is the maximum of 2*sp + 1 and 11, so
  # 25 for monthly data and 11 for quarterly/yearly data
  window_length <- max(c(2*sp + 1, 12))
  
  # reassign selected features
  sft <- sf
  
  if (sp == 1) {
    sft <- sf[!sf == "seasonal_strength"]
  }
  
  for (j in c(3, 5, 7, 10, 15)){
    
    X_knts <- perform_knts(ts_file=f,
                           ts_file_path=fp,
                           seasonal_period=sp,
                           window_length=window_length,
                           k=j,
                           features_to_calculate=fv,
                           selected_features=sft)
    
    write.csv(X_knts, file=paste0(fp, "k-nts_", j, "_", f), row.names=FALSE)
    
  }
}
