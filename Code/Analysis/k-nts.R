### Perform k-NTS Data Protection (without the machine learning feature selection)
# i.e., using manually selected features

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)

# paths to the data files and feature files
file_path <- "../../Data/Cleaned/"
features_path <- "../../Data/Features/"

# function to import and process series
import_data <- function(file_name, sp){
  
  ###
  # Takes the name file_name of a time series data set and the seasonal period
  # of that time series data. Imports the data, pre-processes and converts 
  # to a timeseries object, and returns the data.
  ###
  
  # import data and convert to a list of series
  ts_data <- as.list(as.data.frame(t(read.csv(file_name))))
  
  # remove NA values from the end of each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  # truncate data to strictly positive
  ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  
  # take the log of the data
  ts_data <- lapply(ts_data, log)
  
  return(ts_data)
}

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(file_path), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)

# function to import corresponding features
import_features <- function(ts_file_name, feature_file_directory){
  
  ###
  # Takes the name ts_file_name which is the name of a time series data set
  # and imports and returns the corresponding file containing the features of those
  # time series from the directory feature_file_directory
  ###
  
  feature_file_name <- grep(ts_file_name, list.files(feature_file_directory), value=TRUE)
  
  return(read.csv(paste0(feature_file_directory, feature_file_name)))
}

import_features(file_names[1], features_path)

feature_file_name <- grep(file_names[1], list.files(features_path), value=TRUE)



### features still need to be extracted on a rolling basis











# read in original time series
X <- read.csv("../../Data/Train/Clean/m3_monthly_micro_h1.csv")

# convert to a list of series
X <- as.list(as.data.frame(t(X)))

# remove NA values from each series
X <- lapply(X, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
X <- lapply(X, function(x) ts(x, frequency=12))

# take the log of the data
X <- lapply(X, log)







# vector of features to select
sf <- c("entropy", "e_acf1", "trend", "seasonal_strength",
        "skewness", "kurtosis", "hurst",
        "series_mean", "series_variance")

# split X into three separate datasets, one for each series length
Xs <- list()
unique_lengths <- unique(sapply(X, length))
lengths <- sapply(X, length)
for (l in seq_along(unique_lengths)){
  ids <- lengths==unique_lengths[l]
  Xs[[l]] <- X[ids]
}

knts_alg <- function(time_series, window_length, k, features_to_calculate, selected_features){
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
  
  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=12))
  
  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
  # normalize features
  # C <- as.data.frame(scale(C))
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  
  # for each time period in the initial window
  for (j in 1:num_series){
    
    # select the jth column
    d <- D[,j]
    
    # sort the distances in the jth column smallest to largest
    sorted <- sort(d, index.return=TRUE)
    
    # select from index 2 to k+1 since first index corresponds to the series itself
    K <- sorted$ix[2:k+1]
    
    # for each series
    for (t in 1:window_length){
      
      # sample an index
      i <- sample(K, size=1)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=12))
    
    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
    # normalize features
    # C <- as.data.frame(scale(C))
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)
    
    ## Calculate the feature distance matrix D
    # ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
    D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    
    for (j in 1:num_series){
      
      # select the jth column
      d <- D[,j]
      
      # sort the distances in the jth column smallest to largest
      sorted <- sort(d, index.return=TRUE)
      
      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sorted$ix[2:k+1]
      
      # sample an index
      # later can implement sampling based on distance based probabilities
      i <- sample(K, size=1)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  return(X_new)
  
}

# perform data protection for three values of k = {5, 10, 15}

perform_knts <- function(time_series, window_length, k, features_to_calculate, selected_features){
  
  X_k <- lapply(time_series, function(x) knts_alg(x, window_length=window_length, k=k, features_to_calculate=features_to_calculate, selected_features=selected_features))
  
  X_k <- lapply(X_k, function(x) as.data.frame(t(x)))
  
  X_k <- lapply(X_k, exp)
  
  X_k <- do.call(rbind.fill, X_k)
  
  return(X_k)
  
}

window_length <- 25

X_k3 <- perform_knts(time_series=Xs, window_length=window_length, k=3, features_to_calculate=fv, selected_features=sf)
X_k5 <- perform_knts(time_series=Xs, window_length=window_length, k=5, features_to_calculate=fv, selected_features=sf)
X_k7 <- perform_knts(time_series=Xs, window_length=window_length, k=7, features_to_calculate=fv, selected_features=sf)
X_k10 <- perform_knts(time_series=Xs, window_length=window_length, k=10, features_to_calculate=fv, selected_features=sf)
X_k15 <- perform_knts(time_series=Xs, window_length=window_length, k=15, features_to_calculate=fv, selected_features=sf)

write.csv(X_k3, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts_3.csv", row.names=FALSE)
write.csv(X_k5, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts_5.csv", row.names=FALSE)
write.csv(X_k7, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts_7.csv", row.names=FALSE)
write.csv(X_k10, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts_10.csv", row.names=FALSE)
write.csv(X_k15, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts_15.csv", row.names=FALSE)
