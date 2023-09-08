### Perform k-NTS Data Protection (without the machine learning feature selection)
# i.e., using manually selected features

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tsfeatures)
library(e1071)

source("custom_feature_functions.R")

# paths to the data files and feature files
fp <- "../../Data/Cleaned/"
# features_path <- "../../Data/Features/"

# function to import and process series
import_data <- function(file_name, file_path, sp){
  
  ###
  # Takes the name file_name of a time series data set and the seasonal period
  # of that time series data. Imports the data, pre-processes and converts 
  # to a timeseries object, and returns the data.
  ###
  
  # import data and convert to a list of series
  ts_data <- as.list(as.data.frame(t(read.csv(paste0(file_path, file_name)))))
  
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
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

# features to calculate
fv <- c("entropy", "hurst", "stl_features",
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
  
  ##################################
  
  X_cor <- cor(do.call(cbind, X_window))
  
  ##################################
  
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
    K <- sorted$ix[2:(k+1)]
    
    #############################
    
    # obtain the correlations corresponding to the K indexes
    cors <- X_cor[,j][K]
    
    swap_weights <- exp(cors)/sum(exp(cors))
    
    #############################
    
    # for each series
    for (t in 1:window_length){
      
      # sample an index
      i <- sample(K, size=1, prob=swap_weights)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    ##################################
    
    X_cor <- cor(do.call(cbind, X_window))
    
    ##################################
    
    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
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
      K <- sorted$ix[2:(k+1)]
      
      #############################
      
      # obtain the correlations corresponding to the K indexes
      cors <- X_cor[,j][K]
      
      swap_weights <- exp(cors)/sum(exp(cors))
      
      #############################
      
      # sample an index
      i <- sample(K, size=1, prob=swap_weights)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  return(X_new)
  
}

# perform data protection for three values of k = {5, 10, 15}

perform_knts <- function(ts_file, ts_file_path, seasonal_period, window_length, k, features_to_calculate, selected_features){
  
  # read in time series
  X <- import_data(file_name=ts_file, file_path=ts_file_path, sp=seasonal_period)
  
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
  
  X_k <- lapply(X_k, exp)
  
  X_k <- do.call(rbind.fill, X_k)
  
  return(X_k)
  
}

### perform the protection ###

### use a window length = 2x + 1 the sp when sp > 1
### otherwise use 9, which is the same length as
### the shortest window with a seasonal period (quarterly)

for (f in file_names){
  
  # reassign selected features
  sft <- sf
  
  # determine sp
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  if (sp > 1){
    window_length <- 2*sp + 1
  } else if (sp == 1) {
    window_length <- 9
    # remove seasonal_strength from selected features when sp=1
    sft <- sf[!sf %in% c("seasonal_strength")]
  }
  
  for (j in c(3, 5, 7, 10, 15)){
    
    X_knts <- perform_knts(ts_file=f,
                           ts_file_path=fp,
                           seasonal_period=sp,
                           window_length=window_length,
                           k=j,
                           features_to_calculate=fv,
                           selected_features=sft)
    
    write.csv(X_knts, file=paste0(fp, "k-nts-corr_", j, "_", f))
    
  }
}
