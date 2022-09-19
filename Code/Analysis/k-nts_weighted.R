### Perform k-NTS Data Protection

# Author: Cameron Bale

# Approach 1: Include the mean in the distance measurement.

# ------------------------------------------------------------------------- #

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

# vector of features to calculate
fv <- c("acf_features", "stability", "stl_features", "hurst")

# vector of features to select
sf <- c("linearity", "hurst", "trend", "stability", "x_acf1")

# split X into three separate datasets, one for each series length
Xs <- list()
unique_lengths <- unique(sapply(X, length))
lengths <- sapply(X, length)
for (l in seq_along(unique_lengths)){
  ids <- lengths==unique_lengths[l]
  sub_X <- X[ids]
  Xs[[l]] <- sub_X
}

# # convert each subset of series into a dataframe
# Xs <- lapply(Xs, as.data.frame)

# # normalize the features
# Cs <- lapply(Cs, function(x) as.data.frame(scale(x)))
# 
# # transpose the feature vectors
# Cs <- lapply(Cs, function(x) as.data.frame(t(x)))

# ## double check correct dimensions
# # series dimensions
# for (x in Xs){
#   print(dim(x))
# }
# # feature dimensions
# for (x in Cs){
#   print(dim(x))
# }

### Now perform the swapping

# # feature matrix
# C <- as.matrix(Cs[[1]])
# series matrix
# X <- Xs[[1]]
# # number of time series
# num_series <- length(X)
# # number of time periods
# num_periods <- length(X[[1]])
# # length of rolling window
# window_length <- 25
# # number of nearest series to consider for swapping
# k <- 3
# 
# # matrix to hold new series
# X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
# 
# ## restrict the data to the current window
# X_window <- lapply(X, function(x) ts(x[1:window_length], frequency=12))
# 
# ## calculate the features for the current window
# C <- tsfeatures(X_window, features=fv)
# 
# # select desired features
# C <- C[,c("trend", "seas_acf1", "x_acf1", "hurst", "stability")]
# 
# # calculate the mean of each series and include as a feature
# means <- sapply(X_window, mean)
# 
# # include the means as a feature
# C[,"mean"] <- means
# 
# # convert C to a c x J matrix (num features by num series)
# C <- t(C)
# 
# ## Calculate the feature distance matrix D
# ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
# 
# D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)

# ## choose one of k nearest series and select the last value of that series
# for (j in 1:num_series){
#   
#   # select the jth column
#   d <- D[,j]
#   
#   # sort the distances in the jth column smallest to largest
#   sorted <- sort(d, index.return=TRUE)
#   
#   # select from index 2 to k+1 since first index corresponds to the series itself
#   K <- sorted$ix[2:k+1]
#   
#   # sample an index
#   # later can implement sampling based on distance based probabilities
#   i <- sample(K, size=1)
#   
#   val <- X[num_periods,i]
#   
#   print(c(i, val))
#   
# }

## Now do it for the full series, replacing values in the first window based on a single set of characteristics,
## then roll forward by one period and do it repeatedly until the end of the series

## Swapping for initial window

# # for each time period in the initial window
# for (t in 1:window_length){
#   
#   # for each series
#   for (j in 1:num_series){
#     
#     # select the jth column
#     d <- D[,j]
#     
#     # sort the distances in the jth column smallest to largest
#     sorted <- sort(d, index.return=TRUE)
#     
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sorted$ix[2:k+1]
#     
#     # sample an index
#     # later can implement sampling based on distance based probabilities
#     i <- sample(K, size=1)
#     
#     # replace the value
#     X_new[t,j] <- X[[i]][t]
#     
#   }
# }

########################################
### Continue swapping for the rest of the time periods using a rolling window approach
########################################

# for (t in (window_length+1):num_periods){
#   
#   # restrict the data to the current window
#   X_window <- lapply(X, function(x) ts(x[(t-window_length+1):t], frequency=12))
# 
#   ## calculate the features for the current window
#   C <- tsfeatures(X_window, features=fv)[,c("trend", "seas_acf1", "x_acf1", "hurst", "stability")]
# 
#   # calculate the mean of each series and include as a feature
#   C[,"mean"] <- sapply(X_window, mean)
# 
#   # transpose C to a c x J matrix (num features by num series)
#   C <- t(C)
# 
#   ## Calculate the feature distance matrix D
#   ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
#   D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
# 
#   for (j in 1:num_series){
# 
#     # select the jth column
#     d <- D[,j]
# 
#     # sort the distances in the jth column smallest to largest
#     sorted <- sort(d, index.return=TRUE)
# 
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sorted$ix[2:k+1]
# 
#     # sample an index
#     # later can implement sampling based on distance based probabilities
#     i <- sample(K, size=1)
# 
#     # replace the value
#     X_new[t,j] <- X[[i]][t]
# 
#   }
# }

## Now do it for the full series, replacing values in the first window based on a single set of characteristics,
## then roll forward by one period and do it repeatedly until the end of the series

## Swapping for initial window

# # for each time period in the initial window
# for (t in 1:window_length){
#   
#   # for each series
#   for (j in 1:num_series){
#     
#     # select the jth column
#     d <- D[,j]
#     
#     # sort the distances in the jth column smallest to largest
#     sorted <- sort(d, index.return=TRUE)
#     
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sorted$ix[2:k+1]
#     
#     # sample an index
#     # later can implement sampling based on distance based probabilities
#     i <- sample(K, size=1)
#     
#     # replace the value
#     X_new[t,j] <- X[[i]][t]
#     
#   }
# }

###################################################################################
###################################################################################
# Implement the above in a function
###################################################################################
###################################################################################

k_nts <- function(time_series, window_length, k, features_to_calculate, selected_features){
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
  
  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=12))
  
  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate)[,selected_features]
  
  # calculate the mean of each series and include as a feature
  # C[,"mean"] <- sapply(X_window, mean)
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  ## Calculate the feature distance matrix D_f
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  D_f <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  
  ## Calculate the series distance matrix D_x
  X_window_mat <- do.call(cbind, X_window)
  D_x <- ones_column %*% diag(t(X_window_mat)%*%X_window_mat) - 2*t(X_window_mat)%*%X_window_mat + diag(t(X_window_mat)%*%X_window_mat) %*% t(ones_column)
  
  # for each time period in the initial window
  for (t in 1:window_length){
    
    # for each series
    for (j in 1:num_series){
      
      # select the jth columns
      d_f <- D_f[,j]
      d_x <- D_x[,j]
      
      # sort the distances in the jth column smallest to largest
      sorted <- sort(d_f, index.return=TRUE)
      
      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sorted$ix[2:k+1]
      
      # subset value distances based on K
      value_distances <- 1/d_x[K]
      
      # convert to probabilities
      probs <- sapply(value_distances, function(x) x/sum(value_distances))
      
      # sample an index
      # later can implement sampling based on distance based probabilities
      i <- sample(K, size=1, prob=probs)
      
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
    C <- tsfeatures(X_window, features=features_to_calculate)[,selected_features]
    
    # calculate the mean of each series and include as a feature
    # C[,"mean"] <- sapply(X_window, mean)
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)
  
    ## Calculate the feature distance matrix D_f
    D_f <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    
    ## Calculate the series distance matrix D_x
    X_window_mat <- do.call(cbind, X_window)
    D_x <- ones_column %*% diag(t(X_window_mat)%*%X_window_mat) - 2*t(X_window_mat)%*%X_window_mat + diag(t(X_window_mat)%*%X_window_mat) %*% t(ones_column)
    
    for (j in 1:num_series){
      
      # select the jth columns
      d_f <- D_f[,j]
      d_x <- D_x[,j]
      
      # sort the distances in the jth column smallest to largest
      sorted <- sort(d_f, index.return=TRUE)
      
      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sorted$ix[2:k+1]
      
      # subset value distances based on K
      value_distances <- 1/d_x[K]
      
      # convert to probabilities
      probs <- sapply(value_distances, function(x) x/sum(value_distances))
      
      # sample an index
      # later can implement sampling based on distance based probabilities
      i <- sample(K, size=1, prob=probs)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  return(X_new)
  
}

# perform data protection for three values of k = {5, 10, 15}

library(plyr)
library(tsfeatures)

X_k5 <- lapply(Xs, function(x) k_nts(x, window_length=25, k=5, features_to_calculate=fv, selected_features=sf))

X_k5 <- lapply(X_k5, function(x) as.data.frame(t(x)))

X_k5 <- lapply(X_k5, exp)

X_k5 <- do.call(rbind.fill, X_k5)

X_k10 <- lapply(Xs, function(x) k_nts(x, window_length=25, k=10, features_to_calculate=fv, selected_features=sf))

X_k10 <- lapply(X_k10, function(x) as.data.frame(t(x)))

X_k10 <- lapply(X_k10, exp)

X_k10 <- do.call(rbind.fill, X_k10)

X_k15 <- lapply(Xs, function(x) k_nts(x, window_length=25, k=15, features_to_calculate=fv, selected_features=sf))

X_k15 <- lapply(X_k15, function(x) as.data.frame(t(x)))

X_k15 <- lapply(X_k15, exp)

X_k15 <- do.call(rbind.fill, X_k15)

write.csv(X_k5, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_weighted_k-nts_5.csv", row.names=FALSE)

write.csv(X_k10, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_weighted_k-nts_10.csv", row.names=FALSE)

write.csv(X_k15, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_weighted_k-nts_15.csv", row.names=FALSE)



