## File contains helper functions for the k-nTS+ code files

# Author: Cameron Bale

# function to import and process series
import_data <- function(file_name, file_path, sp, truncate=TRUE, take_log=TRUE){
  
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
  if (truncate){
    ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  }
  
  # take the log of the data
  if (take_log){
    ts_data <- lapply(ts_data, log)
  }
  
  return(ts_data)
}

# function for replacing outliers
outlier_removal <- function(ts){
  temp_ts <- ts
  outlier_test <- tsoutliers(temp_ts, lambda=NULL)
  temp_ts[outlier_test$index] <- outlier_test$replacement
  return(temp_ts)
}

feature_selection <- function(scaled_feature_data, num_rfe_iters, models){
  
  ##############################################################################
  
  relief_start <- Sys.time()
  
  # Stage 1: RReliefF
  evals <- lapply(scaled_feature_data, function(x) attrEval("values", data=x, estimator="RReliefFexpRank"))
  
  evals_combined <- lapply(1:length(evals), function(x) as_tibble(evals[[x]], rownames="feature") %>% mutate(model = models[x]))
  
  evals_combined <- do.call(rbind, evals_combined)
  
  # features selected by RReliefF for each model
  relief_selection <- lapply(evals, function(x) names(x[x > 0]))
  
  relief_stop <- Sys.time()
  
  ##############################################################################
  
  rfe_start <- Sys.time()
  
  # Stage 2: RFE
  # setting seed
  set.seed(42)
  
  # list for oob errors
  oob_list <- list()
  
  # list for variable rankings
  rank_list <- list()
  
  for (i in seq_along(scaled_feature_data)){
    
    df <- scaled_feature_data[[i]]
    
    # list for oob errors
    oob_list[[i]] <- list()
    
    # list for variable rankings
    rank_list[[i]] <- list()
    
    for (j in 1:num_rfe_iters){
      
      # features to consider for cross validation
      rf_feature_names <- relief_selection[[i]]
      
      # out of bag errors
      oob_errors <- c()
      
      # loop variable ranks
      loop_ranks <- c()
      
      while(length(rf_feature_names) > 0){
        
        # create train data
        train <- df[, c("values", rf_feature_names)]
        
        # train random forest with current feature set
        rf_res <- ranger(values ~ ., data=train, importance="permutation", num.trees=500)
        
        oob_errors <- c(oob_errors, mean(abs(rf_res$predictions-train$values)))
        
        least_imp <- names(sort(importance(rf_res))[1])
        
        loop_ranks <- append(loop_ranks, least_imp)
        
        rf_feature_names <- rf_feature_names[rf_feature_names != least_imp]
        
        # print(paste0("Dataframe ", i, ", Iteration ", j, ". Number of features: ", length(rf_feature_names)+1))
        
      }
      
      rank_list[[i]][[j]] <- loop_ranks
      
      oob_list[[i]][[j]] <- oob_errors
      
      # print(paste0("Dataframe ", i, ", Iteration ", j, " complete."))
      
    }
    
  }
  
  avg_oob <- lapply(lapply(oob_list, function(x) do.call(cbind, x)), function(y) rowMeans(y))
  
  combined_oob <- do.call(rbind, lapply(1:length(avg_oob), function(x) tibble("num_features"=length(avg_oob[[x]]):1, "value"=avg_oob[[x]], "model"=models[x])))
  
  ns <- combined_oob %>%
    group_by(model) %>%
    mutate(min_error = min(value)) %>%
    ungroup() %>%
    filter(value == min_error) %>%
    group_by(model) %>%
    summarize(num_selected = min(num_features), .groups='drop') %>%
    mutate(avg_selected = floor(mean(num_selected))) %>%
    distinct(avg_selected) %>%
    pull()
  
  rank_df <- do.call(rbind, lapply(1:length(rank_list), function(y) do.call(rbind, lapply(rank_list[[y]], function(x) tibble("var"=x, "rank"=length(x):1, "model"=models[[y]])))))
  
  sf <- as_tibble(rank_df) %>%
    group_by(var) %>%
    summarize(avg_rank = mean(rank)) %>%
    arrange(avg_rank) %>%
    slice(1:ns) %>%
    pull(var)
  
  # calculate feature importances
  importances <- lapply(scaled_feature_data, function(x) ranger(values ~ ., data=x[,c('values', sf)], importance="permutation", num.trees=500)$variable.importance)
  
  total_importances <- apply(do.call(rbind, importances), 2, sum)
  
  importance_weights <- total_importances/sum(total_importances)
  
  rfe_stop <- Sys.time()
  
  return(list("rank_df" = rank_df, 
              "combined_oob" = combined_oob, 
              "evals_combined" = evals_combined,
              "selected_features" = sf,
              "importance_weights" = importance_weights,
              "relief_time" = difftime(relief_stop, relief_start, units="secs"),
              "rfe_time" = difftime(rfe_stop, rfe_start, units="secs"),
              "rf" = rf_res))
}

knts_alg <- function(time_series, sp, window_length, kvals, features_to_calculate, selected_features, importance_weights=NULL){
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- list()
  
  for(i in seq_along(kvals)){
    X_new[[i]] <- matrix(0.0, nrow=num_periods, ncol=num_series)
  }
  
  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
  
  if (sum(c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5") %in% selected_features) > 0){
    C <- cross_correlations(X_window)
    # calculate the features for the current window
    
    if (sum(!selected_features %in% c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5")) > 0){                                                                                         # reorder to match the weights order
      C <- cbind(tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[, selected_features[!selected_features %in% c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5")]], C)[,selected_features]
    }
    
  } else {
    # calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  }
  
  # replace NA values with 0
  for(i in 1:ncol(C)){
    C[,i][is.na(C[,i])] <- 0
  }
  
  ## allow to remove a constant column if it exists
  to_keep <- apply(C, 2, var) != 0
  C <- C[, to_keep]
  
  # normalize features and convert C to a c x J matrix (num features by num series)
  C <- t(as.data.frame(scale(C)))
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  
  if (!is.null(importance_weights)){
    # create weights matrix
    W <- diag(x=importance_weights[to_keep])
    D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)
  } else {
    D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  }

  ## matrix containing ordered indices of k nearest neighbors of each series
  ## in each column
  index_D <- as.data.frame(apply(D, 2, function(x) sort(x, index.return=TRUE)$ix))
  
  Kmats <- lapply(kvals, function(k) index_D[2:(k+1),])
  
  replacement_indices <- lapply(Kmats, function(y) lapply(y, function(x) sample(x, size=window_length, replace=TRUE)))
  
  for (i in seq_along(kvals)){
    for (j in 1:num_series){
      X_new[[i]][1:window_length, j] <- diag(do.call(rbind, time_series[replacement_indices[[i]][[j]]])[,1:window_length])
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    if (sum(c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5") %in% selected_features) > 0){
      C <- cross_correlations(X_window)
      # calculate the features for the current window
      
      if (sum(!selected_features %in% c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5")) > 0){                                                                                         
        C <- cbind(tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[, selected_features[!selected_features %in% c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5")]], C)
      }
      
      # reorder to match the order of the importance weights
      C <- C[,selected_features]
      
    } else {
      # calculate the features for the current window
      C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    }
    
    for(i in 1:ncol(C)){
      C[,i][is.na(C[,i])] <- 0
    }
    
    ## allow to remove a constant column if it exists
    to_keep <- apply(C, 2, var) != 0
    C <- C[, to_keep]
    
    # normalize features and transpose to a c x J matrix (num features by num series)
    C <- t(as.data.frame(scale(C)))
    
    if (!is.null(importance_weights)){
      # create weights matrix
      W <- diag(x=importance_weights[to_keep])
      ## Calculate the feature distance matrix D
      D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)
    } else {
      ## Calculate the feature distance matrix D
      D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    }
    
    ## matrix containing ordered indices of k nearest neighbors of each series
    ## in each column
    index_D <- as.data.frame(apply(D, 2, function(x) sort(x, index.return=TRUE)$ix))
    
    Kmats <- lapply(kvals, function(k) index_D[2:(k+1),])
    
    replacement_indices <- lapply(Kmats, function(y) sapply(y, function(x) sample(x, size=1)))
    
    X_window <- do.call(rbind, X_window)
    
    for (i in seq_along(kvals)){
      X_new[[i]][t,] <- unname(X_window[,window_length])[replacement_indices[[i]]]
    }
  }
  
  # attempt to remove outliers using tsoutliers
  X_new <- lapply(X_new, function(x) as.list(as.data.frame(x)))
  
  # convert each series to a TS object with appropriate seasonal frequency
  X_new <- lapply(X_new, function(x) lapply(x, function(y) ts(y, frequency=sp)))
  
  X_new <- lapply(X_new, function(x) lapply(x, outlier_removal))
  
  X_new <- lapply(X_new, function(x) as.matrix(do.call(cbind, x)))
  
  return(X_new)
  
}

# function to perform k-nTS/k-nTS+ for different values of k
perform_knts <- function(ts_file, ts_file_path, seasonal_period, window_length, kvals, features_to_calculate, selected_features, is_rate, is_plus=FALSE, importance_weights=NULL){
  
  if (is_rate){
    # read in time series
    X <- import_data(file_name=ts_file, file_path=ts_file_path, sp=seasonal_period, truncate=FALSE, take_log=FALSE)
  } else {
    # read in time series
    X <- import_data(file_name=ts_file, file_path=ts_file_path, sp=seasonal_period)
  }
  
  # split X into separate datasets, one for each series length
  Xs <- list()
  lengths <- sapply(X, length)
  unique_lengths <- unique(lengths)
  for (l in seq_along(unique_lengths)){
    ids <- lengths==unique_lengths[l]
    Xs[[l]] <- X[ids]
  }
  
  if (is_plus){
    X_protected <- lapply(1:length(Xs), function(x) knts_alg(Xs[[x]], sp=seasonal_period, window_length=window_length, kvals=kvals, features_to_calculate=features_to_calculate, selected_features=selected_features[[x]], importance_weights=importance_weights[[x]]))
  } else {
    X_protected <- lapply(Xs, function(x) knts_alg(x, sp=seasonal_period, window_length=window_length, kvals=kvals, features_to_calculate=features_to_calculate, selected_features=selected_features, importance_weights=importance_weights))
  }
  
  # grab the matrices containing the protected data for each value of k
  X_k <- list()
  
  for(i in seq_along(kvals)){
    X_k[[i]] <- lapply(X_protected, function(x) x[[i]])
  }
  
  if (is_rate){
    X_k <- lapply(X_k, function(x) lapply(x, function(y) as.data.frame(t(y))))
    
    X_k <- lapply(X_k, function(x) do.call(rbind.fill, x))
    
  } else {
    X_k <- lapply(X_k, function(x) lapply(x, function(y) as.data.frame(t(y))))
    
    X_k <- lapply(X_k, function(x) do.call(rbind.fill, x))
    
    X_k <- lapply(X_k, exp)
  }
  
  return(X_k)
  
}

# build function to do the trimming for a given time series
knts_bounded <- function(protected_time_series, original_time_series, threshold){
  
  # set threshold
  M <- threshold*sd(original_time_series)
  
  # if the protected value is too small, replace it with A_i - M, otherwise, replace it with A_i + M
  new_protected <- ifelse(original_time_series - protected_time_series > 0, original_time_series - M, original_time_series + M)
  
  # check whether within threshold
  to_keep <- abs(original_time_series - protected_time_series) <= M
  # replace the values we want to keep
  new_protected[to_keep] <- protected_time_series[to_keep]
  
  return(new_protected)
}

# # function to perform k-nTS/k-nTS+
# knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features){
#   
#   # number of time series
#   num_series <- length(time_series)
#   
#   # number of time periods
#   num_periods <- length(time_series[[1]])
#   
#   # matrix to hold new series
#   X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
#   
#   # restrict the data to the beginning window
#   X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
#   
#   # calculate the features for the current window
#   C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#   
#   ## allow to remove a constant column if it exists
#   C <- C[, apply(C, 2, var) != 0]
#   
#   # normalize features and convert C to a c x J matrix (num features by num series)
#   C <- t(as.data.frame(scale(C)))
#   
#   ## Calculate the feature distance matrix D
#   ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
#   D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
#   
#   # for each time period in the initial window
#   for (j in 1:num_series){
#     
#     # sort the distances in the jth column smallest to largest
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
#     
#     # for each series
#     for (t in 1:window_length){
#       
#       # sample an index and replace the value
#       X_new[t,j] <- time_series[[sample(K, size=1)]][t]
#       
#     }
#   }
#   
#   ########################################
#   ### Continue swapping for the rest of the time periods using a rolling window approach
#   ########################################
#   
#   for (t in (window_length+1):num_periods){
#     
#     # restrict the data to the current window
#     X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
#     
#     ## calculate the features for the current window
#     C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#     
#     ## allow to remove a constant column if it exists
#     C <- C[, apply(C, 2, var) != 0]
#     
#     # normalize features and transpose to a c x J matrix (num features by num series)
#     C <- t(as.data.frame(scale(C)))
#     
#     ## Calculate the feature distance matrix D
#     D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
#     
#     for (j in 1:num_series){
#       
#       # sort the distances in the jth column smallest to largest
#       # select from index 2 to k+1 since first index corresponds to the series itself
#       K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
#       
#       # sample an index and replace the value
#       X_new[t,j] <- time_series[[sample(K, size=1)]][t]
#       
#     }
#   }
#   
#   # attempt to remove outliers using tsoutliers
#   X_new <- as.list(as.data.frame(X_new))
#   
#   # convert each series to a TS object with appropriate seasonal frequency
#   X_new <- lapply(X_new, function(x) ts(x, frequency=sp))
#   
#   X_new <- lapply(X_new, outlier_removal)
#   
#   X_new <- as.matrix(do.call(cbind, X_new))
#   
#   return(X_new)
#   
# }

# knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features, importance_weights){
#   
#   # number of time series
#   num_series <- length(time_series)
#   
#   # number of time periods
#   num_periods <- length(time_series[[1]])
#   
#   # matrix to hold new series
#   X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
#   
#   # restrict the data to the beginning window
#   X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
#   
#   # calculate the features for the current window
#   C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#   
#   # normalize features and convert C to a c x J matrix (num features by num series)
#   C <- t(as.data.frame(scale(C)))
#   
#   # create weights matrix
#   W <- diag(x=importance_weights[to_keep])
#   
#   ## Calculate the feature distance matrix D
#   ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
#   D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)
#   
#   # for each time period in the initial window
#   for (j in 1:num_series){
#     
#     # sort the distances in the jth column smallest to largest
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
#     
#     # for each series
#     for (t in 1:window_length){
#       
#       # sample an index and replace the value
#       X_new[t,j] <- time_series[[sample(K, size=1)]][t]
#       
#     }
#   }
#   
#   ########################################
#   ### Continue swapping for the rest of the time periods using a rolling window approach
#   ########################################
#   
#   for (t in (window_length+1):num_periods){
#     
#     # restrict the data to the current window
#     X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
#     
#     ## calculate the features for the current window
#     C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#     
#     ## allow to remove a constant column if it exists
#     to_keep <- apply(C, 2, var) != 0
#     C <- C[, to_keep]
#     
#     # normalize features and transpose to a c x J matrix (num features by num series)
#     C <- t(as.data.frame(scale(C)))
#     
#     # create weights matrix
#     W <- diag(x=importance_weights[to_keep])
#     
#     ## Calculate the feature distance matrix D
#     D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)
#     
#     for (j in 1:num_series){
#       
#       # sort the distances in the jth column smallest to largest
#       # select from index 2 to k+1 since first index corresponds to the series itself
#       K <- sort(D[,j], index.return=TRUE)$ix[2:(k+1)]
#       
#       # sample an index and replace the value
#       X_new[t,j] <- time_series[[sample(K, size=1)]][t]
#       
#     }
#   }
#   
#   # attempt to remove outliers using tsoutliers
#   X_new <- as.list(as.data.frame(X_new))
#   
#   # convert each series to a TS object with appropriate seasonal frequency
#   X_new <- lapply(X_new, function(x) ts(x, frequency=sp))
#   
#   X_new <- lapply(X_new, outlier_removal)
#   
#   X_new <- as.matrix(do.call(cbind, X_new))
#   
#   return(X_new)
#   
# }

