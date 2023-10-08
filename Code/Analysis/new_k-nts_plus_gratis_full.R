### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tidyverse)
library(FNN)
library(feasts)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(ranger)
library(tidytext)
library(CORElearn)
library(gratis)

source("custom_feature_functions.R")

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
  
  ts_data <- lapply(ts_data, log)
  
  return(ts_data)
}

# function to perform two-stage feature selection using RReliefF and RFE
feature_selection <- function(scaled_feature_data, num_rfe_iters){
  
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
        
        print(paste0("Dataframe ", i, ", Iteration ", j, ". Number of features: ", length(rf_feature_names)+1))
        
      }
      
      rank_list[[i]][[j]] <- loop_ranks
      
      oob_list[[i]][[j]] <- oob_errors
      
    }
    
  }
  
  avg_oob <- lapply(lapply(oob_list, function(x) do.call(cbind, x)), function(y) rowMeans(y))
  
  combined_oob <- do.call(rbind, lapply(1:length(avg_oob), function(x) tibble("num_features"=length(avg_oob[[x]]):1, "value"=avg_oob[[x]], "model"=models[x])))
  
  ns <- combined_oob %>%
    group_by(model) %>%
    mutate(min_error = min(value),
           within_5p = ifelse((value-min_error)/min_error <= 0.05, 1, 0)) %>%
    ungroup() %>%
    filter(within_5p == 1) %>%
    group_by(model) %>%
    summarize(num_selected = min(num_features), .groups='drop') %>%
    mutate(avg_selected = floor(mean(num_selected))) %>%
    distinct(avg_selected) %>%
    pull()
  
  rank_df <- do.call(rbind, lapply(1:length(rank_list), function(y) do.call(rbind, lapply(rank_list[[y]], function(x) tibble("var"=x, "rank"=length(x):1, "model"=models[[y]])))))
  
  sf <- rank_df %>%
    group_by(var) %>%
    summarize(avg_rank = mean(rank)) %>%
    arrange(avg_rank) %>%
    slice(1:ns) %>%
    pull(var)
  
  rfe_stop <- Sys.time()
  
  return(list("rank_df" = rank_df, 
              "combined_oob" = combined_oob, 
              "evals_combined" = evals_combined,
              "selected_features" = sf,
              "relief_time" = difftime(relief_stop, relief_start, units="mins"),
              "rfe_time" = difftime(rfe_stop, rfe_start, units="mins"),
              "rf" = rf_res))
  
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# ts_scaler <- function(synthetic_y, y){
#   synthetic_y <- ts(scale(synthetic_y) * sd(y) + mean(y), frequency=frequency(y))
#   return(synthetic_y)
# }

# mar_sim <- function(num_series, length_series, sp){
#   mm <- mar_model(seasonal_periods = sp)
#   sim_series <- mm %>%
#     generate(num_series=num_series,
#              length_series=length_series) 
#   
#   return(sim_series)
# }


simulate_series <- function(ts_file, ts_file_path, sp, num_models, num_series, num_neighbors, features_to_calculate, selected_features){
  
  # read in time series
  X <- import_data(file_name=ts_file, file_path=ts_file_path, sp=sp)
  
  # split X into separate datasets, one for each series length
  Xs <- list()
  unique_lengths <- unique(sapply(X, length))
  lengths <- sapply(X, length)
  for (l in seq_along(unique_lengths)){
    ids <- lengths==unique_lengths[l]
    Xs[[l]] <- X[ids]
  }
  
  sim_series <- list()
  
  for (i in seq_along(Xs)){
    
    overall_mean <- mean(sapply(Xs[[i]], mean))
    
    ss <- list()
    
    for (m in 1:num_models){
      
      simmed <- mar_model(seasonal_periods=sp) %>%
        generate(length=length(Xs[[i]][[1]]), 
                 nseries=num_series) %>%
        as_tibble() %>%
        select(key, value) %>%
        group_split(key, .keep=FALSE)
      
      simmed <- lapply(simmed, function(x) ts(x %>% pull(value), frequency=sp))
      
      mins <- sapply(simmed, min)
      mins <- ifelse(mins < 1, abs(mins), 0)
      
      simmed <- lapply(1:length(simmed), function(x) simmed[[x]] + mins[x] + 1)
      
      simmed <- lapply(simmed, log)
      
      simmed_mean <- mean(sapply(simmed, mean))
      
      # mean center the simulated time series on the overall mean of
      # the confidential series
      simmed <- lapply(simmed, function(x) x - simmed_mean + overall_mean)
      
      ss <- append(ss, simmed)
      
    }
    
    sim_series[[i]] <- ss
    
  }
  
  original_features <- lapply(Xs, function(x) tsfeatures(x, features=features_to_calculate, scale=FALSE)[,selected_features])

  sim_features <- lapply(sim_series, function(x) tsfeatures(x, features=features_to_calculate, scale=FALSE)[,selected_features])
  
  neighbor_ids <- lapply(1:length(sim_features), function(x) unique(as.vector(get.knnx(data=sim_features[[x]], query=original_features[[x]], k=num_neighbors, algorithm=c("kd_tree"))$nn.index)))
  
  sim_series <- lapply(1:length(sim_series), function(x) sim_series[[x]][neighbor_ids[[x]]])
  
  return(sim_series)
  
}

################################################################################
# 
# # now test with comparing to an actual series
# sp <- 12
#  
# X <- import_data("monthly-DEMOGRAPHIC_h1_train.csv", "../../Data/Cleaned/", sp)
# 
# X <- X[18:length(X)]

# # number of mixture components
# K <- sample(c(1,2,3,4,5), size=1)
# 
# # number of autoregressive params
# pvals <- sample(c(0, 1, 2, 3), K, replace=TRUE)
# 
# # number of seasonal autoregressive params
# if (sp > 1){
#   Pvals <- sample(c(0, 1, 2), K, replace=TRUE)
# } else {
#   Pvals <- rep(0, K)
# }
# 
# # differencing params
# # seasonal can only be non-zero when sp > 1
# if (sp > 1){
#   dvals <- rbinom(3, 1, 0.9)
#   Dvals <- rbinom(3, 1, 0.4)
# } else {
#   dvals <- rbinom(3, 1, 0.9)
#   Dvals <- rep(0, K)
# }
# 
# # create matrices for the actual autoregressive params
# phi <- matrix(0, nrow=max(pvals), ncol=K)
# Phi <- matrix(0, nrow=max(Pvals), ncol=K)
# 
# for (i in seq_along(pvals)){
#   if (pvals[i] > 0){
#     phi[1:pvals[i],i] <- rnorm(pvals[i], 0, 0.5)
#   }
#   if (Pvals[i] > 0){
#     Phi[1:Pvals[i],i] <- rnorm(Pvals[i], 0, 0.5)
#   }
# }
# 
# betas <- runif(n=K)
# 
# sigmas <- rlnorm(K, 0.1, 0.1)

# sim_series <- mar_model(seasonal_periods=sp,
#           k=K,
#           weights=betas/sum(betas),
#           sigmas=sigmas,
#           phi=phi,
#           Phi=Phi) %>%
#   generate(length=length(X[[1]]), 
#            nseries=500)

# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# num_series=100
# 
# num_models=250
# 
# # split X into separate datasets, one for each series length
# Xs <- list()
# unique_lengths <- unique(sapply(X, length))
# lengths <- sapply(X, length)
# for (l in seq_along(unique_lengths)){
#   ids <- lengths==unique_lengths[l]
#   Xs[[l]] <- X[ids]
# }
# 
# sim_series <- list()
# 
# for (i in seq_along(Xs)){
#   
#   overall_mean <- mean(sapply(Xs[[i]], mean))
#   
#   ss <- list()
#   
#   for (m in 1:num_models){
#     
#     simmed <- mar_model(seasonal_periods=sp) %>%
#       generate(length=length(Xs[[i]][[1]]), 
#                nseries=num_series) %>%
#       as_tibble() %>%
#       select(key, value) %>%
#       group_split(key, .keep=FALSE)
#     
#     simmed <- lapply(simmed, function(x) ts(x %>% pull(value), frequency=sp))
#     
#     mins <- sapply(simmed, min)
#     mins <- ifelse(mins < 1, abs(mins), 0)
#     
#     simmed <- lapply(1:length(simmed), function(x) simmed[[x]] + mins[x] + 1)
#     
#     simmed <- lapply(simmed, log)
#     
#     simmed_mean <- mean(sapply(simmed, mean))
#     
#     # mean center the simulated time series on the overall mean of
#     # the confidential series
#     simmed <- lapply(simmed, function(x) x - simmed_mean + overall_mean)
#     
#     ss <- append(ss, simmed)
#     
#   }
#   
#   sim_series[[i]] <- ss
#   
# }
# 
# original_features <- lapply(Xs, function(x) tsfeatures(x, features=fv, scale=FALSE))
# 
# sim_features <- lapply(sim_series, function(x) tsfeatures(x, features=fv, scale=FALSE))
# 
# neighbor_ids <- lapply(1:length(sim_features), function(x) unique(as.vector(get.knnx(data=sim_features[[x]], query=original_features[[x]], k=15, algorithm=c("kd_tree"))$nn.index)))
# 
# sim_series <- lapply(1:length(sim_series), function(x) sim_series[[x]][neighbor_ids[[x]]])

########## make sure this is implemented only for series of the same length

# overall_mean <- mean(sapply(X, mean))

# generate 50,000 simulated time series
# every 100 series come from a different MAR model

# perform mean centering on each set of simulated time 
# series

# overall_mean <- mean(sapply(X, mean))
# 
# sim_series <- list()
# for (i in 1:500){
#   
#   ss <- mar_model(seasonal_periods=sp) %>%
#     generate(length=length(X[[1]]), 
#              nseries=100) %>%
#     as_tibble() %>%
#     select(key, value) %>%
#     group_split(key, .keep=FALSE)
#   
#   ss <- lapply(ss, function(x) ts(x %>% pull(value), frequency=sp))
#   
#   mins <- sapply(ss, min)
#   mins <- ifelse(mins < 1, abs(mins), 0)
#   
#   ss <- lapply(1:length(ss), function(x) ss[[x]] + mins[x] + 1)
#   
#   ss <- lapply(ss, log)
#   
#   ss_mean <- mean(sapply(ss, mean))
# 
#   # mean center the simulated time series on the overall mean of
#   # the confidential series
#   ss <- lapply(ss, function(x) x - ss_mean + overall_mean)
#   
#   sim_series <- append(sim_series, ss)
#   
# }

# sim_series <- sim_series[!sapply(sim_series, function(x) any(is.infinite(x)))]


## now find the 15 most similar series on features to the existing series
## and keep those

## Calculate the feature distance matrix D
# D_calc <- function(ts_data){
#   ones_column <- as.matrix(rep(1, ncol(ts_data)), nrow=ncol(ts_data))
#   temp <- ones_column %*% diag(t(ts_data)%*%ts_data) - 2*t(ts_data)%*%ts_data + diag(t(ts_data)%*%ts_data) %*% t(ones_column)
#   return(temp)
# }

# use a kd-tree to perform nearest neighbor searching
# need input matrices

# neighbor_ids <- unique(as.vector(get.knnx(data=do.call(rbind, sim_series), query=do.call(rbind, X), k=3, algorithm=c("kd_tree"))$nn.index))
# 
# sim_series <- sim_series[neighbor_ids]
# 
# ## look at feature space of original and simulated series
# 
# feature_calculator <- function(ts, features_to_calculate, scale_series, sp){
# 
#   temp <- tsfeatures(ts, features=features_to_calculate, scale=scale_series) %>%
#     select(-nperiods, -seasonal_period)
# 
#   if(sp > 1){
#     temp <- temp %>%
#       select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
#   }
# 
#   return(temp)
# }




# qf_features <- feature_calculator(X, fv, scale=FALSE, sp)
# 
# sim_features <- feature_calculator(sim_series[[1]], fv, scale=FALSE, sp)
# 
# qf_pca <- prcomp(qf_features, center=FALSE, scale=FALSE)
# 
# qf_pcs <- as_tibble(qf_pca$x[,1:2]) %>%
#   mutate(data="monthly-DEMOGRAPHIC")
# 
# pcs <- as_tibble(predict(qf_pca, sim_features)[,1:2]) %>%
#   mutate(data="synthetic")
# 
# combined_pcs <- bind_rows(qf_pcs, pcs)
# 
# combined_pcs %>%
#   ggplot(aes(x=PC1, y=PC2)) +
#   geom_point() +
#   facet_wrap(~data)

# temp <- unlist(lapply(X, function(x) simulate_series(x, num_models, 15)), recursive=FALSE)
#
#
#
#
# X_sim <- lapply(1:40, function(x) simulate_series(num_series=500,
#                                                   length_series=46,
#                                                   sp=1))
#
# X_sim <- unlist(X_sim, recursive=FALSE)
#
# # scale series to match target series
#
# X_sim <- lapply(X_sim, function(x) ts_scaler(x, X[[1]]))

# # vector of feature names to calculate in k-nTS+
# fv <- c("entropy", "lumpiness", "stability",
#         "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
#         "crossing_points", "flat_spots", "hurst",
#         "unitroot_kpss", "unitroot_pp", "stl_features",
#         "acf_features", "pacf_features",
#         "nonlinearity", "series_mean", "series_variance",
#         "skewness", "kurtosis")
#
# sf <- c("trend",
#         "unitroot_pp",
#         "spike",
#         "max_var_shift",
#         "max_level_shift")
# #
# # # vector of feature names to calculate in k-nTS+
# temp_fv <- c("entropy", "lumpiness", "stability",
#         "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
#         "crossing_points", "flat_spots", "hurst",
#         "unitroot_kpss", "unitroot_pp", "stl_features",
#         "acf_features", "pacf_features",
#         "nonlinearity", "series_mean", "series_variance",
#         "skewness", "kurtosis")
#
# temp_sf <- c("trend",
#         "unitroot_pp",
#         "spike",
#         "max_var_shift",
#         "max_level_shift")

# function to return vector of feature values
# feature_calculator <- function(y) {
#   return(deframe(gather(tsfeatures(y, features=temp_fv, scale=FALSE)[temp_sf])))
# }

# y_features <- feature_calculator(temp_data[[1]])
#
# fake_series <- generate_target(nseries=15,
#                                length = length(temp_data[[1]]),
#                                seasonal_periods=1,
#                                feature_function = feature_calculator,
#                                target = y_features,
#                                tolerance=0.3)
#
# temp <- fake_series %>%
#   select(-index) %>%
#   group_split(key, .keep=FALSE)
#
# temp <- lapply(temp, function(x) x %>% pull(value))
#
# temp <- lapply(temp, function(x) ts_scaler(x, temp_data[[1]]))
#
# temp <- lapply(1:length(temp), function(x) tibble(index=1:length(temp[[x]]),
#                                                   key=paste0("Series ", x),
#                                                   value=temp[[x]][1:length(temp[[x]])],
#                                                   synthetic=1))
#
# temp[[21]] <- tibble(index=1:length(temp_data[[1]]),
#                             key="Series Original",
#                             value=temp_data[[1]][1:length(temp_data[[1]])],
#                             synthetic=0)
#
# temp <- do.call(rbind, temp)
#
# temp %>%
#   ggplot(aes(x=index, y=value, color=key)) +
#   geom_line() +
#   facet_wrap(~synthetic)
#
#


# # function to generate a bunch of synthetic series that target the feature
# # values of the provided series
# synthetic_generator <- function(real_series, features_to_calculate, selected_features, num_to_generate, feature_tol){
# 
#   # used internally by feature calculation function `feature_calculator`
#   temp_fv <<- features_to_calculate[!features_to_calculate %in% c("series_mean", "series_variance")]
#   temp_sf <<- selected_features[!selected_features %in% c("series_mean", "series_variance")]
# 
#   y_features <- feature_calculator(real_series)
# 
#   fake_series <- generate_target(nseries=num_to_generate,
#                                  length = length(real_series),
#                                  seasonal_periods=frequency(real_series),
#                                  feature_function = feature_calculator,
#                                  target = y_features,
#                                  tolerance=feature_tol)
# 
#   temp <- fake_series %>%
#     select(-index) %>%
#     group_split(key, .keep=FALSE)
# 
#   temp <- lapply(temp, function(x) x %>% pull(value))
# 
#   temp <- lapply(temp, function(x) ts_scaler(x, real_series))
# 
#   return(temp)
# }
# 
# augment_data <- function(full_real_series, features_to_calculate, selected_features, num_to_generate, feature_tol){
# 
#   synth_series <- lapply(full_real_series, function(x) synthetic_generator(real_series=x,
#                                                                            features_to_calculate=features_to_calculate,
#                                                                            selected_features=selected_features,
#                                                                            num_to_generate=num_to_generate,
#                                                                            feature_tol=feature_tol))
# 
#   synth_series <- unlist(synth_series, recursive=FALSE)
# 
#   return(append(full_real_series, synth_series))
# 
# }


# 
# 
# 
# 
# 
# 
# start <- Sys.time()
# ttt <- augment_data(temp_data,
#                     features_to_calculate = fv,
#                     selected_features = sf,
#                     num_to_generate = 15,
#                     feature_tol = 0.4)
# stop <- Sys.time()
# 
# stop-start














################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

knts_alg <- function(time_series, synthetic_series, sp, window_length, kvals, features_to_calculate, selected_features, corr_based=FALSE){
  
  # number of time series to actually protect
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # append synthetic series on to original series
  full_series <- append(time_series, synthetic_series)
  
  names(full_series) <- as.character(c(1:length(full_series)))

  # restrict the data to the beginning window
  X_window <- lapply(full_series, function(x) ts(x[1:window_length], frequency=sp))
  
  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
  # normalize features
  # C <- as.data.frame(scale(C))
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, length(full_series)), nrow=length(full_series))
  D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  
  X_new_list <- list()
  
  for (i in seq_along(kvals)){
    # matrix to hold new series
    X_new_list[[i]] <- matrix(0.0, nrow=num_periods, ncol=num_series)
  }
  
  # for each series we need to protect
  # these are the first J series in the augmented data
  for (j in 1:num_series){
    
    # select the jth column
    d <- D[,j]
    
    # sort the distances in the jth column smallest to largest
    sorted <- sort(d, index.return=TRUE)
    
    # select from index 2 to k+1 since first index corresponds to the series itself
    K <- sorted$ix[2:(max(kvals)+1)]
    
    # for each series
    for (t in 1:window_length){
      
      for (k in seq_along(kvals)){
        
        # sample an index
        i <- sample(K[1:kvals[[k]]], size=1)
        
        # replace the value
        X_new_list[[k]][t,j] <- full_series[[i]][t]
        
      }
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(full_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)
    
    ## Calculate the feature distance matrix D
    D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    
    for (j in 1:num_series){
      
      # select the jth column
      d <- D[,j]
      
      # sort the distances in the jth column smallest to largest
      sorted <- sort(d, index.return=TRUE)
      
      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sorted$ix[2:(max(kvals)+1)]
      
      for (k in seq_along(kvals)){
          
        # sample an index
        i <- sample(K[1:kvals[[k]]], size=1)
          
        # replace the value
        X_new_list[[k]][t,j] <- full_series[[i]][t]
          
      }
    }
  }
  
  return(X_new_list)
  
}

perform_knts <- function(ts_file, ts_file_path, seasonal_period, window_length, kvals, features_to_calculate, selected_features, synth_series){

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
  
  X_k <- lapply(1:length(Xs), function(x) knts_alg(Xs[[x]], sp=seasonal_period, synthetic_series=synth_series[[x]], window_length=window_length, kvals=kvals, features_to_calculate=features_to_calculate, selected_features=selected_features))
  
  X_k_combined <- lapply(1:length(kvals), function(x) lapply(1:length(Xs), function(y) X_k[[y]][[x]]))
  
  X_k_combined <- lapply(X_k_combined, function(x) lapply(x, function(y) as.data.frame(t(y))))

  X_k_combined <- lapply(X_k_combined, function(y) lapply(y, exp))

  X_k_combined <- lapply(X_k_combined, function(x) do.call(rbind.fill, x))

  return(X_k_combined)

}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# paths to the data files and feature files
fp <- "../../Data/Cleaned/"
features_path <- "../../Data/Features/"
# path to files with error distributions
ed_file_path <- "../../Outputs/Results/Error_Distributions/"

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

## create file to track computation time for protecting data sets with
# k-nTS+
computation_time <- tibble()

# vector of feature names to calculate in k-nTS+
fv <- c("entropy", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# number of RFE iterations
num_iter <- 25

# track computation time for k-nTS+ swapping

feature_file_names <- grep("h2_train", list.files("../../Data/Features/"), value=TRUE)

# loop over file names
for (f in file_names){
  
  # store file prefix
  prefix <- strsplit(f, split="_")[[1]][1]
  
  ## time for feature processing and preparation
  feature_start <- Sys.time()
  
  # file with errors for all models on the original data
  eds <- read_csv(paste0(ed_file_path, paste0(prefix, "_all_distributions_h2.csv")))
  
  # transform to tidy
  eds <- eds %>% gather(key="name", value="values") %>%
    mutate(name = substring(name, 1, nchar(name)-8)) %>%
    separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_") %>%
    mutate(Protection = if_else(is.na(Protection), "Original", ifelse(Protection == prefix, "Original", Protection)),
           Parameter = if_else(is.na(Parameter), "Original", Parameter)) %>%
    select(-Data)
  
  ### now import corresponding time series features and link to forecast errors
  current_feature_file_names <- grep(prefix, feature_file_names, value=TRUE)
  
  full_features <- tibble()
  
  # import protected features and assign new variable values for linking to errors
  for (ff in current_feature_file_names){
    
    features <- read_csv(paste0("../../Data/Features/", ff))
    
    params <- strsplit(ff, split="_")[[1]]
    
    if (!params[2] %in% c("AN", "DP")){
      features["Protection"] <- "Original"
      features["Parameter"] <- "Original"
    } else {
      features["Protection"] <- params[2]
      features["Parameter"] <- params[3]
    }
    
    full_features <- bind_rows(full_features, features)
    
  }
  
  ## join features to errors
  split_eds <- eds %>% 
    group_by(Model, Protection, Parameter) %>%
    group_split()
  
  full_data <- list()
  
  for (i in seq_along(split_eds)){
    pps <- split_eds[[i]] %>% distinct(Protection, Parameter)
    prot <- pps %>% pull(Protection)
    param <- pps %>% pull(Parameter)
    feat_df <- full_features %>%
      filter(Protection==prot, Parameter==param) %>%
      select(-Protection, -Parameter)
    new <- split_eds[[i]] %>%
      bind_cols(feat_df)
    full_data[[i]] <- new
    print(i)
  }
  
  full_data <- do.call(rbind, full_data)
  
  ############################
  ############################
  
  full_data <- full_data %>%
    group_split(Model)
  
  models <- unlist(lapply(full_data, function(x) distinct(x, Model)))
  
  full_data <- lapply(full_data, function(x) x %>% select(-Model, -Horizon, -Protection, -Parameter))
  
  full_data_scaled <- lapply(full_data, function(x) as.data.frame(scale(x)))
  
  feature_stop <- Sys.time()
  
  #########################################
  
  ############# ############# #############
  
  ## Perform feature selection
  
  fsr <- feature_selection(full_data_scaled, num_iter)
  
  ## save RReliefF feature rankings (across forecasting models)
  
  print("Feature selection done.")
  
  # check if sub directory exists 
  if (file.exists("../../Outputs/RReliefF Rankings/")){
    
    write.csv(fsr[["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/RReliefF_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path("../../Outputs/RReliefF Rankings/"))
    
    # specifying the working directory
    write.csv(fsr[["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/RReliefF_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  ## save RFE feature rankings
  
  # check if sub directory exists 
  if (file.exists("../../Outputs/RFE Rankings/")){
    
    write.csv(fsr[["rank_df"]], file=paste0("../../Outputs/RFE Rankings/RFE_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path("../../Outputs/RFE Rankings/"))
    
    # specifying the working directory
    write.csv(fsr[["rank_df"]], file=paste0("../../Outputs/RFE Rankings/RFE_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  ## save RFE oob results
  
  # check if sub directory exists 
  if (file.exists("../../Outputs/RFE OOB/")){
    
    write.csv(fsr[["combined_oob"]], file=paste0("../../Outputs/RFE OOB/RFE_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path("../../Outputs/RFE OOB/"))
    
    # specifying the working directory
    write.csv(fsr[["combined_oob"]], file=paste0("../../Outputs/RFE OOB/RFE_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  print("File saving done.")
  
  sf <- fsr[["selected_features"]]
  
  # reassign selected features
  sft <- sf
  
  ### use a window length = 2x + 1 the sp when sp > 1
  ### otherwise use 9, which is the same length as
  ### the shortest window with a seasonal period (quarterly)
  
  # determine sp
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # minimum window length of 11 so that x_acf10 can be calculated
  window_length <- max(c(2*sp + 1, 11))
  
  swap_times <- c()
  corr_swap_times <- c()
  
  kvals <- c(3, 5, 7, 10, 15)
  
  synthetic_series <- simulate_series(ts_file=f, 
                                      ts_file_path=fp, 
                                      sp=sp, 
                                      num_models=200, 
                                      num_series=100, 
                                      num_neighbors=max(kvals),
                                      features_to_calculate=fv, 
                                      selected_features=sft)
  
  X_knts <- perform_knts(ts_file=f,
                         ts_file_path=fp,
                         seasonal_period=sp,
                         window_length=window_length,
                         k=kvals,
                         features_to_calculate=fv,
                         selected_features=sft,
                         synth_series=synthetic_series)
  
  for (i in 1:length(X_knts)){
    write.csv(X_knts[[i]], file=paste0(fp, "gratis-full-k-nts-plus_", kvals[i], "_", f), row.names=FALSE)
  }
}
    

    
    # swap_start <- Sys.time()
    # 
    # 
    # 
    # swap_stop <- Sys.time()
    # 
    # swap_times <- c(swap_times, difftime(swap_stop, swap_start, units="mins"))
    
    
    
    ############################################################################
    
    # swap_start <- Sys.time()
    # 
    # X_knts_cor <- perform_knts(ts_file=f,
    #                            ts_file_path=fp,
    #                            seasonal_period=sp,
    #                            window_length=window_length,
    #                            k=j,
    #                            features_to_calculate=fv,
    #                            selected_features=sft,
    #                            corr_based=TRUE)
    # 
    # swap_stop <- Sys.time()
    # 
    # print("Stopping swapping.")
    # 
    # corr_swap_times <- c(corr_swap_times, difftime(swap_stop, swap_start, units="mins"))
    # 
    # write.csv(X_knts_cor, file=paste0(fp, "k-nts-plus-corr_", j, "_", f), row.names=FALSE)
    

  
  # computation_row <- tibble(File = f,
  #                           feature_prep = difftime(feature_stop, feature_start, units="mins"),
  #                           RReliefF = fsr[["relief_time"]],
  #                           RFE = fsr[["rfe_time"]],
  #                           swap3 = swap_times[1],
  #                           swap5 = swap_times[2],
  #                           swap7 = swap_times[3],
  #                           swap10 = swap_times[4],
  #                           swap15 = swap_times[5],
  #                           corswap3 = corr_swap_times[1],
  #                           corswap5 = corr_swap_times[2],
  #                           corswap7 = corr_swap_times[3],
  #                           corswap10 = corr_swap_times[4],
  #                           corswap15 = corr_swap_times[5],)
  # 
  # computation_time <- bind_rows(computation_time, computation_row)
  


# write.csv(computation_time, file="../../Data/Computation Results/k-nts-plus.csv", row.names=FALSE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

## gotta do the step through. Last attempt.
