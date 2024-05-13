### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

###
###
###
###
###
###
###
###

library(plyr)
library(tidyverse)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(ranger)
library(tidytext)
library(CORElearn)

data_folder <- "M3/"

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
  
  # take the log of the data
  ts_data <- lapply(ts_data, log)
  
  return(ts_data)
}

# function to perform two-stage feature selection using RReliefF and RFE
feature_selection <- function(scaled_feature_data, scaled_diff_data, num_rfe_iters){
  
  ##############################################################################
  
  relief_start <- Sys.time()
  
  # Stage 1: RReliefF
  evals <- lapply(scaled_feature_data, function(x) attrEval("Forecasts", data=x, estimator="RReliefFexpRank"))
  
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
  
  for (i in seq_along(scaled_diff_data)){
    
    df <- scaled_diff_data[[i]]
    
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
        train <- df[, c("fcast_diff", rf_feature_names)]
        
        # train random forest with current feature set
        rf_res <- ranger(fcast_diff ~ ., data=train, importance="permutation", num.trees=500)
        
        oob_errors <- c(oob_errors, mean(abs(rf_res$predictions-train$fcast_diff)))
        
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

knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
  
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
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  if ("x_acf1" %in% rownames(C)){
    C["x_acf1",][is.na(C["x_acf1",])] <- 0.0 
  }
  
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
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)
    
    if ("x_acf1" %in% rownames(C)){
      C["x_acf1",][is.na(C["x_acf1",])] <- 0.0 
    }
    
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
      
      # sample an index
      i <- sample(K, size=1)
    
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  print("swapping complete.")
  
  return(X_new)
  
}

perform_knts <- function(ts_file, ts_file_path, seasonal_period, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
  
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
  
  X_k <- lapply(Xs, function(x) knts_alg(x, sp=seasonal_period, window_length=window_length, k=k, features_to_calculate=features_to_calculate, selected_features=selected_features, corr_based=corr_based))
  
  X_k <- lapply(X_k, function(x) as.data.frame(t(x)))
  
  X_k <- lapply(X_k, exp)
  
  X_k <- do.call(rbind.fill, X_k)
  
  return(X_k)
  
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# paths to the data files and feature files
fp <- paste0("../../Data/Cleaned/", data_folder)
features_path <- paste0("../../Data/Features/", data_folder)
# path to files with error distributions
ed_file_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

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
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# number of RFE iterations
num_iter <- 25

# import names of files containing features
feature_file_names <- grep("h2_train", list.files(features_path), value=TRUE)

### import all forecasts
# all forecasts
# filter for just the ones using h2
all_fcasts <- grep("_h2_", list.files(paste0("../../Outputs/Forecasts/", data_folder)), value=TRUE)

# loop over file names
for (f in file_names){
  
  # store file prefix
  prefix <- strsplit(f, split="_")[[1]][1]
  
  ## time for feature processing and preparation
  feature_start <- Sys.time()
  
  ### now import corresponding time series features and link to forecast errors
  current_feature_file_names <- grep(prefix, feature_file_names, value=TRUE)
  
  original_feature_file_name <- grep("AN_", current_feature_file_names, value=TRUE, invert=TRUE)
  original_feature_file_name <- grep("DP_", original_feature_file_name, value=TRUE, invert=TRUE)
  protected_feature_file_names <- c(grep("AN_", current_feature_file_names, value=TRUE), grep("DP_", current_feature_file_names, value=TRUE))
  
  ### import all features
  full_features <- tibble()
  
  # import protected features and assign new variable values for linking to errors
  for (ff in c(protected_feature_file_names, original_feature_file_name)){
    
    features <- read_csv(paste0("../../Data/Features/M3/", ff))
    
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
  
  # filter for just the ones for the current file
  current_all_fcasts <- grep(prefix, all_fcasts, value=TRUE)
  
  original_fcasts <- grep("_AN_", current_all_fcasts, value=TRUE, invert=TRUE)
  original_fcasts <- grep("_DP_", original_fcasts, value=TRUE, invert=TRUE)
  
  protected_fcasts <- c(grep("_AN_", current_all_fcasts, value=TRUE), grep("_DP_", current_all_fcasts, value=TRUE))
  
  # create tibble containing all forecasts
  fcasts <- tibble()
  
  for (ff in c(protected_fcasts, original_fcasts)){
    
    temp_fcasts <- unname(as_vector(read_csv(paste0("../../Outputs/Forecasts/", data_folder, ff))))
    
    finfo <- strsplit(ff, split="_")[[1]]
    
    if (!finfo[3] %in% c("AN", "DP")){
      fc <- tibble(Model=finfo[1],
                   Data=finfo[3],
                   Protection="Original",
                   Parameter="Original",
                   Forecasts=temp_fcasts)
    } else {
      fc <- tibble(Model=finfo[1],
                   Data=finfo[5],
                   Protection=finfo[3],
                   Parameter=finfo[4],
                   Forecasts=temp_fcasts)
    }
    
    fcasts <- bind_rows(fcasts, fc)
    
  }
  
  ## join features to forecasts
  split_fcasts <- fcasts %>% 
    group_by(Model, Protection, Parameter) %>%
    group_split()
  
  full_data <- list()
  
  for (i in seq_along(split_fcasts)){
    pps <- split_fcasts[[i]] %>% distinct(Protection, Parameter)
    prot <- pps %>% pull(Protection)
    param <- pps %>% pull(Parameter)
    feat_df <- full_features %>%
      filter(Protection==prot, Parameter==param) %>%
      select(-Protection, -Parameter)
    new <- split_fcasts[[i]] %>%
      bind_cols(feat_df)
    full_data[[i]] <- new
    print(i)
  }
  
  full_data <- do.call(rbind, full_data)
  
  ########### Now have all forecasts and features for the original and protected
  ########### data set across all forecasting models
  
  ############################
  ############################
  
  full_data <- full_data %>%
    group_split(Model)
  
  models <- unlist(lapply(full_data, function(x) distinct(x, Model)))
  
  full_data <- lapply(full_data, function(x) x %>% select(-Model, -Protection, -Parameter, -Data))
  
  full_data_scaled <- lapply(full_data, function(x) as.data.frame(scale(x)))
  
  ########### Now calculate feature and forecast differences for RFE
  
  all_protected_fcasts <- fcasts %>%
    filter(Protection != "Original") %>%
    group_split(Model, Protection, Parameter)
  
  all_original_fcasts <- fcasts %>%
    filter(Protection == "Original")
  
  full_feature_diffs <- tibble()
  
  # import original feature values for differencing with protected feature values
  orig_feature_values <- read_csv(paste0("../../Data/Features/", data_folder, original_feature_file_name))
  
  # import protected features and assign new variable values for linking to errors
  for (ff in protected_feature_file_names){
    
    features <- read_csv(paste0("../../Data/Features/M3/", ff))
    
    feature_diffs <- features - orig_feature_values
    
    params <- strsplit(ff, split="_")[[1]]
    
    feature_diffs["Protection"] <- params[2]
    feature_diffs["Parameter"] <- params[3]
    
    full_feature_diffs <- bind_rows(full_feature_diffs, feature_diffs)
    
  }
  
  all_fcast_diffs <- list()
  
  for (i in seq_along(all_protected_fcasts)){
    temp <- all_protected_fcasts[[i]] %>% distinct(Model, Data)
    m <- temp %>% pull(Model)
    d <- temp %>% pull(Data)
    of <- all_original_fcasts %>%
      filter(Model==m, Data==d) %>%
      pull(Forecasts)
    all_fcast_diffs[[i]] <- all_protected_fcasts[[i]] %>%
      mutate(fcast_diff = Forecasts - of)
  }
  
  ## join features to differences in forecasts
  
  full_diff_data <- list()
  
  for (i in seq_along(all_fcast_diffs)){
    pps <- all_fcast_diffs[[i]] %>% distinct(Protection, Parameter)
    prot <- pps %>% pull(Protection)
    param <- pps %>% pull(Parameter)
    feat_df <- full_feature_diffs %>%
      filter(Protection==prot, Parameter==param) %>%
      select(-Protection, -Parameter)
    new <- all_fcast_diffs[[i]] %>%
      bind_cols(feat_df)
    full_diff_data[[i]] <- new
    print(i)
  }
  
  full_diff_data <- do.call(rbind, full_diff_data)
  
  full_diff_data <- full_diff_data %>%
    group_split(Model)
  
  models <- unlist(lapply(full_diff_data, function(x) distinct(x, Model)))
  
  full_diff_data <- lapply(full_diff_data, function(x) x %>% select(-Model, -Protection, -Parameter, -Data, -Forecasts))
  
  full_diff_data <- lapply(full_diff_data, function(x) as.data.frame(scale(x)))
  
  feature_stop <- Sys.time()
  
  #########################################
  
  ############# ############# #############
  
  ## Perform feature selection
  
  fsr <- feature_selection(scaled_feature_data=full_data_scaled, scaled_diff_data=full_diff_data, num_rfe_iters=num_iter)
  
  ## save RReliefF feature rankings (across forecasting models)
  
  print("Feature selection done.")
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RReliefF Rankings/", data_folder))){
    
    write.csv(fsr[["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/", data_folder, "RReliefF_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RReliefF Rankings/", data_folder)))
    
    # specifying the working directory
    write.csv(fsr[["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/", data_folder, "RReliefF_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  ## save RFE feature rankings
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RFE Rankings/", data_folder))){
    
    write.csv(fsr[["rank_df"]], file=paste0("../../Outputs/RFE Rankings/", data_folder, "RFE_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RFE Rankings/", data_folder)))
    
    # specifying the working directory
    write.csv(fsr[["rank_df"]], file=paste0("../../Outputs/RFE Rankings/", data_folder, "RFE_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  ## save RFE oob results
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RFE OOB/", data_folder))){
    
    write.csv(fsr[["combined_oob"]], file=paste0("../../Outputs/RFE OOB/", data_folder, "RFE_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RFE OOB/", data_folder)))
    
    # specifying the working directory
    write.csv(fsr[["combined_oob"]], file=paste0("../../Outputs/RFE OOB/", data_folder, "RFE_diffs_", prefix, "_h1_train.csv"), row.names=FALSE)
    
  }
  
  print("File saving done.")
  
  sf <- fsr[["selected_features"]]
  
  # reassign selected features
  sft <- sf
  
  ### use a window length = 2x + 1 the sp when sp > 1
  ### otherwise use 9, which is the same length as
  ### the shortest window with a seasonal period (quarterly)
  
  # determine sp
  sp <- ifelse(grepl("Monthly", f), 12, ifelse(grepl("Quarterly", f), 4, 1))
  
  # minimum window length of 11 so that x_acf10 can be calculated
  window_length <- max(c(2*sp + 1, 11))

  if (sp == 1) {
    sft <- sf[!sf %in% c("seasonal_strength", "peak", "trough", "seas_acf1", "seas_pacf")]
  }
  
  swap_times <- c()
  
  for (j in c(3)){
    
    print("Starting swapping.")
    
    swap_start <- Sys.time()

    X_knts <- perform_knts(ts_file=f,
                           ts_file_path=fp,
                           seasonal_period=sp,
                           window_length=window_length,
                           k=j,
                           features_to_calculate=fv,
                           selected_features=sft)
    
    swap_stop <- Sys.time()
    
    swap_times <- c(swap_times, difftime(swap_stop, swap_start, units="mins"))
    
    write.csv(X_knts, file=paste0(fp, "k-nts-plus-diffs_", j, "_", f), row.names=FALSE)
    
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
    
  }
  
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
  
}

# write.csv(computation_time, file="../../Data/Computation Results/k-nts-plus.csv", row.names=FALSE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
