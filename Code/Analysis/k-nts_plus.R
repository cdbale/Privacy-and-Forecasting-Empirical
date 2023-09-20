### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tidyverse)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(ranger)
library(tidytext)
library(CORElearn)

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

# steps:

# - track computation time (each part - RReliefF, RFE, swapping) - need
# new file to track computation time for each of the original (unprotected)
# files.

# - import original data
# - import baseline protected versions of data
# - import corresponding features
# write a function to do everything and save the results for one 
# original file at a time
# Save:
# - RReliefF feature rankings
# - Random Forest feature rankings
# - which features were selected for each data set
# - computation time for each part

## NEED TO RE-Read in X with h1 when doing protection

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

# read in original time series
X <- import_data(file_name="yearly-MACRO_h1_train.csv", file_path=fp, sp=1)

# file with errors for all models on original data
eds <- read_csv(paste0(ed_file_path, "yearly-MACRO_all_distributions_h2.csv"))

# transform to two columns - model name and errors
eds <- eds %>% gather(key="name", value="values") %>%
  mutate(name = substring(name, 1, nchar(name)-8)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_") %>%
  mutate(Protection = if_else(is.na(Protection), "Original", ifelse(Protection == "yearly-MACRO", "Original", Protection)),
         Parameter = if_else(is.na(Parameter), "Original", Parameter)) %>%
  select(-Data)

### now import corresponding time series features and link to forecast errors

feature_file_names <- grep("h2_train", list.files("../../Data/Features/"), value=TRUE)
feature_file_names <- grep("yearly-MACRO", feature_file_names, value=TRUE)

full_features <- tibble()

# import protected features and assign new variable values for linking to errors
for (f in feature_file_names){
  
  features <- read_csv(paste0("../../Data/Features/", f))
  
  params <- strsplit(f, split="_")[[1]]
  
  print(params)
  
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

#########################################

############# ############# #############

############# Initial feature filtering using Relief #############

############ experimenting with choosing the value of k for RReliefF ############

evals <- lapply(full_data_scaled, function(x) attrEval("values", data=x, estimator="RReliefFexpRank"))

evals_combined <- lapply(1:length(evals), function(x) as_tibble(evals[[x]], rownames="feature") %>% mutate(model = models[x]))

evals_combined <- do.call(rbind, evals_combined)

# relief_plotter <- function(weights_data, model_name){
#   plt <- weights_data %>%
#     filter(model==model_name) %>%
#     arrange(value) %>%
#     mutate(feature=factor(feature, levels=feature)) %>%
#     ggplot(aes(x=feature, y=value)) +
#     geom_col() +
#     coord_flip()
#   
#   return(plt)
# }
# 
# relief_plotter(evals_combined, "DES")

avg_evals <- evals_combined %>%
  group_by(feature) %>%
  summarize(avg_weight=mean(value))

avg_evals <- avg_evals %>%
  arrange(avg_weight)

sorted_names <- avg_evals$feature

avg_evals %>%
  mutate(feature=factor(feature, 
                        levels=sorted_names)) %>%
  ggplot(aes(x=feature, y=avg_weight)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Weight")

###################################################
###################################################
###################################################

relief_selection <- lapply(evals, function(x) names(x[x > 0]))

# number of RFE iterations
num_iter <- 25

# setting seed
set.seed(42)

# list for oob errors
oob_list <- list()

# list for variable rankings
rank_list <- list()

for (i in seq_along(full_data_scaled)){
  
  df <- full_data_scaled[[i]]
  
  # list for oob errors
  oob_list[[i]] <- list()
  
  # list for variable rankings
  rank_list[[i]] <- list()
  
  for (j in 1:num_iter){
    
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
      
      oob_errors <- c(oob_errors, rf_res$prediction.error)
      
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

# new plot for revision

combined_oob %>%
  ggplot(aes(x=num_features, y=value, color=model, shape=model)) +
  geom_line(size=0.75) +
  geom_point(size=3) +
  labs(x="Number of Features (Subset Size)",
       y="Out-of-Bag MSE",
       color="Model",
       shape="Model") +
  theme(text = element_text(size = 15))

# combined_oob %>%
#   ggplot(aes(x=num_features, y=value)) +
#   geom_line(size=0.6) +
#   facet_wrap(~model) +
#   labs(x="Number of Features (Subset Size)",
#        y="OOB MSE")

## --------------------------------------------------------- ##

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

tops <- rank_df %>%
  group_by(model, var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(model, avg_rank) %>%
  slice(1:ns) %>%
  group_split()

top_feats <- lapply(tops, function(x) x$var)

final_importances <- list()

for (i in seq_along(full_data_scaled)){
  
  df <- full_data_scaled[[i]][,c("values", top_feats[[i]])]
  
  rf_res <- ranger(values ~ ., data=df, importance="permutation", num.trees=500)
  
  final_importances[[i]] <- importance(rf_res)
}

final_importances <- do.call(rbind, lapply(1:length(final_importances), function(x) tibble("var"=names(final_importances[[x]]), "imp"=final_importances[[x]], "model"=models[x])))

#####################################################

final_importances %>%
  ggplot(aes(x=var, y=imp)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~model, scales='free') +
  scale_x_reordered() +
  labs(x="Feature Name",
       y="Increase in MSE")

#######################################

sf

#####################################################

## under k-nTS plus, the data provider generates forecasts
## for period t, and uses random forest to measure feature
## importance for predicting the forecast accuracy of period 
## t. The most important features are used in k-nts plus, in
## hopes of minimizing the impact of data protection on forecast
## accuracy in period t+1.

# vector of feature names to calculate
fv <- c("entropy", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

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
  
  return(X_new)
  
}

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

for (f in file_names[1:2]){
  
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
    
    write.csv(X_knts, file=paste0(fp, "k-nts-plus_", j, "_", f))
    
  }
}
