### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tidyverse)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(randomForest)

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

series_mean <- function(x){
  return(mean(x))
}

series_variance <- function(x){
  return(var(x))
}

#####################################################
#####################################################

## under k-nTS plus, the data provider generates forecasts
## for period t, and uses random forest to measure feature
## importance for predicting the forecast accuracy of period 
## t. The most important features are used in k-nts plus, in
## hopes of minimizing the impact of data protection on forecast
## accuracy in period t+1.

# path to files with error distributions
ed_file_path <- "../../Outputs/Results/Error_Distributions/"

# file with errors for all models on original data
eds <- read_csv(paste0(ed_file_path, "all_distributions_h2.csv"))

# transform to two columns - model name and errors
eds <- eds %>% gather(key="name", value="values") %>%
  mutate(name = gsub("Multivariate_LGBM", "LGBM", name),
         name = substring(name, 1, nchar(name)-4)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter"), sep="_") %>%
  mutate(Parameter = if_else(is.na(Parameter), "Original", Parameter))

### Perform feature extraction for all datasets for h2.

file_names <- grep("protected", list.files("../../Data/Train/Clean/"), value=TRUE)

file_names <- grep("h2", file_names, value=TRUE)

extract_features <- function(time_series, sp, feature_vector){
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(time_series)))
  
  # remove NA values from each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  # truncate data to strictly positive
  ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  
  # take the log of the data
  ts_data <- lapply(ts_data, log)
  
  # calculate time series features
  features <- tsfeatures(ts_data, features=feature_vector, scale=FALSE)
  
  return(features)
}

fv <- c("entropy", "lumpiness", "stability",
        "max_level_shift", "max_var_shift", "max_kl_shift",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

for (f in file_names){
  data_set <- read.csv(paste0("../../Data/Train/Clean/", f))
  features <- extract_features(data_set, sp=12, feature_vector=fv) 
  features <- features %>%
    select(-nperiods, -seasonal_period)
  write.csv(features, file=paste0("../../Data/Train/Clean/tsfeatures/", sub(".*micro_", "", f)), row.names=FALSE)
}

# extract the features for the original data
orig_features <- tsfeatures(X, features=fv, scale=FALSE) %>%
  select(-nperiods, -seasonal_period)
orig_features["Horizon"] <- "h2"
orig_features["Protection"] <- "original"
orig_features["Parameter"] <- "Original"

file_names <- grep("tsfeatures", list.files("../../Data/Train/Clean/tsfeatures/"), value=TRUE, invert=TRUE)
file_names <- grep("h2_", file_names, value=TRUE)

full_features <- orig_features

# import protected features and assign new variable values for linking to errors
for (f in file_names){
  
  features <- read_csv(paste0("../../Data/Train/Clean/tsfeatures/", f))
  
  params <- strsplit(f, split="_")

  features["Horizon"] <- params[[1]][1]
  features["Protection"] <- params[[1]][2]
  features["Parameter"] <- strsplit(params[[1]][3], split=".csv")
  
  full_features <- bind_rows(full_features, features)
  
}

## join features to errors
split_eds <- eds %>% 
  group_by(Model, Protection, Parameter) %>%
  group_split()

full_data <- list()

of <- orig_features %>%
  select(1:39)

for (i in seq_along(split_eds)){
  pps <- split_eds[[i]] %>% distinct(Protection, Parameter)
  prot <- pps %>% pull(Protection)
  param <- pps %>% pull(Parameter)
  feat_df <- full_features %>%
    filter(Protection==prot, Parameter==param) %>%
    select(1:39)
  new <- split_eds[[i]] %>%
    bind_cols(feat_df)
  full_data[[i]] <- new
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

library(CORElearn)

############ experimenting with choosing the value of k for RReliefF ############

evals <- lapply(full_data_scaled, function(x) attrEval("values", data=x, estimator="RReliefFexpRank"))

evals_combined <- lapply(1:length(evals), function(x) as_tibble(evals[[x]], rownames="feature") %>% mutate(model = models[x]))

evals_combined <- do.call(rbind, evals_combined)

relief_plotter <- function(weights_data, model_name){
  plt <- weights_data %>%
    filter(model==model_name) %>%
    arrange(value) %>%
    mutate(feature=factor(feature, levels=feature)) %>%
    ggplot(aes(x=feature, y=value)) +
    geom_col() +
    coord_flip()
  
  return(plt)
}

relief_plotter(evals_combined, "DES")

avg_evals <- evals_combined %>%
  group_by(feature) %>%
  summarize(avg_weight=mean(value))

avg_evals %>%
  arrange(avg_weight) %>%
  mutate(feature=factor(feature, 
                        levels=c('unitroot_kpss',
                                 'hurst',
                                 'entropy',
                                 'skewness',
                                 'lumpiness',
                                 'stability',
                                 'kurtosis',
                                 'crossing_points',
                                 'x_acf10',
                                 'e_acf1',
                                 'unitroot_pp',
                                 'max_kl_shift',
                                 'flat_spots',
                                 'time_level_shift',
                                 'x_pacf5',
                                 'time_kl_shift',
                                 'diff1_acf1',
                                 'nonlinearity',
                                 'time_var_shift',
                                 'diff1x_pacf5',
                                 'diff2_acf1',
                                 'diff2x_pacf5',
                                 'e_acf10',
                                 'seas_pacf',
                                 'x_acf1',
                                 'trend',
                                 'diff1_acf10',
                                 'seasonal_strength',
                                 'diff2_acf10',
                                 'seas_acf1',
                                 'trough',
                                 'peak',
                                 'series_mean',
                                 'curvature',
                                 'max_var_shift',
                                 'max_level_shift',
                                 'linearity',
                                 'series_variance',
                                 'spike'),
                        labels=c("Unitroot KPSS",
                                 "Hurst",
                                 "Spectral Entropy",
                                 "Skewness",
                                 "Lumpiness",
                                 "Stability",
                                 "Kurtosis",
                                 "Crossing Points",
                                 "X ACF10",
                                 "Error ACF",
                                 "Unitroot PP",
                                 "Max KL Shift",
                                 "Flat Spots",
                                 "Time Level Shift",
                                 "X PACF5",
                                 "Time KL Shift",
                                 "First Difference ACF",
                                 "Nonlinearity",
                                 "Time Variance Shift",
                                 "First Difference PACF5",
                                 "Second Difference ACF",
                                 "Second Difference PACF5",
                                 "Error ACF10",
                                 "Seasonal PACF",
                                 "X ACF",
                                 "Trend",
                                 "First Difference ACF10",
                                 "Seasonal Strength",
                                 "Second Difference ACF10",
                                 "Seasonal ACF",
                                 "Trough", 
                                 "Peak",
                                 "Mean", 
                                 "Curvature",
                                 "Max Variance Shift",
                                 "Max Level Shift",
                                 "Linearity",
                                 "Variance",
                                 "Spike"))) %>%
  ggplot(aes(x=feature, y=avg_weight)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Weight")

###################################################
###################################################
###################################################

relief_selection <- lapply(evals, function(x) names(x[x > 0]))

library(ranger)

# number of RFE iterations
num_iter <- 50

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

combined_oob %>%
  ggplot(aes(x=num_features, y=value)) +
  geom_line(size=0.6) +
  facet_wrap(~model) +
  labs(x="Number of Features (Subset Size)",
       y="OOB MSE")

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

top_6s <- rank_df %>%
  group_by(model, var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(model, avg_rank) %>%
  slice(1:6) %>%
  group_split()

top_feats <- lapply(top_6s, function(x) x$var)

final_importances <- list()

for (i in seq_along(full_data_scaled)){
  
  df <- full_data_scaled[[i]][,c("values", top_feats[[i]])]
  
  rf_res <- ranger(values ~ ., data=df, importance="permutation", num.trees=500)
  
  final_importances[[i]] <- importance(rf_res)
}

final_importances <- do.call(rbind, lapply(1:length(final_importances), function(x) tibble("var"=names(final_importances[[x]]), "imp"=final_importances[[x]], "model"=models[x])))

library(tidytext)

#####################################################

final_importances %>%
  mutate(var=factor(var, 
                    levels=c('unitroot_kpss',
                                 'hurst',
                                 'entropy',
                                 'skewness',
                                 'lumpiness',
                                 'stability',
                                 'kurtosis',
                                 'crossing_points',
                                 'x_acf10',
                                 'e_acf1',
                                 'unitroot_pp',
                                 'max_kl_shift',
                                 'flat_spots',
                                 'time_level_shift',
                                 'x_pacf5',
                                 'time_kl_shift',
                                 'diff1_acf1',
                                 'nonlinearity',
                                 'time_var_shift',
                                 'diff1x_pacf5',
                                 'diff2_acf1',
                                 'diff2x_pacf5',
                                 'e_acf10',
                                 'seas_pacf',
                                 'x_acf1',
                                 'trend',
                                 'diff1_acf10',
                                 'seasonal_strength',
                                 'diff2_acf10',
                                 'seas_acf1',
                                 'trough',
                                 'peak',
                                 'series_mean',
                                 'curvature',
                                 'max_var_shift',
                                 'max_level_shift',
                                 'linearity',
                                 'series_variance',
                                 'spike'),
                        labels=c("Unitroot KPSS",
                                 "Hurst",
                                 "Spectral Entropy",
                                 "Skewness",
                                 "Lumpiness",
                                 "Stability",
                                 "Kurtosis",
                                 "Crossing Points",
                                 "X ACF10",
                                 "Error ACF",
                                 "Unitroot PP",
                                 "Max KL Shift",
                                 "Flat Spots",
                                 "Time Level Shift",
                                 "X PACF5",
                                 "Time KL Shift",
                                 "First Difference ACF",
                                 "Nonlinearity",
                                 "Time Variance Shift",
                                 "First Difference PACF5",
                                 "Second Difference ACF",
                                 "Second Difference PACF5",
                                 "Error ACF10",
                                 "Seasonal PACF",
                                 "X ACF",
                                 "Trend",
                                 "First Difference ACF10",
                                 "Seasonal Strength",
                                 "Second Difference ACF10",
                                 "Seasonal ACF",
                                 "Trough", 
                                 "Peak",
                                 "Mean", 
                                 "Curvature",
                                 "Max Variance Shift",
                                 "Max Level Shift",
                                 "Linearity",
                                 "Variance",
                                 "Spike")),
                    var = reorder_within(var, imp, model)) %>%
  ggplot(aes(x=var, y=imp)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~model, scales='free') +
  scale_x_reordered() +
  labs(x="Feature Name",
       y="Increase in MSE")

#######################################

sf

# trend, spike, max_var_shift, series_variance, max_level_shift, series_mean

fv <- c("stl_features", "max_var_shift", "series_variance", "series_mean", "max_level_shift")

# split X into three separate datasets, one for each series length
Xs <- list()
unique_lengths <- unique(sapply(X, length))
lengths <- sapply(X, length)
for (l in seq_along(unique_lengths)){
  ids <- lengths==unique_lengths[l]
  sub_X <- X[ids]
  Xs[[l]] <- sub_X
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

write.csv(X_k3, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_3.csv", row.names=FALSE)
write.csv(X_k5, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_5.csv", row.names=FALSE)
write.csv(X_k7, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_7.csv", row.names=FALSE)
write.csv(X_k10, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_10.csv", row.names=FALSE)
write.csv(X_k15, file="../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_15.csv", row.names=FALSE)
