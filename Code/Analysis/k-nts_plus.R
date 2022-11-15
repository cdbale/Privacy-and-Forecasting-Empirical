### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

###### CODE NEEDS LINE REMOVING KNTS FROM ERROR DISTRIBUTION ######

library(plyr)
library(tidyverse)
library(tsfeatures)
library(e1071)
library(randomForest)
library(ggplot2)

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
         name = gsub("k_nts", "knts", name),
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

# calculate change in error relative to original data
model_groups <- eds %>%
  group_by(Model) %>%
  group_split()

value_differencer <- function(Y){
  orig_errors <- Y %>%
    filter(Protection == "original") %>%
    pull(values)
  
  new_Y <- Y %>%
    group_by(Protection, Parameter) %>%
    mutate(values_diff = orig_errors - values)
  
  return(new_Y)
}

model_groups <- lapply(model_groups, value_differencer)

eds <- do.call(rbind, model_groups)

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
  feat_changes <- as_tibble(as.matrix(of) - as.matrix(feat_df))
  new <- split_eds[[i]] %>%
    bind_cols(feat_changes)
  full_data[[i]] <- new
}

full_data <- do.call(rbind, full_data)

full_data <- full_data %>%
  filter(Protection != "original") %>%
  select(-Model, -Horizon, -Protection, -Parameter, -values)

full_data <- as.data.frame(scale(full_data))

############# Initial feature filtering using Relief #############

library(CORElearn)

evals <- attrEval("values_diff", data=full_data, estimator="RReliefFexpRank")

as_tibble(evals, rownames="Feature") %>%
  arrange(desc(value)) %>%
  mutate(Feature=factor(Feature, levels=Feature)) %>%
  ggplot(aes(x=Feature, y=value)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Weight",
       title = "Feature Weights from RReliefF")

## seems to be some rough clusters of importances
# - Spike is massively important on its own
# - linearity, series_mean, and max_var_shift are all about equally important ~30%
# - max_var_shift, series_variance, and curvature are roughly equally important
# - peak is kind of on it's own, but still over 15%

# interesting results for kurtosis, skewness, and spectral entropy

# we choose our cutoff at 0%, giving us the following features:

relief_selection <- names(evals[evals >= 0.10])

relief_selection

# "stability"         "max_level_shift"   "time_level_shift"  "max_var_shift"     "time_var_shift"   
# "time_kl_shift"     "hurst"             "unitroot_kpss"     "trend"             "spike"            
# "linearity"         "curvature"         "e_acf1"            "e_acf10"           "seasonal_strength"
# "peak"              "trough"            "x_acf1"            "diff1_acf1"        "diff1_acf10"      
# "diff2_acf1"        "diff2_acf10"       "seas_acf1"         "x_pacf5"           "diff1x_pacf5"     
# "diff2x_pacf5"      "seas_pacf"         "series_mean"       "series_variance"  

#############  #############

rf_data <- full_data[,c("values_diff", relief_selection)]

# set.seed(42)

rf_res <- randomForest(y=rf_data$values_diff, x=rf_data[,-1], importance=TRUE, nperm=5)

# Get variable importance from the model fit
ImpData <- as_tibble(importance(rf_res), rownames="Feature") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(Feature=factor(Feature, levels=Feature))

ImpData %>%
  ggplot(aes(x=Feature, y=`%IncMSE`)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Weight",
       title = "Permutation Based Feature Importance from Random Forest")

sf <- ImpData %>%
  filter(`%IncMSE` > 0) %>%
  pull(Feature) %>%
  as.character()

sf

fv <- c("stl_features", "max_var_shift", "max_level_shift", "series_variance", "series_mean")

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
