# Assess the noise/accuracy curve of a data set that performs poorly under
# k-nts+

# Author: Cameron Bale

library(forecast)
library(tsfeatures)
library(tidyverse)
library(tsne)
library(e1071)
library(ranger)
library(CORElearn)
# library(proxy)
source('custom_feature_functions.R')

# function to calculate time series features
feature_calculator <- function(ts, features_to_calculate, sp){
  
  temp <- tsfeatures(ts, features=features_to_calculate, scale=FALSE) %>%
    select(-nperiods, -seasonal_period)
  
  return(temp)
}

# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# import the M4 time series data
# there are 759 time series with a length of 104, and the percent increase
# in MAE was over 1000

sp <- 12

# import the M4 monthly data
m4 <- read.csv("../../Data/Cleaned/M4/Monthly_h1_train.csv")

# import data and convert to a list of series
m4 <- as.list(as.data.frame(t(m4)))

# remove NA values from the end of each series
m4 <- lapply(m4, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
m4 <- lapply(m4, function(x) ts(x, frequency=sp))

# keep only the series of length 104
lengths <- sapply(m4, length)
X_b <- m4[lengths==104]

# truncate data to strictly positive
X_b <- lapply(X_b, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
X_b <- lapply(X_b, log)

# import the test data and filter for the test data for the series of length 104
m4_test <- read.csv("../../Data/Cleaned/M4/Monthly_h1_test.csv")
X_test_b <- m4_test[lengths==104,]

### now build a for-loop that adds increasing amounts of noise to the time
### series, generates forecasts, and saves the accuracy

# use additive noise for the noise addition since this an intuitive amount
# of noise that we can tie back to the standard deviation of the time series

additive_noise <- function(time_series, s){
  noise_vals <- rnorm(n=length(time_series), sd=s*sd(time_series))
  return(time_series + noise_vals)
}

# define vector of s values for additive noise
svals <- seq(from=0, to=3.0, by=0.05)

# vectors to store mae values for both models
full_ses_mae <- c()
full_des_mae <- c()
avg_spec_entropy <- c()

# loop over s values
for (i in svals){
  
  # add noise
  noisy_X <- lapply(X_b, function(x) additive_noise(x, i))
  
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  full_ses_mae <- c(full_ses_mae, mean(abs(ses_fcasts - X_test_b)))
  full_des_mae <- c(full_des_mae, mean(abs(des_fcasts - X_test_b)))
  
  # calculate the spectral entropy of the time series
  avg_spec_entropy <- c(avg_spec_entropy, mean(sapply(noisy_X, entropy)))
  
}

# combine mae and s values into a tibble
bad_results <- tibble(SES = full_ses_mae,
                      DES = full_des_mae,
                      Avg_Spec_Entropy = avg_spec_entropy,
                      s = svals)

bad_results <- bad_results %>%
  gather(key="Model", value="MAE", -s, -Avg_Spec_Entropy) %>%
  mutate(Set = "Bad Protected Accuracy")

################################################################################

X_g <- m4[lengths==67]

# truncate data to strictly positive
X_g <- lapply(X_g, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
X_g <- lapply(X_g, log)

# import the test data and filter for the test data for the series of length 67
X_test_g <- m4_test[lengths==67,]

### now build a for-loop that adds increasing amounts of noise to the time
### series, generates forecasts, and saves the accuracy

# use additive noise for the noise addition since this an intuitive amount
# of noise that we can tie back to the standard deviation of the time series
# vectors to store mae values for both models
full_ses_mae <- c()
full_des_mae <- c()
avg_spec_entropy <- c()

# loop over s values
for (i in svals){
  
  # add noise
  noisy_X <- lapply(X_g, function(x) additive_noise(x, i))
  
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  full_ses_mae <- c(full_ses_mae, mean(abs(ses_fcasts - X_test_g)))
  full_des_mae <- c(full_des_mae, mean(abs(des_fcasts - X_test_g)))
  
  # calculate the spectral entropy of the time series
  avg_spec_entropy <- c(avg_spec_entropy, mean(sapply(noisy_X, entropy)))
  
}

# combine mae and s values into a tibble
good_results <- tibble(SES = full_ses_mae,
                       DES = full_des_mae,
                       Avg_Spec_Entropy = avg_spec_entropy,
                       s = svals)

good_results <- good_results %>%
  gather(key="Model", value="MAE", -s, -Avg_Spec_Entropy) %>%
  mutate(Set="Good Protected Accuracy")

all_results <- good_results %>% bind_rows(bad_results)

all_results %>%
  ggplot(aes(x=s, y=MAE, color=Model)) +
  geom_line() +
  facet_wrap(~Set) +
  labs(x = "Additive Noise Parameter: s",
       title = "Mean Absolute Error Across Protected Series")

# now plot the time series themselves
X_g_df <- bind_cols(X_g) %>%
  mutate(Time = 1:n(),
         Set = "Good Protected Accuracy") %>%
  gather(key="Series", value="Value", -Time, -Set)

X_b_df <- bind_cols(X_b) %>%
  mutate(Time = 1:n(),
         Set = "Bad Protected Accuracy") %>%
  gather(key="Series", value="Value", -Time, -Set)

X_df <- X_g_df %>% bind_rows(X_b_df)

X_df %>%
  ggplot(aes(x=Time, y=Value, color=Series)) +
  geom_line() +
  facet_wrap(~Set, scales='free') +
  ylim(4, 11) +
  theme(legend.position='none') +
  labs(title="Original Time Series")

all_results %>%
  distinct(s, Avg_Spec_Entropy, Set) %>%
  ggplot(aes(x=s, y=Avg_Spec_Entropy)) +
  geom_line() +
  facet_wrap(~Set) +
  labs(x="Additive Noise Parameter: s",
       y="Average Spectral Entropy")

################################################################################
################################################################################
################################################################################
################################################################################

# we know the following features were selected by the machine learning-based
# feature selection when all M4 monthly time series were included together

selected_features <- list()

relief_rankings <- read_csv("../../Outputs/RReliefF Rankings/M4/RReliefF_Monthly_h1_train.csv")
rfe_oob <- read_csv("../../Outputs/RFE OOB/M4/RFE_Monthly_h1_train.csv")
rfe_rankings <- read_csv("../../Outputs/RFE Rankings/M4/RFE_Monthly_h1_train.csv")
  
# calculate how many features needed to have within 5% of minimum prediction
# error
nf <- rfe_oob %>%
  filter(model %in% c("SES", "DES")) %>%
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
  
sf <- rfe_rankings %>%
  filter(model %in% c("SES", "DES")) %>%
  group_by(var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank) %>%
  slice(1:nf) %>%
  pull(var)

sf_weights <- rfe_rankings %>%
  filter(model %in% c("SES", "DES")) %>%
  group_by(var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank) %>%
  slice(1:nf) %>%
  pull(avg_rank)

sf

sf_weights <- 1/sf_weights / (sum(1/sf_weights))

# selected variance, spike, mean, max level shift, x_acf1, max var shift, hurst

## plot the feature distributions
orig_features_b <- feature_calculator(ts=X_b, features_to_calculate=fv, sp=12)
orig_features_g <- feature_calculator(ts=X_g, features_to_calculate=fv, sp=12)

# import k-nTS+ data
knts_m4 <- read_csv("../../Data/Cleaned/M4/k-nts-plus-M4_3_Monthly_h1_train.csv")

# import data and convert to a list of series
knts_m4 <- as.list(as.data.frame(t(knts_m4)))

# remove NA values from the end of each series
knts_m4 <- lapply(knts_m4, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
knts_m4 <- lapply(knts_m4, function(x) ts(x, frequency=sp))

# keep only the series of length 104
lengths <- sapply(knts_m4, length)
knts_b <- knts_m4[lengths==104]

# truncate data to strictly positive
knts_b <- lapply(knts_b, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
knts_b <- lapply(knts_b, log)

## repeat for knts series with good increase in forecast error

knts_g <- knts_m4[lengths==67]

# truncate data to strictly positive
knts_g <- lapply(knts_g, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
knts_g <- lapply(knts_g, log)

### extract features from knts_g and knts_b

knts_features_b <- feature_calculator(ts=knts_b, features_to_calculate=fv, sp=12)
knts_features_g <- feature_calculator(ts=knts_g, features_to_calculate=fv, sp=12)

## plot feature distributions like in the paper

knts_features_b %>%
  bind_rows(orig_features_b) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_b))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')
  
###### repeat for series with good increase in forecast error

knts_features_g %>%
  bind_rows(orig_features_g) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_g))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')

################################################################################
################################################################################

##### perform swapping using the intersection of the nearest neighbors
##### based on individual features

knts_alg_individual_features <- function(time_series, feature_weights, sp, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- matrix(NA, nrow=num_periods, ncol=num_series)
  
  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
  
  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  if ("x_acf1" %in% rownames(C)){
    C["x_acf1",][is.na(C["x_acf1",])] <- 0.0 
  }
  
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  
  distance_vectors <- list()
  # calculate a feature distance vector for each feature
  for(i in 1:nrow(C)){
    cf <- matrix(C[i,], nrow=1)
    distance_vectors[[i]] <- ones_column %*% diag(t(cf)%*%cf) - 2*t(cf)%*%cf + diag(t(cf)%*%cf) %*% t(ones_column)
  }
  
  # for each time period in the initial window
  for (j in 1:num_series){
    
    # loop over the distance matrices, sorting the corresponding columns for
    # the jth series and finding the nearest neighbors based on each feature
    set_size <- 15
    
    all_K <- list()
    
    for (dv in seq_along(distance_vectors)){
      
      # select the jth column
      d <- distance_vectors[[dv]][,j]
      
      # sort the distances in the jth column smallest to largest
      sorted <- sort(d, index.return=TRUE)
      
      # select from index 2 to k+1 since first index corresponds to the series itself
      all_K[[dv]] <- sorted$ix[2:(set_size+1)]
      
    }
    
    all_K_df <- tibble()
    
    for (n in seq_along(all_K)){
      temp <- tibble(series = all_K[[n]], neighbor_importance=(1/(1:set_size))/sum(1:set_size), feature_importance=feature_weights[[n]], overall_importance=neighbor_importance*feature_importance)
      all_K_df <- bind_rows(all_K_df, temp)
    }
    
    selected_neighbors <- all_K_df %>%
      group_by(series) %>%
      summarize(overall_importance = sum(overall_importance), .groups='drop') %>%
      arrange(desc(overall_importance)) %>%
      slice(1:k) %>%
      pull(series)
    
    # for each series
    for (t in 1:window_length){
      
      # sample an index
      i <- sample(selected_neighbors, size=1)
      
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
    
    distance_vectors <- list()
    # calculate a feature distance vector for each feature
    for(i in 1:nrow(C)){
      cf <- matrix(C[i,], nrow=1)
      distance_vectors[[i]] <- ones_column %*% diag(t(cf)%*%cf) - 2*t(cf)%*%cf + diag(t(cf)%*%cf) %*% t(ones_column)
    }
    
    for (j in 1:num_series){
      
      all_K <- list()
      
      for (dv in seq_along(distance_vectors)){
        
        # select the jth column
        d <- distance_vectors[[dv]][,j]
        
        # sort the distances in the jth column smallest to largest
        sorted <- sort(d, index.return=TRUE)
        
        # select from index 2 to k+1 since first index corresponds to the series itself
        all_K[[dv]] <- sorted$ix[2:(set_size+1)]
        
      }
      
      all_K_df <- tibble()
      
      for (n in seq_along(all_K)){
        temp <- tibble(series = all_K[[n]], neighbor_importance=(1/(1:set_size))/sum(1:set_size), feature_importance=feature_weights[[n]], overall_importance=neighbor_importance*feature_importance)
        all_K_df <- bind_rows(all_K_df, temp)
      }
      
      selected_neighbors <- all_K_df %>%
        group_by(series) %>%
        summarize(overall_importance = sum(overall_importance), .groups='drop') %>%
        arrange(desc(overall_importance)) %>%
        slice(1:k) %>%
        pull(series)
      
      # sample an index
      i <- sample(selected_neighbors, size=1)
      
      # replace the value
      X_new[t,j] <- time_series[[i]][t]
      
    }
  }
  
  print("swapping complete.")
  
  return(X_new)
  
}

knts_g <- knts_alg_individual_features(time_series=X_g, feature_weights=sf_weights, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=sf)
knts_b <- knts_alg_individual_features(time_series=X_b, feature_weights=sf_weights, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=sf)

# convert to lists of time series
knts_g <- lapply(as.list(as.data.frame(knts_g)), function(x) ts(x, frequency=sp))
knts_b <- lapply(as.list(as.data.frame(knts_b)), function(x) ts(x, frequency=sp))

# truncate data to strictly positive
knts_g <- lapply(knts_g, function(x) ifelse(x >= 1, x, 1))
knts_b <- lapply(knts_b, function(x) ifelse(x >= 1, x, 1))

################################################################################

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_g, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_g, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_g))
des_errors <- mean(abs(des_fcasts - X_test_g))

# compute percent change
good_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_g <- good_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_g <- mean(ses_errors, des_errors)

(prot_mae_g - orig_mae_g)/orig_mae_g * 100

################################################################################

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_b, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_b, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_b))
des_errors <- mean(abs(des_fcasts - X_test_b))

# compute percent change
bad_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_b <- bad_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_b <- mean(ses_errors, des_errors)

(prot_mae_b - orig_mae_b)/orig_mae_b * 100









# plot these time series on t-SNE axes using these selected features

# # function to calculate the distance to the three nearest neighbors
# neighbor_distances <- function(d_matrix, num_neighbors){
#   distance_vector <- c()
#   for (i in 1:ncol(d_matrix)){
#     d <- d_matrix[,i]
#     sort_d <- sort(d)[2:(num_neighbors+1)]
#     avg_d <- mean(sort_d)
#     distance_vector <- append(distance_vector, avg_d)
#   }
#   return(distance_vector)
# }
# 
# ## Calculate the feature distance matrix D
# D_calc <- function(ts_data){
#   ones_column <- as.matrix(rep(1, ncol(ts_data)), nrow=ncol(ts_data))
#   temp <- ones_column %*% diag(t(ts_data)%*%ts_data) - 2*t(ts_data)%*%ts_data + diag(t(ts_data)%*%ts_data) %*% t(ones_column)
#   return(temp)
# }
# 
# # function to extract selected features from time series, perform dimention
# # reduction using tsne, and calculate the average distance of each time
# # series on the dimension reduced feature space to its 3 nearest neighbors
# feature_tsne <- function(ts_data, features_to_calculate, sf, sp){
#   
#   temp_features <- feature_calculator(ts_data, features_to_calculate, sp)
#   
#   temp_features <- temp_features[,sf]
#   
#   temp_tsne <- tsne(as.matrix(temp_features), whiten=FALSE)
#   
#   tsne_avg_distance <- neighbor_distances(D_calc(t(temp_tsne)), num_neighbors=3)
#   
#   all_results <- tibble("Dim1"=temp_tsne[,1],
#                         "Dim2"=temp_tsne[,2],
#                         "AvgNeighborDistance"=tsne_avg_distance)
#   
#   return(all_results)
# }
# 
# # perform t-sne on the combined data sets
# X_full <- append(X_g, X_b)
# 
# tsne_full <- feature_tsne(ts_data=X_full, features_to_calculate=fv, sf=sf, sp=12) %>%
#   mutate(Set = rep(c("Good Protected Accuracy", "Bad Protected Accuracy"), times=c(length(X_g), length(X_b))))
# 
# tsne_full %>%
#   ggplot(aes(x=Dim1, y=Dim2, color=Set)) +
#   geom_point(alpha=0.6) +
#   labs(x = "t-SNE Dimension 1",
#        y = "t-SNE Dimension 2",
#        title = "Feature Vectors Plotted on the 2-D t-SNE Feature Space")

# # extract the selected features from the good and bad data sets
# tsne_g <- feature_tsne(ts_data=X_g, features_to_calculate=fv, sf=sf, sp=12) %>%
#   mutate(Set = "Good Protected Accuracy")
# tsne_b <- feature_tsne(ts_data=X_b, features_to_calculate=fv, sf=sf, sp=12) %>%
#   mutate(Set = "Bad Protected Accuracy")
# 
# # plot the dimension reduced feature vectors, colored by the average distance
# # to nearest neighbors
# all_tsne <- tsne_g %>% bind_rows(tsne_b)
# 
# all_tsne %>%
#   ggplot(aes(x=Dim1, y=Dim2, color=AvgNeighborDistance)) +
#   geom_point(alpha=0.6) +
#   facet_wrap(~Set)
  
# These time series are clearly very different from each other on the overall
# feature space. Furthermore, we included them all in the RReliefF and RFE 
# feature selection process. There could be a problem with that: RFE tries to
# predict the forecast accuracy using the time series features. However, if
# the series included in that process are very different on their feature values
# AND they have different forecast accuracies, the RFE could be implicitly
# picking the features that predict differences in forecast accuracy due to
# differences in the original series and not differences in features created
# from data protection.

# Thus, we should try to only include time series in the feature selection
# that are homogenous on feature values to begin with! Then, the variation
# in accuracy that is due to differences in features will be arising from privacy
# protection, not differences in features of the original series.

# we may want to consider clustering. For now, let's perform separate
# feature selection processes on each data set

# need a data set with the features from the original and protected versions of time
# series with the corresponding forecast errors

# new vector of additive noise s values
svals <- c(0.25, 0.5, 1.0, 1.5, 2.0)

# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(X_b, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(X_b, function(x) as.vector(holt(x, h=1)$mean)))

# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- abs(ses_fcasts - X_test_b)
des_errors <- abs(des_fcasts - X_test_b)

full_b <- list(bind_cols(orig_features_b, values=ses_errors), bind_cols(orig_features_b, values=des_errors))

for (s in svals){
  # create protected data set
  noisy_X_b <- lapply(X_b, function(x) additive_noise(x, s))
  
  # calculate protected features
  noisy_features_b <- feature_calculator(ts=noisy_X_b, features_to_calculate=fv, sp=12)
  
  # perform forecasting and combine errors with features
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X_b, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X_b, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  ses_errors <- abs(ses_fcasts - X_test_b)
  des_errors <- abs(des_fcasts - X_test_b)
  
  # combine errors and features
  feat_error_ses <- bind_cols(noisy_features_b, values=ses_errors)
  feat_error_des <- bind_cols(noisy_features_b, values=des_errors)
  
  # combine with original features/errors
  full_b[[1]] <- bind_rows(full_b[[1]], feat_error_ses)
  full_b[[2]] <- bind_rows(full_b[[2]], feat_error_des)
  
  print(paste0("sval ", s, " complete."))
}

# so now we have dataframes containing the features and forecast errors
# of one subset of time series and time series model

# scale the feature data

full_b_scaled <- lapply(full_b, function(x) as.data.frame(scale(x)))

models <- c("SES", "DES")

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

fs_b <- feature_selection(scaled_feature_data=full_b_scaled, num_rfe_iters=25)

################################################################################

# now repeat for the series with good accuracy

# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(X_g, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(X_g, function(x) as.vector(holt(x, h=1)$mean)))

# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- abs(ses_fcasts - X_test_g)
des_errors <- abs(des_fcasts - X_test_g)

full_g <- list(bind_cols(orig_features_g, values=ses_errors), bind_cols(orig_features_g, values=des_errors))

for (s in svals){
  # create protected data set
  noisy_X_g <- lapply(X_g, function(x) additive_noise(x, s))
  
  # calculate protected features
  noisy_features_g <- feature_calculator(ts=noisy_X_g, features_to_calculate=fv, sp=12)
  
  # perform forecasting and combine errors with features
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X_g, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X_g, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  ses_errors <- abs(ses_fcasts - X_test_g)
  des_errors <- abs(des_fcasts - X_test_g)
  
  # combine errors and features
  feat_error_ses <- bind_cols(noisy_features_g, values=ses_errors)
  feat_error_des <- bind_cols(noisy_features_g, values=des_errors)
  
  # combine with original features/errors
  full_g[[1]] <- bind_rows(full_g[[1]], feat_error_ses)
  full_g[[2]] <- bind_rows(full_g[[2]], feat_error_des)
  
  print(paste0("sval ", s, " complete."))
}

# so now we have dataframes containing the features and forecast errors
# of one subset of time series and time series model

# scale the feature data

full_g_scaled <- lapply(full_g, function(x) as.data.frame(scale(x)))

fs_g <- feature_selection(scaled_feature_data=full_g_scaled, num_rfe_iters=25)

# Which features were selected for each data set?

# the series with bad protected forecast accuracy require a higher 
# number of features to be able to accurately predict the variation
# in forecast accuracy. 10 features were selected in total:
# "hurst"           "trend"           "spike"           "unitroot_pp"     "x_acf1"  "series_variance"   "max_var_shift"  
# "unitroot_kpss"   "series_mean"     "lumpiness"  
fs_b['selected_features']

# "spike"  "max_var_shift"   "series_mean"    "series_variance" "linearity"   "hurst" 
fs_g['selected_features']

# the series that had poor forecast accuracy to begin with require only 6 features
# to attain sufficient predictive power

# now, perform swapping using the selected features for each data set.

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

knts_g <- knts_alg(time_series=X_g, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=fs_g[['selected_features']])
knts_b <- knts_alg(time_series=X_b, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=fs_b[['selected_features']])

# convert to lists of time series
knts_g <- lapply(as.list(as.data.frame(knts_g)), function(x) ts(x, frequency=sp))
knts_b <- lapply(as.list(as.data.frame(knts_b)), function(x) ts(x, frequency=sp))

# truncate data to strictly positive
knts_g <- lapply(knts_g, function(x) ifelse(x >= 1, x, 1))
knts_b <- lapply(knts_b, function(x) ifelse(x >= 1, x, 1))

################################################################################

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_g, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_g, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_g))
des_errors <- mean(abs(des_fcasts - X_test_g))

# compute percent change
good_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_g <- good_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_g <- mean(ses_errors, des_errors)

(prot_mae_g - orig_mae_g)/orig_mae_g * 100

################################################################################

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_b, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_b, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_b))
des_errors <- mean(abs(des_fcasts - X_test_b))

# compute percent change
bad_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_b <- bad_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_b <- mean(ses_errors, des_errors)

(prot_mae_b - orig_mae_b)/orig_mae_b * 100

## try weighting the distance calculation by the importance of the features.
## this would require standardizing the features first

# calculate feature importances
importances_b <- lapply(full_b_scaled, function(x) ranger(values ~ ., data=x[,c('values', fs_b[['selected_features']])], importance="permutation", num.trees=500))

total_importance_b <- importances_b[[1]]$variable.importance + importances_b[[2]]$variable.importance

weights_b <- total_importance_b/sum(total_importance_b)

weights_b

# define a custom distance function
weighted_dist <- function(x, y, d_weights){
  return(sum(d_weights*((x - y)^2)))
}

# pr_DB$set_entry(FUN = weighted_dist, names = c("weighted_dist"))

# ## re-define the k-nTS+ swapping function using custom distance function
# knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
#   
#   # number of time series
#   num_series <- length(time_series)
#   
#   # number of time periods
#   num_periods <- length(time_series[[1]])
#   
#   # matrix to hold new series
#   X_new <- matrix(NA, nrow=num_periods, ncol=num_series)
#   
#   # restrict the data to the beginning window
#   X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))
#   
#   C <- do.call(cbind, X_window)
#   
#   # C <- do.call(rbind, X_window)
#   
#   ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
#   D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
#   
#   # calculate the features for the current window
#   # C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#   
#   # if ("x_acf1" %in% colnames(C)){
#   #   C[,"x_acf1"][is.na(C[,"x_acf1"])] <- 0.0 
#   # }
#   
#   # scale time series features
#   # C <- as.matrix(scale(C))
#   
#   # dist_weights <- (1:window_length)/sum(1:window_length)
#   
#   ## Calculate the feature distance matrix D
#   # D <- as.matrix(dist(x=C, y=C, method=weighted_dist, dist_weights))
#   
#   # for each time period in the initial window
#   for (j in 1:num_series){
#     
#     # select the jth column
#     d <- D[,j]
#     
#     # sort the distances in the jth column smallest to largest
#     sorted <- sort(d, index.return=TRUE)
#     
#     # select from index 2 to k+1 since first index corresponds to the series itself
#     K <- sorted$ix[2:(k+1)]
#     
#     last_chosen <- -1
#     
#     # for each series
#     for (t in 1:window_length){
#       
#       # X_new[t,j] <- mean(sapply(K, function(x) time_series[[x]][t]))
#       
#       orig_acf <- acf(X_window[[j]][1:t], plot=FALSE)[['acf']]
#       
#       orig_acf[is.na(orig_acf)] <- 0.0
#       
#       if (t > 1){
#         
#         neighbor_acfs <- list()
#         
#         for (neighbor in seq_along(K)){
#           
#           # create vector with existing new series
#           temp_xnew <- X_new[,j][!is.na(X_new[,j])]
#           
#           temp_xnew <- append(temp_xnew, time_series[[K[neighbor]]][t])
#           
#           neighbor_acfs[[neighbor]] <- acf(temp_xnew, plot=FALSE)[['acf']]
#           
#         }
#         
#         for (nacf in seq_along(neighbor_acfs)){
#           neighbor_acfs[[nacf]][is.na(neighbor_acfs[[nacf]])] <- 0.0
#         }
#         
#         acfs_dists <- sapply(neighbor_acfs, function(x) sqrt(sum((x - orig_acf)^2)))
#         
#         to_use <- which.min(acfs_dists)
#         
#         to_use <- ifelse(last_chosen == K[to_use], K[-to_use][which.min(acfs_dists[-to_use])], K[to_use])
#         
#         last_chosen <- to_use
#         
#         X_new[t,j] <- time_series[[to_use]][t]
#         
#       } else {
#         
#         # sample an index
#         i <- sample(K, size=1)
#         
#         last_chosen <- i
#         
#         X_new[t,j] <- time_series[[i]][t]
#         
#       }
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
#     C <- do.call(cbind, X_window)
#     
#     # C <- do.call(rbind, X_window)
#     
#     ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
#     D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
#     
#     # ## calculate the features for the current window
#     # C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
#     # 
#     # if ("x_acf1" %in% colnames(C)){
#     #   C[,"x_acf1"][is.na(C[,"x_acf1"])] <- 0.0 
#     # }
#     # 
#     # # scale time series features
#     # C <- as.matrix(scale(C))
#     # 
#     # ## Calculate the feature distance matrix D
#     # D <- as.matrix(dist(x=C, y=C, method=weighted_dist, dist_weights))
#     
#     for (j in 1:num_series){
#       
#       # select the jth column
#       d <- D[,j]
#       
#       # sort the distances in the jth column smallest to largest
#       sorted <- sort(d, index.return=TRUE)
#       
#       # select from index 2 to k+1 since first index corresponds to the series itself
#       K <- sorted$ix[2:(k+1)]
#       
#       # X_new[t,j] <- mean(sapply(K, function(x) time_series[[x]][t]))
#       
#       orig_acf <- acf(X_window[[j]], plot=FALSE)[['acf']]
#       
#       orig_acf[is.na(orig_acf)] <- 0.0
#         
#       neighbor_acfs <- list()
#         
#       for (neighbor in seq_along(K)){
#           
#         # create vector with existing new series
#         temp_xnew <- X_new[,j][!is.na(X_new[,j])][(t-window_length+1):(t-1)]
#           
#         temp_xnew <- append(temp_xnew, time_series[[K[neighbor]]][t])
#           
#         neighbor_acfs[[neighbor]] <- acf(temp_xnew, plot=FALSE)[['acf']]
#           
#       }
#       
#       for (nacf in seq_along(neighbor_acfs)){
#         neighbor_acfs[[nacf]][is.na(neighbor_acfs[[nacf]])] <- 0.0
#       }
#         
#       acfs_dists <- sapply(neighbor_acfs, function(x) sqrt(sum((x - orig_acf)^2)))
#         
#       to_use <- which.min(acfs_dists)
#         
#       to_use <- ifelse(last_chosen == K[to_use], K[-to_use][which.min(acfs_dists[-to_use])], K[to_use])
#         
#       last_chosen <- to_use
#         
#       X_new[t,j] <- time_series[[to_use]][t]
#       
#       # X_new[t,j] <- mean(sapply(K, function(x) time_series[[x]][t]))
#       
#       # sample an index
#       # i <- sample(K, size=1)
# 
#       # replace the value
# 
#       # X_new[t,j] <- time_series[[i]][t]
#     }
#   }
#   
#   print("swapping complete.")
#   
#   return(X_new)
#   
# }

##### perform swapping using the intersection of the nearest neighbors
##### based on individual features

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



























knts_g <- knts_alg(time_series=X_g, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=fs_g[['selected_features']])

# convert to lists of time series
knts_g <- lapply(as.list(as.data.frame(knts_g)), function(x) ts(x, frequency=sp))

# truncate data to strictly positive
knts_g <- lapply(knts_g, function(x) ifelse(x >= 1, x, 1))

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_g, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_g, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_g))
des_errors <- mean(abs(des_fcasts - X_test_g))

# compute percent change
good_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_g <- good_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_g <- mean(ses_errors, des_errors)

(prot_mae_g - orig_mae_g)/orig_mae_g * 100

## look at feature distributions

knts_features_g <- feature_calculator(ts=knts_g, features_to_calculate=fv, sp=12)

## plot feature distributions like in the paper

knts_features_g %>%
  bind_rows(orig_features_g) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_g))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')

################################################################################
################################################################################

##### perform swapping using weighted euclidean distance on standardized
##### features

knts_b <- knts_alg(time_series=X_b, sp=sp, window_length=25, k=3, features_to_calculate=fv, selected_features=fs_b[['selected_features']])

# convert to lists of time series
knts_b <- lapply(as.list(as.data.frame(knts_b)), function(x) ts(x, frequency=sp))

# truncate data to strictly positive
knts_b <- lapply(knts_b, function(x) ifelse(x >= 1, x, 1))

## now perform forecasting for the k-nts+ series
# now perform forecasting using SES and DES
ses_fcasts <- unname(sapply(knts_b, function(x) as.vector(ses(x, h=1)$mean)))
des_fcasts <- unname(sapply(knts_b, function(x) as.vector(holt(x, h=1)$mean)))
# reverse the log
ses_fcasts <- exp(ses_fcasts)
des_fcasts <- exp(des_fcasts)

# compute forecast error (MAE)
ses_errors <- mean(abs(ses_fcasts - X_test_b))
des_errors <- mean(abs(des_fcasts - X_test_b))

# compute percent change
bad_results %>% 
  filter(s == 0) %>% 
  mutate(knts_mae = c(ses_errors, des_errors),
         pct_change = (knts_mae-MAE)/MAE * 100)

# overall pct change
orig_mae_b <- bad_results %>% 
  filter(s == 0) %>%
  pull(MAE) %>%
  mean()

prot_mae_b <- mean(ses_errors, des_errors)

(prot_mae_b - orig_mae_b)/orig_mae_b * 100

## look at feature distributions

knts_features_b <- feature_calculator(ts=knts_b, features_to_calculate=fv, sp=12)

## plot feature distributions like in the paper

knts_features_b %>%
  bind_rows(orig_features_b) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_b))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')





knts_features_b <- feature_calculator(ts=knts_b, features_to_calculate=fv, sp=12)


knts_features_b %>%
  bind_rows(orig_features_b) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_b))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')







### Let's actually try looking at some of the neighbors that are selected


t <- 104

# number of time series
num_series <- length(X_b)

# number of time periods
num_periods <- length(X_b[[1]])

# matrix to hold new series
X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)

# restrict the data to the beginning window
X_window <- lapply(X_b, function(x) ts(x[(t-window_length+1):t], frequency=sp))

# calculate the features for the current window
C <- tsfeatures(X_window, features=fv, scale=FALSE)[,fs_b[['selected_features']]]

if ("x_acf1" %in% colnames(C)){
  C[,"x_acf1"][is.na(C[,"x_acf1"])] <- 0.0 
}

# scale time series features
C <- as.matrix(scale(C))

## Calculate the feature distance matrix D
D <- as.matrix(dist(x=C, y=C, method=weighted_dist, weights_b))

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
    if (rbinom(1, 1, 0.5) == 1){
      X_new[t,j] <- time_series[[i]][t]
    } else {
      X_new[t,j] <- time_series[[j]][t]
    }
    
  }
}





tsfeatures(X_b[[1]], features=c("acf_features"))

s1 <- X_b[[1]]
s2 <- X_b[[1]][2:length(s1)]
s1 <- s1[1:(length(s1)-1)]
cov(s2,s1)/var(X_b[[1]])




# we are replacing values in time t
# we want the value in time t to be as similar as possible to the original
# value in time t, but these are chosen randomly to preserve some privacy.
# There is some autocorrelation function in the original series. We want to
# choose a point to place in time t that is close to the original point and
# preserves the same auto-correlation with the previously swapped points
# that was present in the original data.

# so for swapping at time t, we obtain a set of k-nearest neighbors on
# the similarity of points. We can compute the autocovariance parameters
# of the original series, denoted c. We can then perform an exhaustive
# search amongst the k-nearest neighbors to find the autocovariance of the
# potentially swapped points with the previously swapped points. We want to
# minimize the difference between autocovariance coefficients. Choose the point
# that minimizes the difference in autocovariance function, and put in a requirement
# that the chosen series cannot have been chosen in the previous time period.












