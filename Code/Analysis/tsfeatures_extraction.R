##### Code to extract time series features for predicting whether a privacy adjustment improved accuracy.

## Author: Cameron Bale

library(e1071)
library(plotly)
library(GGally)
library(tidyverse)
library(tsfeatures)
library(ggplot2)
library(forecast)

# read in the data
original_data <- read.csv("../../Data/Train/Clean/m3_monthly_micro_h1.csv")

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

series_mean <- function(x){
  return(mean(x))
}

series_variance <- function(x){
  return(var(x))
}

#######################################################

fv <- c("entropy", "stl_features", "skewness", 
        "kurtosis", "hurst", "series_mean", "series_variance",
        "max_var_shift", "max_level_shift", "acf_features", "pacf_features")

orig_features <- extract_features(original_data, sp=12, feature_vector=fv)

plot_features <- orig_features %>%
  select(entropy, hurst, skewness, kurtosis, e_acf1, trend, 
         seasonal_strength, series_mean, series_variance, spike,
         max_var_shift, max_level_shift)

colnames(plot_features) <- c("Spectral Entropy", "Hurst", "Skewness", "Kurtosis", 
                             "EACF", "Trend", "Seasonality", "Mean", "Variance", 
                             "Spike", "Max Variance Shift", "Max Level Shift")

ggpairs(plot_features)

write.csv(orig_features, file="../../Data/Train/Clean/tsfeatures/tsfeatures_h1.csv", row.names=FALSE)

### Perform feature extraction for all datasets.

file_names <- grep("protected", list.files("../../Data/Train/Clean/"), value=TRUE)

for (f in file_names){
  data_set <- read.csv(paste0("../../Data/Train/Clean/", f))
  features <- extract_features(data_set, sp=12, feature_vector=fv)
  write.csv(features, file=paste0("../../Data/Train/Clean/tsfeatures/", sub(".*micro_", "", f)), row.names=FALSE)
}
