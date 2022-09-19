##### Code to extract time series features for predicting whether a privacy adjustment improved accuracy.

## Author: Cameron Bale

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

fv <- c("series_mean", "series_variance", "entropy", 
        "stability", "lumpiness", "max_level_shift",
        "max_var_shift", "max_kl_shift", "crossing_points", 
        "flat_spots", "hurst", "stl_features", "acf_features", "nonlinearity")

orig_features <- extract_features(original_data, sp=12, feature_vector=fv)

write.csv(orig_features, file="../../Data/Train/Clean/tsfeatures/tsfeatures_h1.csv", row.names=FALSE)

### Perform feature extraction for all datasets.

file_names <- grep("protected", list.files("../../Data/Train/Clean/"), value=TRUE)

for (f in file_names){
  data_set <- read.csv(paste0("../../Data/Train/Clean/", f))
  features <- extract_features(data_set, sp=12, feature_vector=fv)
  write.csv(features, file=paste0("../../Data/Train/Clean/tsfeatures/", sub(".*micro_", "", f)), row.names=FALSE)
}







# # define a function to calculate the coefficient of variation
# cv <- function(series){
#   return(sd(series)/mean(series))
# }

# simple outlier detection function
# one for when an outlier occurs at forecast origin, 
# within 5, and within 10 time periods

# for top coding, we want to see if it corrects positive outliers
# tsoutliers will find the outliers for us
# we can determine if they are positive based on whether they are greater than the mean

# positive_outlier_detection <- function(series){
#   outliers <- tsoutliers(series)$index
#   pos_outliers <- s[outliers] > mean(s)
#   outliers <- outliers[pos_outliers]
#   within_1 <- 1 * (length(series) %in% outliers)
#   within_5 <- 1 * any((outliers > (length(series)-5)))
#   within_10 <- 1 * any((outliers > (length(series)-10)))
#   features <- c(within_1, within_5, within_10)
#   names(features) <- c("Outlier_within_1", "Outlier_within_5", "Outlier_within_10")
#   return(features)
# }

# calculate desired features
# ts_features <- tsfeatures(ts_data, features=c("cv", "positive_outlier_detection", 
#                                               "entropy", "stl_features", "acf_features",
#                                               "nonlinearity"),
#                           scale=FALSE)


