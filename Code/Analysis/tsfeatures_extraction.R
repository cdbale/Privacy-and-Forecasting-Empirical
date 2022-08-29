##### Code to extract time series features for predicting whether a privacy adjustment improved accuracy.

## Author: Cameron Bale

library(tidyverse)
library(tsfeatures)
library(ggplot2)
library(forecast)

# read in the data
original_data <- read.csv("../../Data/Train/Clean/m3_monthly_micro_h1.csv")

# convert to a list of series
ts_data <- as.list(as.data.frame(t(original_data)))

# remove NA values from each series
ts_data <- lapply(ts_data, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
ts_data <- lapply(ts_data, function(x) ts(x, frequency=12))

# take the log of the data
ts_data <- lapply(ts_data, log)

# define a function to calculate the coefficient of variation
cv <- function(series){
  return(sd(series)/mean(series))
}

# simple outlier detection function
# one for when an outlier occurs at forecast origin, 
# within 5, and within 10 time periods

# for top coding, we want to see if it corrects positive outliers
# tsoutliers will find the outliers for us
# we can determine if they are positive based on whether they are greater than the mean

positive_outlier_detection <- function(series){
  outliers <- tsoutliers(series)$index
  pos_outliers <- s[outliers] > mean(s)
  outliers <- outliers[pos_outliers]
  within_1 <- 1 * (length(series) %in% outliers)
  within_5 <- 1 * any((outliers > (length(series)-5)))
  within_10 <- 1 * any((outliers > (length(series)-10)))
  features <- c(within_1, within_5, within_10)
  names(features) <- c("Outlier_within_1", "Outlier_within_5", "Outlier_within_10")
  return(features)
}

# s <- ts_data[[474]]
# 
# outliers <- tsoutliers(s)$index
# 
# pos_outliers <- s[outliers] > mean(s)
# 
# outliers <- outliers[pos_outliers]
# 
# 1 * any((outliers > (length(s)-5)))
# 
# lapply(ts_data, positive_outlier_detection)

# calculate desired features
ts_features <- tsfeatures(ts_data, features=c("cv", "positive_outlier_detection", 
                                              "entropy", "stl_features", "acf_features",
                                              "nonlinearity"),
                          scale=FALSE)

write.csv(ts_features, file="../../Data/Train/Clean/tsfeatures_h1_top_outliers.csv", row.names=FALSE)
