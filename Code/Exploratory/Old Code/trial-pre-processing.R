# test out pre-processing steps

library(plyr)
library(tidyverse)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(ranger)
library(tidytext)
library(CORElearn)
library(stats)
library(forecast)

# steps:

# - track computation time (each part - RReliefF, RFE, swapping)
# - import original data
# - import baseline protected versions of data
# - import corresponding features
# write a function to do everything and save the results for one 
# original file at a time
# Save:
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
  
  return(ts_data)
}

sp=12

X <- import_data(file_name="monthly-MICRO_h1_train.csv", file_path="../../Data/Cleaned/", sp=sp)

# note that we won't need to consider any seasonal features for swapping

# and these can be removed from the RFE as well

# for each series,

# perform a seasonality test

is_seasonal <- function(time_series, sp){
  
  if ((sp > 1) & (length(time_series) >= 3*sp)){
    
    acfs <- acf(time_series, lag.max=sp, plot=FALSE, type="correlation")[['acf']][,,1]
    
    test_result <- abs(acfs[sp+1]) > (1.645*sqrt((1+2*sum(acfs[1:sp]^2))/length(time_series)))
  } else {
    test_result <- FALSE
  }
  return(test_result)
}

is_seasonal(X[[300]], sp)

# if seasonal,

# apply box-cox transformation

lmda <- BoxCox.lambda(X[[300]], method='guerrero')

transformed <- BoxCox(X[[300]], lambda=lmda)

# STL decomposition

decomposed <- stl(transformed, s.window="periodic")[['time.series']]

# inverse box-cox on trend + remainder component 
sadj <- InvBoxCox(decomposed[,2] + decomposed[,3], lambda=lmda)

# keep seasonal component to add back in at the end

# for now, arbitrarily choose values

# 0.10 for monthly,
# 0.25 for yearly/quarterly

# smooth the seasonally adjusted series using Loess
# It looks like we need about 4 points per degree in
# the model fit. The default degree is 2. So we need
# a span of alpha = (2*4)/n

alpha=0.10

treg <- 1:length(sadj)
smoothed_model <- loess(sadj ~ treg, span=alpha)
smoothed_sadj <- ts(predict(smoothed_model), frequency=sp)

plot(sadj)
lines(smoothed_sadj)

# scale the target and reference series by the forecast
# origin. Here's the two problems: Scaling each series by its own
# forecast origin means we would only have 1s to swap with, which
# would mean we always preserve the final confidential point of
# each series, or all the confidential points if we scaled by the
# 'origin' of each window. Or, if we scaled the reference series by 
# the origin of the target series, the relative scales would be
# preserved and the values could still be off once the target series
# was re-scaled.

# We're going to scale each series by the point immediately prior to
# the forecast origin.

scaler <- smoothed_sadj[length(smoothed_sadj)-1]

smoothed_sadj <- smoothed_sadj/scaler

plot(smoothed_sadj)












