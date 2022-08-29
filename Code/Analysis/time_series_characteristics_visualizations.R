### Code to visualize the time series characteristics before and after data protection.

## Author: Cameron Bale

library(tidyverse)
library(tsfeatures)
library(ggplot2)
library(ggpubr)
library(gridExtra)
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

