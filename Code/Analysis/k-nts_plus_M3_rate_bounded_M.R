### Perform k-NTS Data Protection

# Author: Cameron Bale

# ------------------------------------------------------------------------- #

library(plyr)
library(tsfeatures)
library(e1071)
library(ggplot2)
library(ranger)
library(tidytext)
library(CORElearn)
library(forecast)
library(tidyverse)

data_folder <- "M3_rate/"

# steps:

# - import k-nTS+ protected data files. Stick with k = 3 for now.
# - pick a threshold, e.g., M = sigma_x
# - for all protected values P_i, check whether |A_i - P_i| <= M
# - if TRUE, then keep protected value.
# - if FALSE, then if A_i - P_i > 0 (meaning A_i > P_i) then set P_i = A_i - M
# - else A_i - P_i < 0 (meaning A_i < P_i) then set P_i = A_i + M

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
  
  return(ts_data)
}

# test on a single k-nTS+ file
data_path <- paste0("../../Data/Cleaned/", data_folder)

# vectors of k-nTS+ and original file names
knts_files <- grep("k-nts-plus_3_", list.files(data_path), value=TRUE)
og_files <- grep("_h1_train", list.files(data_path), value=TRUE)
og_files <- grep("AN_", og_files, value=TRUE, invert=TRUE)
og_files <- grep("DP_", og_files, value=TRUE, invert=TRUE)
og_files <- grep("k-nts", og_files, value=TRUE, invert=TRUE)

# build function to do the trimming for a given time series
knts_bounded <- function(protected_time_series, original_time_series, threshold){
  
  # set threshold
  M <- threshold*sd(original_time_series)
  
  # if the protected value is too small, replace it with A_i - M, otherwise, replace it with A_i + M
  new_protected <- ifelse(original_time_series - protected_time_series > 0, original_time_series - M, original_time_series + M)
  
  # check whether within threshold
  to_keep <- abs(original_time_series - protected_time_series) <= M
  # replace the values we want to keep
  new_protected[to_keep] <- protected_time_series[to_keep]
  
  return(new_protected)
}

# do the trimming for all data sets

threshold_values <- c(0.5, 1, 1.5)

for (f in knts_files){
  
  file_id <- paste0(strsplit(f, "_")[[1]][3:6], collapse="_")
  
  # original file
  og_f <- grep(file_id, og_files, value=TRUE)
  
  # set seasonal period
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # import data sets
  knts_data <- import_data(f, data_path, sp=sp)
  og_data <- import_data(og_f, data_path, sp=sp)
  
  for (th in threshold_values){
    
    new_protected <- list()
    
    for (j in seq_along(og_data)){
      new_protected[[j]] <- knts_bounded(knts_data[[j]], og_data[[j]], th)
    }
    
    ml <- max(sapply(new_protected, length))
    
    for(i in seq_along(new_protected)){
      length(new_protected[[i]]) <- ml
    }
    
    write.csv(do.call(rbind, new_protected), paste0("../../Data/Cleaned/", data_folder, "k-nts-plus-bounded_3-", th, "_", file_id), row.names=FALSE)
  }
}
