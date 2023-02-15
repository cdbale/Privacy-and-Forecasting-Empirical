## Calculating performance gap portion of REP metric of protected vs. original data

# Author: Cameron Bale

# uses the REP function in the `REP_20220531 - code.R` file.

# window length
# The RNN and LGBM models use an input window length of 25,
# meaning they can only generate fitted values for period 26 and
# beyond. For comparability, we restrict the representativeness
# calculations for all models and time series to time 26 and
# beyond.

window_length <- 25

# source file for REP function
source('REP_20220531 - code.R')

library(tidyverse)
library(ggplot2)
library(stringr)

# import protected data file names
protected_files <- list.files("../../Data/Train/Clean/")
protected_files <- grep("protected_m3_monthly_micro_h1_", protected_files, value=TRUE)
protected_files <- grep("Top_", protected_files, value=TRUE, invert=TRUE)
protected_files <- grep("Bottom_", protected_files, value=TRUE, invert=TRUE)

fitted_files <- list.files("../../Outputs/Forecasts/fitted_values")
fitted_files <- grep("original", fitted_files, value=TRUE)

#########################
data_prep <- function(ts_file, sp, is_fitted=FALSE){
  
  if (is_fitted){
    ts_data <- read.csv(paste0("../../Outputs/Forecasts/fitted_values/", ts_file))
  } else {
    ts_data <- read.csv(paste0("../../Data/Train/Clean/", ts_file))
  }
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(ts_data)))
  
  # remove NA values from each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  return(ts_data)
}
#########################

# import original data
orig_data <- data_prep(ts_file="m3_monthly_micro_h1.csv", sp=12)

orig_data <- lapply(orig_data, function(x) ts(x[(window_length+1):length(x)], frequency=12))

rep_list <- list()

for (file in protected_files){
  
  protected_data <- data_prep(ts_file=file, sp=12)
  
  protected_data <- lapply(protected_data, function(x) ts(x[(window_length+1):length(x)], frequency=12))
  
  short_file <- str_sub(file, start=31, end=-5)
  
  rep_list[[short_file]] <- sapply(1:length(orig_data), function(x) REP(y=orig_data[[x]], fitted=protected_data[[x]], f=1)$Perf_Gap)
  
}

fitted_rep_list <- list()

for (file in fitted_files){
  
  # remove the beginning input window for consistency across forecasting models
  
  fitted_values <- data_prep(ts_file=file, sp=12, is_fitted=TRUE)
  
  already_truncated <- grepl("RNN", file) | grepl("LGBM", file)
  
  if (already_truncated){
    fitted_values <- fitted_values
  } else {
    fitted_values <- lapply(fitted_values, function(x) ts(x[(window_length+1):length(x)], frequency=12))
  }

  id <- str_sub(file, end=-5)
  
  print(id)
  
  fitted_rep_list[[id]] <- sapply(1:length(orig_data), function(x) REP(y=orig_data[[x]], fitted=fitted_values[[x]], f=1)$Perf_Gap)
  
}

# average across all series for each protected data set
rep_results <- do.call(cbind, rep_list)
fitted_rep_results <- do.call(cbind, fitted_rep_list)

round(apply(rep_results, 2, mean), 2)
round(apply(fitted_rep_results, 2, mean), 2)

mean(round(apply(fitted_rep_results, 2, mean), 2))


