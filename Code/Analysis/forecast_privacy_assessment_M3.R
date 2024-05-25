## Assess the privacy of forecasts.

## We are performing a task that is incompatible. We want to have past
## data values be as dissimilar (private) from the unprotected values as
## possible, but have the forecasted value (which is a function of the
## past values) be as similar to the true value as possible. So, good
## forecasts are likely not private (we will find out now if they are)
## or aren't

## assess privacy simulation for VAR SIM forecasts on original data
## and k-nTS+ forecasts on rate data

library(tidyverse)

### helper functions ###

pmf <- function(x){
  return(x/sum(x))
}

########################

# Author: Cameron Bale

data_folder = "M3/"

## we will use the test data and the forecast files

# paths to the data files and feature files
fp <- paste0("../../Data/Cleaned/", data_folder)

########################

# obtain list of the number of series in each data set

# import names of original data files - this may include protected versions
# so we have to remove those
train_file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
train_file_names <- grep("AN_", train_file_names, value=TRUE, invert=TRUE)
train_file_names <- grep("DP_", train_file_names, value=TRUE, invert=TRUE)
train_file_names <- grep("k-nts", train_file_names, value=TRUE, invert=TRUE)

train_file_names

########################

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_test", list.files(fp), value=TRUE)

file_names

privacy_methods = list("original" = c("1"),
                       "var-sim-lag" = c("s"), 
                       "var-knts-lag" = c("k"), 
                       "var-an-lag" = c("n"),
                       "AN_" = c(0.25, 0.5, 1, 1.5, 2), 
                       "DP_" = c(0.1, 1, 4.6, 10, 20), 
                       "k-nts_" = c(3, 5, 7, 10, 15), 
                       "k-nts-plus_" = c(3, 5, 7, 10, 15),
                       "k-nts-plus-bounded_" = c("3-0.5", "3-1", "3-1.5"))

# obtain names of files for protected data for forecasting the last period
forecast_file_names <- list.files(paste0("../../Outputs/Forecasts/", data_folder))

forecast_file_names <- grep("h2", forecast_file_names, value=TRUE, invert=TRUE)

fcast_ident_data <- tibble()

for (i in seq_along(privacy_methods)){
  
  current_name <- names(privacy_methods[i])
  
  params <- privacy_methods[[i]]
  
  for (j in seq_along(params)){
    
    current_param <- as.character(params[j])
    
    ident_probs <- c()
    
    # import the files that correspond to the privacy method and parameter
    if (current_name == "original"){
      pm_files <- grep("var-sim-lag", forecast_file_names, value=TRUE, invert=TRUE)
      pm_files <- grep("var-knts-lag", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("var-an-lag", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("AN_", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("DP_", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("k-nts_", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("k-nts-plus_", pm_files, value=TRUE, invert=TRUE)
      pm_files <- grep("k-nts-plus-bounded_", pm_files, value=TRUE, invert=TRUE)
    } else if (current_name %in% c("var-sim-lag", "var-knts-lag", "var-an-lag")) {
      pm_files <- grep(current_name, forecast_file_names, value=TRUE)
    } else {
      pm_files <- grep(current_name, forecast_file_names, value=TRUE)
      pm_files <- grep(paste0("_", current_param, "_"), pm_files, value=TRUE)
    }

    for (f in seq_along(pm_files)){

      props_identified <- c()
      
      num_series <- c()
        
      forecasts <- t(read.csv(paste0("../../Outputs/Forecasts/", data_folder, pm_files[f])))[,1]
        
      file_info <- strsplit(pm_files[f], split="_")[[1]]
      
      if (current_name == "original"){
        file_id <- file_info[3]
      } else {
        file_id <- file_info[5]
      }
      
      model <- file_info[1]
      
      test <- read.csv(paste0("../../Data/Cleaned/", data_folder, grep(file_id, file_names, value=TRUE)))[,1]
      
      # find how many series of each length occur in each data set,
      # use this to calculate baseline privacy risk
      
      length_counts <- list()
        
      # import data and convert to a list of series
      ts_data <- as.list(as.data.frame(t(read.csv(paste0(fp, grep(file_id, train_file_names, value=TRUE))))))
        
      # remove NA values from the end of each series
      ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
        
      # split into separate data sets, one for each series length
      ls <- c()
      lengths <- unname(sapply(ts_data, length))
      unique_lengths <- unique(lengths)
      
      for (l in seq_along(unique_lengths)){
        
        ls <- c(ls, sum(lengths==unique_lengths[l]))
        
        temp_test <- test[lengths==unique_lengths[l]]
        
        temp_forecasts <- forecasts[lengths==unique_lengths[l]]
        
        forecast_matrix <- matrix(rep(temp_forecasts, length(temp_forecasts)), ncol=length(temp_forecasts), byrow=TRUE)
        
        diff_matrix <- apply(forecast_matrix, 2, function(x) pmf(1/(abs(x-temp_test)+.Machine$double.eps)))
        
        props_identified <- c(props_identified, mean(apply(diff_matrix, 2, which.max) == (1:length(temp_test))))
        
        num_series <- c(num_series, length(temp_test))
        
      }
      
      fcast_ident_data <- bind_rows(fcast_ident_data, 
                                    tibble(method=current_name,
                                           parameter=current_param,
                                           model=model,
                                           file_id=file_id,
                                           num_series=num_series, 
                                           proportion_identified=props_identified))
    }
  }
}

## examine weighted average of proportion identified and
## max proportion identified

avg_prop_ident <- fcast_ident_data %>%
  group_by(method, parameter, file_id, num_series) %>%
  summarize(avg_proportion_identified = mean(proportion_identified), .groups='drop')
      
weight_avg_prop_ident <- avg_prop_ident %>%
  group_by(method, parameter) %>%
  summarize(weight_avg_prop = sum(num_series/sum(num_series) * avg_proportion_identified),
            total_series = sum(num_series))
      
var_weight_avg_prop_ident <- fcast_ident_data %>%
  filter(model == "VAR") %>%
  group_by(method, parameter, file_id, num_series) %>%
  summarize(avg_proportion_identified = mean(proportion_identified), .groups='drop') %>%
  filter(method %in% c("var-sim-lag", "k-nts-plus_", "k-nts-plus-bounded_")) %>%
  group_by(method, parameter) %>%
  summarize(weight_avg_prop = sum(num_series/sum(num_series) * avg_proportion_identified),
            total_series = sum(num_series))

write.csv(weight_avg_prop_ident, "../../Outputs/Results/M3/Tables/weighted_fcast_prop_ident.csv", row.names=FALSE)
write.csv(var_weight_avg_prop_ident, "../../Outputs/Results/M3/Tables/weighted_var_fcast_prop_ident.csv", row.names=FALSE)
