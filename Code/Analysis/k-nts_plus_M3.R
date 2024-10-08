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

data_folder <- "M3/"

source("custom_feature_functions.R")
source("k-nts_helper_functions.R")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# paths to the data files and feature files
fp <- paste0("../../Data/Cleaned/", data_folder)
features_path <- paste0("../../Data/Features/", data_folder)
# path to files with error distributions
ed_file_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

## create file to track computation time for protecting data sets with
# k-nTS+
computation_time <- tibble()

# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# number of RFE iterations
num_iter <- 25

# track computation time for k-nTS+ swapping
feature_file_names <- grep("h2_train", list.files(features_path), value=TRUE)

# loop over file names
for (f in file_names){
  
  # determine sp
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # import data to determine where to split based on length
  f_data <- import_data(file_name=f, file_path=fp, sp=sp)
  
  # length grouping
  grouping_id <- as.numeric(as.factor(sapply(f_data, length)))
  
  # store file prefix
  prefix <- strsplit(f, split="_")[[1]][1]
  
  ## time for feature processing and preparation
  feature_start <- Sys.time()
  
  # file with errors for all models on the original data
  eds <- read_csv(paste0(ed_file_path, paste0(prefix, "_all_distributions_h2.csv")))
  
  # transform to tidy
  eds <- eds %>% gather(key="name", value="values") %>%
    mutate(name = substring(name, 1, nchar(name)-8)) %>%
    separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_") %>%
    mutate(Protection = if_else(is.na(Protection), "Original", ifelse(Protection == prefix, "Original", Protection)),
           Parameter = if_else(is.na(Parameter), "Original", Parameter)) %>%
    select(-Data)
  
  # now add series groupings
  eds <- eds %>%
    group_by(Model, Protection, Parameter) %>%
    group_split()
  
  eds <- bind_rows(lapply(eds, function(x) x %>% bind_cols(grouping_id = grouping_id)))
  
  ### now import corresponding time series features and link to forecast errors
  current_feature_file_names <- grep(prefix, feature_file_names, value=TRUE)
  
  full_features <- tibble()
  
  # import protected features and assign new variable values for linking to errors
  for (ff in current_feature_file_names){
    
    features <- read_csv(paste0(features_path, ff))
    
    params <- strsplit(ff, split="_")[[1]]
    
    print(params)
    
    if (!params[2] %in% c("AN", "DP")){
      features["Protection"] <- "Original"
      features["Parameter"] <- "Original"
    } else {
      features["Protection"] <- params[2]
      features["Parameter"] <- params[3]
    }
    
    full_features <- bind_rows(full_features, features)
    
  }
  
  ## join features to errors
  split_eds <- eds %>% 
    group_by(Model, Protection, Parameter) %>%
    group_split()
  
  full_data <- list()
  
  for (i in seq_along(split_eds)){
    pps <- split_eds[[i]] %>% distinct(Protection, Parameter)
    prot <- pps %>% pull(Protection)
    param <- pps %>% pull(Parameter)
    feat_df <- full_features %>%
      filter(Protection==prot, Parameter==param) %>%
      select(-Protection, -Parameter)
    new <- split_eds[[i]] %>%
      bind_cols(feat_df)
    full_data[[i]] <- new
    print(i)
  }
  
  full_data <- do.call(rbind, full_data)
  
  ############################
  ############################
  
  models <- unique(full_data$Model)
  
  full_data <- full_data %>%
    group_split(grouping_id)
  
  # we have the series split by their length into groups, within each group
  # we have the errors from all models combined with the time series features
  full_data <- lapply(full_data, function(x) x %>% group_split(Model))
  
  full_data <- lapply(full_data, function(x) lapply(x, function(y) y %>% select(-Model, 
                                                                                -Horizon, 
                                                                                -Protection, 
                                                                                -Parameter,
                                                                                -grouping_id)))
  
  full_data_scaled <- lapply(full_data, function(x) lapply(x, function(y) as.data.frame(scale(y))))
  
  feature_stop <- Sys.time()
  
  #########################################
  
  ############# ############# #############
  
  ## Perform feature selection
  
  fsr <- lapply(full_data_scaled, function(x) feature_selection(x, num_rfe_iters=num_iter, models=models))
  
  ## save RReliefF feature rankings (across forecasting models)
  
  print("Feature selection done.")
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RReliefF Rankings/", data_folder))){
    
    for (i in seq_along(fsr)){
      write.csv(fsr[[i]][["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/", data_folder, "RReliefF_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)  
    }
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RReliefF Rankings/", data_folder)), recursive=TRUE)
    
    for (i in seq_along(fsr)){
      # specifying the working directory
      write.csv(fsr[[i]][["evals_combined"]], file=paste0("../../Outputs/RReliefF Rankings/", data_folder, "RReliefF_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)
    }
    
  }
  
  ## save RFE feature rankings
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RFE Rankings/", data_folder))){
    
    for (i in seq_along(fsr)){
      write.csv(fsr[[i]][["rank_df"]], file=paste0("../../Outputs/RFE Rankings/", data_folder, "RFE_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)
    }
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RFE Rankings/", data_folder)), recursive=TRUE)
    
    for (i in seq_along(fsr)){
      write.csv(fsr[[i]][["rank_df"]], file=paste0("../../Outputs/RFE Rankings/", data_folder, "RFE_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)
    }
  }
  
  ## save RFE oob results
  
  # check if sub directory exists 
  if (file.exists(paste0("../../Outputs/RFE OOB/", data_folder))){
    
    for (i in seq_along(fsr)){
      write.csv(fsr[[i]][["combined_oob"]], file=paste0("../../Outputs/RFE OOB/", data_folder, "RFE_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)
    }
    
    
  } else {
    
    # create a new sub directory inside
    # the main path
    dir.create(file.path(paste0("../../Outputs/RFE OOB/", data_folder)), recursive=TRUE)
    
    for (i in seq_along(fsr)){
      write.csv(fsr[[i]][["combined_oob"]], file=paste0("../../Outputs/RFE OOB/", data_folder, "RFE_", prefix, "_", i, "_h1_train.csv"), row.names=FALSE)
    }
  }
  
  print("File saving done.")
  
  sf <- lapply(fsr, function(x) x[["selected_features"]])
  imp_weights <- lapply(fsr, function(x) x[["importance_weights"]])
  
  window_length <- max(c(2*sp + 1, 12))
  
  ################################################
  kvals <- c(3, 5, 7, 10, 15)
  
  swap_start <- Sys.time()
  
  X_knts <- perform_knts(ts_file=f,
                         ts_file_path=fp,
                         seasonal_period=sp,
                         window_length=window_length,
                         kvals=kvals,
                         features_to_calculate=fv,
                         selected_features=sf,
                         importance_weights=imp_weights,
                         is_plus=TRUE,
                         is_rate=FALSE)
  
  swap_stop <- Sys.time()
  
  swap_time <- difftime(swap_stop, swap_start, units="secs")
  
  for(i in seq_along(X_knts)){
    write.csv(X_knts[[i]], file=paste0(fp, "k-nts-plus_", kvals[i], "_", f), row.names=FALSE)
  }
  
  computation_row <- tibble(File = f,
                            feature_prep = as.double(difftime(feature_stop, feature_start, units="secs")),
                            RReliefF = sum(sapply(1:length(fsr), function(x) fsr[[x]][["relief_time"]])),
                            RFE = sum(sapply(1:length(fsr), function(x) fsr[[x]][["rfe_time"]])),
                            swap3 = swap_time,
                            swap5 = swap_time,
                            swap7 = swap_time,
                            swap10 = swap_time,
                            swap15 = swap_time)

  computation_time <- bind_rows(computation_time, computation_row)
  
  print(paste0("File ", f, " complete."))
  
}

write.csv(computation_time, file=paste0("../../Data/Computation_Time/", substr(data_folder, 1, nchar(data_folder)-1), "_k-nts-plus.csv"), row.names=FALSE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
