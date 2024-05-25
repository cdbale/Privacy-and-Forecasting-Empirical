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

source("custom_feature_functions.R")
source("k-nts_helper_functions.R")

# test on a single k-nTS+ file
data_path <- paste0("../../Data/Cleaned/", data_folder)

# vectors of k-nTS+ and original file names
knts_files <- grep("k-nts-plus_3_", list.files(data_path), value=TRUE)
og_files <- grep("_h1_train", list.files(data_path), value=TRUE)
og_files <- grep("AN_", og_files, value=TRUE, invert=TRUE)
og_files <- grep("DP_", og_files, value=TRUE, invert=TRUE)
og_files <- grep("k-nts", og_files, value=TRUE, invert=TRUE)

# do the trimming for all data sets

threshold_values <- c(0.5, 1, 1.5)

for (f in knts_files){
  
  file_id <- paste0(strsplit(f, "_")[[1]][3:6], collapse="_")
  
  # original file
  og_f <- grep(file_id, og_files, value=TRUE)
  
  # set seasonal period
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  
  # import data sets
  knts_data <- import_data(f, data_path, sp=sp, truncate=FALSE, take_log=FALSE)
  og_data <- import_data(og_f, data_path, sp=sp, truncate=FALSE, take_log=FALSE)
  
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
