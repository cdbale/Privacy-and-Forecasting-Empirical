### Assessing the privacy of protected time series against 
### identification and attribute disclosure.

# Author: Cameron Bale

data_folder = "M3/"

######## start with calculating the identification risk from
######## random guessing

# paths to the data files and feature files
fp <- paste0("../../Data/Cleaned/", data_folder)

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

file_names

# find how many series of each length occur in each data set,
# use this to calculate baseline privacy risk

length_counts <- list()

for (f in seq_along(file_names)){
  
  # import data and convert to a list of series
  ts_data <- as.list(as.data.frame(t(read.csv(paste0(fp, file_names[f])))))
  
  # remove NA values from the end of each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # split into separate data sets, one for each series length
  ls <- c()
  lengths <- sapply(ts_data, length)
  unique_lengths <- unique(lengths)
  for (l in seq_along(unique_lengths)){
    ls <- c(ls, sum(lengths==unique_lengths[l]))
  }
  
  length_counts[[f]] <- ls
}

# total number of series
num_series <- sum(unlist(length_counts))

# since prob of identification from random guessing is 1/# in group,
# and we weight that by # in group/total number series, the # in group
# cancel and we are left with 1/total number series * number of groups
1/num_series * length(unlist(length_counts))

#### Identification disclosure ####

## start with performing for one data set ##

## generalize to function/loop later ##

# read in protected time series

process_series <- function(time_series){
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(time_series)))
  
  # remove NA values from each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # truncate data to strictly positive
  ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  
  # split X into separate data sets, one for each series length
  Xs <- list()
  unique_lengths <- unique(sapply(ts_data, length))
  lengths <- sapply(ts_data, length)
  for (l in seq_along(unique_lengths)){
    ids <- lengths==unique_lengths[l]
    sub_X <- ts_data[ids]
    Xs[[l]] <- sub_X
  }
  
  return(Xs)
  
}

############## Making the assumption that all time series have the same length
############## and occur over the same time periods

################# Write a function to do one simulation - then run the function S times ######################

# sample num_samples of sample_size points from a time series, or at the specified
# sample indices
perform_sampling <- function(series, sample_size, sample_index=NULL){
  
  if (is.null(sample_index)){
    
    max_index <- length(series) - sample_size 
    starting_index <- sample(1:max_index, size=1)
    sample <- series[starting_index:(starting_index+sample_size-1)]
    return(list("sample"=sample, "starting_index"=starting_index))
    
  } else if (!is.null(sample_index)){
    
    sample <- series[sample_index:(sample_index+sample_size-1)]
    return(list("sample"=sample, "starting_index"=sample_index))
    
  }
  
}

l2_norm <- function(x){
  return(sqrt(sum(x^2)))
}

pmf <- function(x){
  return(x/sum(x))
}

privacy_assessment <- function(confidential_data, protected_data, sample_size){
  
  ########### Calculate pmfs for identity disclosure risk ###########
  
  # take a sample of size E from each time series in the confidential data
  csamps <- lapply(confidential_data, function(y) perform_sampling(y, sample_size=sample_size))
  
  # for each confidential sample, take a sample from the same time period from each 
  # protected series
  psamps <- lapply(csamps, function(x) lapply(protected_data, function(y) perform_sampling(y, sample_size=sample_size, sample_index=x$starting_index)))
  
  # calculate the similarities between a given confidential sample and each corresponding protected sample
  similarities <- lapply(1:length(psamps), function(x) sapply(psamps[[x]], function(y) 1/(l2_norm(csamps[[x]]$sample - y$sample)+.Machine$double.eps)))
  
  # calculate the pmf for each confidential series identity
  pmfs <- lapply(similarities, pmf)
  
  return(list("pmfs"=pmfs))#, "errors"=sum_abs_errors))
}

## perform S privacy simulations

run_simulations <- function(confidential_data, protected_data, sample_size, num_simulations){
  
  PMFS <- list()
  # ERRORS <- c()
  
  for (s in 1:num_simulations){
    sim_results <- privacy_assessment(confidential_data=confidential_data, protected_data=protected_data, sample_size=sample_size)
    PMFS[[s]] <- sim_results$pmfs
    # ERRORS <- c(ERRORS, sim_results$errors)
  }
  
  # create list of matrices, each matrix has pmfs from the simulation,
  # each row is for a series, the pmf across all series
  # The diagonal is the probability of correct match
  PMFS <- lapply(PMFS, function(x) do.call(rbind, x))
  
  # calculate the average proportion of correctly identified time series across all external data
  # samples and confidential time series
  IsIdentified <- unlist(lapply(PMFS, function(x) apply(x, 1, which.max) == 1:nrow(x)))
  
  return(list("IsIdentified"=IsIdentified, "num_series"=length(confidential_data)))
}

simulation_results <- function(confidential_data_list, protected_data_list, sample_size, num_simulations, num_series){
  
  results <- run_simulations(confidential_data_list, protected_data_list, sample_size=sample_size, num_simulations=num_simulations)
  
  weighted_prop <- results[[2]]/num_series * mean(results[[1]])
  
  return(weighted_prop)
  
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# going to need to take a weighted average of identification probabilities

# need the total number of series, the number of series in a given set, and
# the identification probability within that set

# number of values to sample from each series
E <- 10

# number of simulations
S <- 20

# obtain names of files for protected data for forecasting the last period
protected_file_names <- list.files(paste0("../../Data/Cleaned/", data_folder))

protected_file_names <- grep("h2", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- grep("_test.csv", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- protected_file_names[!protected_file_names %in% file_names]

protected_file_names

privacy_methods = list("original" = c(""),
                       "VAR-simulated" = c("sim"),
                       "AN_" = c(0.25, 0.5, 1, 1.5, 2), 
                       "DP_" = c(0.1, 1, 4.6, 10, 20), 
                       "k-nts_" = c(3, 5, 7, 10, 15), 
                       "k-nts-plus_" = c(3, 5, 7, 10, 15),
                       "k-nts-plus-bounded_" = c("3-0.5", "3-1", "3-1.5"))

var_sim_files <- list.files("../../Outputs/VAR Simulated/M3/")
# make sure protected versions are excluded
var_sim_files <- grep("AN_", var_sim_files, value=TRUE, invert=TRUE)
var_sim_files <- grep("DP_", var_sim_files, value=TRUE, invert=TRUE)
var_sim_files <- grep("k-nts", var_sim_files, value=TRUE, invert=TRUE)
var_sim_files <- grep("rate", var_sim_files, value=TRUE, invert=TRUE)

weighted_ident <- list()

for (i in seq_along(privacy_methods)){
  
  current_name <- names(privacy_methods[i])
  
  weighted_ident[[current_name]] <- list()
  
  params <- privacy_methods[[i]]
  
  for (j in seq_along(params)){
    
    current_param <- as.character(params[j])
    
    ident_probs <- c()
    
    if (current_name == "VAR-simulated"){
      pm_files <- var_sim_files
    } else if (current_name == "original") {
      pm_files <- file_names
    } else {
      # import the files that correspond to the privacy method and parameter
      pm_files <- grep(current_name, list.files(fp), value=TRUE)
      pm_files <- grep(paste0("_", current_param, "_"), pm_files, value=TRUE)
      pm_files <- grep("_h2", pm_files, value=TRUE, invert=TRUE)
    }
    
    for (f in seq_along(pm_files)){
      
      if (current_name == "VAR-simulated"){
        
        confidential_file_prefix <- strsplit(pm_files[f], "_")[[1]][1]
        
        # import the original series
        X <- process_series(read.csv(paste0(fp, grep(confidential_file_prefix, file_names, value=TRUE))))
        
        # import the protected series
        Xp <- process_series(read.csv(paste0("../../Outputs/VAR Simulated/M3/", pm_files[f])))
        
        # the VAR simulated series may not be as long as the unprotected series due to the lags needed 
        # in the simulation process.
        # so, we just need to exclude the extra values from the beginning of the unprotected series
        # i.e., we need to ensure that confidential series and protected series overlap on the same time periods only
        pls <- sapply(Xp, function(x) length(x[[1]]))
        
        X <- lapply(1:length(X), function(x) lapply(X[[x]], function(y) y[(length(y)-pls[x]+1):length(y)]))
        
        for (subex in seq_along(Xp)){
          
          ident_probs <- append(ident_probs, simulation_results(confidential_data_list=X[[subex]], protected_data_list=Xp[[subex]], sample_size=E, num_simulations=S, num_series=num_series))
          
        }
        
      } else if (current_name == "original") {
      
        # import the protected series
        X <- process_series(read.csv(paste0(fp, pm_files[f])))
      
        # import the protected series
        Xp <- process_series(read.csv(paste0(fp, pm_files[f])))

        for (subex in seq_along(Xp)){
        
          ident_probs <- append(ident_probs, simulation_results(confidential_data_list=X[[subex]], protected_data_list=Xp[[subex]], sample_size=E, num_simulations=S, num_series=num_series))
        
        }
        
      } else {
        
        confidential_file_prefix <- strsplit(pm_files[f], "_")[[1]][3]
        
        # import the protected series
        X <- process_series(read.csv(paste0(fp, grep(confidential_file_prefix, file_names, value=TRUE))))
        
        # import the protected series
        Xp <- process_series(read.csv(paste0(fp, pm_files[f])))
        
        for (subex in seq_along(Xp)){
          
          ident_probs <- append(ident_probs, simulation_results(confidential_data_list=X[[subex]], protected_data_list=Xp[[subex]], sample_size=E, num_simulations=S, num_series=num_series))
          
        }
      }
    }
    
    weighted_ident[[current_name]][[current_param]] <- ident_probs
    
  } 
}

library(tidyverse)

# compute proportion identified in each data subset

length_counts <- unlist(length_counts)

# compute weighted average
totals <- lapply(weighted_ident, function(x) sapply(x, function(y) sum(y)))

totals <- lapply(1:length(totals), function(x) tibble("Method"=names(privacy_methods)[x],
                                                      "AvgIdentificationProb"=totals[[x]],
                                                      "Parameter"=names(totals[[x]])))

totals <- do.call(rbind, totals)

# check if sub directory exists 
if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
  write.csv(totals, paste0("../../Outputs/Results/", data_folder, "Tables/overall_privacy_averages.csv"), row.names = FALSE)
} else {
  # create a new sub directory for storing time series features
  dir.create(file.path(paste0("../../Outputs/Results/", data_folder, "Tables/")), recursive=TRUE)
  write.csv(totals, paste0("../../Outputs/Results/", data_folder, "Tables/overall_privacy_averages.csv"), row.names = FALSE)
}



################################################################################
