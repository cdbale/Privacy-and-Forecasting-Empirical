### Assessing the privacy of protected time series against 
### identification and attribute disclosure.

# Author: Cameron Bale

######## start with calculating the identification risk from
######## random guessing

# paths to the data files and feature files
fp <- "../../Data/Cleaned/"

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
sum(unlist(length_counts))

# since prob of identification from random guessing is 1/# in group,
# and we weight that by # in group/total number series, the # in group
# cancel and we are left with 1/total number series * number of groups
1/sum(unlist(length_counts)) * length(unlist(length_counts))

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
  
  # ########### Attribute disclosure risk ###########
  # 
  # # calculate the predicted match for each confidential series
  # id_preds <- lapply(pmfs, which.max)
  # 
  # # known confidential values
  # knowns <- c(do.call(cbind, lapply(csamps, function(x) x$sample)))
  # 
  # # get the protected sample from each of the id_pred series
  # id_samples <- c(do.call(cbind, lapply(1:length(id_preds), function(x) sapply(id_preds[[x]], function(y) psamps[[x]][[y]]$sample))))
  # 
  # df <- data.frame("knowns"=knowns, "id_samples"=id_samples)
  # 
  # # regress known confidential values on protected values
  # mod <- lm(knowns ~ id_samples, data=df)
  # 
  # # get the next protected value from each predicted match series
  # next_protected <- data.frame("id_samples"=t(do.call(cbind, lapply(1:length(id_preds), function(x) protected_data[[id_preds[[x]]]][csamps[[x]]$starting_index+sample_size]))))
  # 
  # # predict confidential values
  # predicted_vals <- predict(mod, next_protected)
  # 
  # # collect actual values to compare to predicted
  # actual_vals <- sapply(1:length(csamps), function(x) confidential_data[[x]][csamps[[x]]$starting_index+sample_size])
  # 
  # # calculate mean absolute error
  # sum_abs_errors <- abs(predicted_vals - actual_vals)
  
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
  
  # calculate the average percent change in the probability of identification disclosure
  # across all series and simulations
  # ProbIdent <- unlist(lapply(PMFS, function(x) (diag(x) - (1/nrow(x))) * nrow(x) * 100))
  
  # calculate the average proportion of correctly identified time series across all external data
  # samples and confidential time series
  IsIdentified <- unlist(lapply(PMFS, function(x) apply(x, 1, which.max) == 1:nrow(x)))
  
  # return(list("ProbIdent"=ProbIdent, "IsIdentified"=IsIdentified, "Errors"=ERRORS))
  
  return(list("IsIdentified"=IsIdentified, "num_series"=length(confidential_data)))
}

simulation_results <- function(confidential_data_list, protected_data_list, sample_size, num_simulations){
  
  # results <- lapply(1:length(confidential_data_list), function(x) run_simulations(confidential_data_list[[x]], protected_data_list[[x]], sample_size=E, num_simulations=S))
  
  results <- run_simulations(confidential_data_list, protected_data_list, sample_size=E, num_simulations=S)
  
  # AvgProbIdent <- mean(unlist(sapply(results, function(x) x$ProbIdent)))
  
  # AvgPropIdent <- lapply(results, function(x) list(mean(x$IsIdentified), x$num_series))
  
  weighted_prop <- results[[2]]/2363 * mean(results[[1]])
  
  # AvgPropIdent <- mean(unlist(sapply(results, function(x) x$IsIdentified)))
  
  # AvgAbsError <- mean(unlist(sapply(results, function(x) x$Errors)))
  
  # return(list("AvgProbIdent"=AvgProbIdent, "AvgPropIdent"=AvgPropIdent, "AttPredError"=AvgAbsError))
  
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
protected_file_names <- list.files("../../Data/Cleaned/")

protected_file_names <- grep("h2", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- grep("_test.csv", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- protected_file_names[!protected_file_names %in% file_names]


########## don't do it this way anymore, make a function that does the
########## privacy simulation for a given protection method and parameter value.
########## run the function for each combination.

protected_file_names

privacy_methods = list("VAR-simulated" = c("sim"),
                       # "AN_" = c(0.25, 0.5, 1, 1.5, 2), 
                       # "DP_" = c(0.1, 1, 4.6, 10, 20), 
                       # "k-nts_" = c(3, 5, 7, 10, 15), 
                       # "k-nts-corr_" = c(3, 5, 7, 10, 15), 
                       "k-nts-plus_" = c(3, 5, 7, 10, 15),
                       "gratis-full-k-nts-plus_" = c(3, 5, 7, 10, 15),
                       "gratis-k-nts-plus_" = c(3),
                       # "k-nts-plus-corr_" = c(3, 5, 7, 10, 15),
                       "preprocess-k-nts-plus_" = c(3, 5, 7, 10, 15))
                       # "preprocess-lw-k-nts-plus_" = c(3, 5, 7, 10, 15))

var_sim_files <- list.files("../../Outputs/VAR Simulated/")
# make sure protected versions are excluded
var_sim_files <- grep("AN_", var_sim_files, value=TRUE, invert=TRUE)
var_sim_files <- grep("DP_", var_sim_files, value=TRUE, invert=TRUE)
var_sim_files <- grep("k-nts", var_sim_files, value=TRUE, invert=TRUE)

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
    } else {
      # import the files that correspond to the privacy method and parameter
      pm_files <- grep(current_name, list.files("../../Data/Cleaned/"), value=TRUE)
      if (!current_name %in% c("preprocess-k-nts-plus_", "preprocess-lw-k-nts-plus_")){
        pm_files <- grep("preprocess", pm_files, value=TRUE, invert=TRUE)
      }
      if (!current_name %in% c("gratis-full-k-nts-plus_", "gratis-k-nts-plus_")){
        pm_files <- grep("gratis", pm_files, value=TRUE, invert=TRUE)
      }
      pm_files <- grep(paste0("_", current_param, "_"), pm_files, value=TRUE)
      pm_files <- grep("_h2", pm_files, value=TRUE, invert=TRUE)
      
    }
    
    for (f in seq_along(pm_files)){
      
      if (current_name == "VAR-simulated"){
        
        confidential_file_prefix <- strsplit(pm_files[f], "_")[[1]][1]
        
        # import the protected series
        X <- process_series(read.csv(paste0("../../Data/Cleaned/", grep(confidential_file_prefix, file_names, value=TRUE))))
        
        # import the protected series
        Xp <- process_series(read.csv(paste0("../../Outputs/VAR Simulated/", pm_files[f])))
        
        for (subex in seq_along(Xp)){
          
          ident_probs <- append(ident_probs, simulation_results(confidential_data_list=X[[subex]], protected_data_list=Xp[[subex]], sample_size=E, num_simulations=S))
          
        }
        
      } else {
      
        confidential_file_prefix <- strsplit(pm_files[f], "_")[[1]][3]
      
        # import the protected series
        X <- process_series(read.csv(paste0("../../Data/Cleaned/", grep(confidential_file_prefix, file_names, value=TRUE))))
      
        # import the protected series
        Xp <- process_series(read.csv(paste0("../../Data/Cleaned/", pm_files[f])))

        for (subex in seq_along(Xp)){
        
          ident_probs <- append(ident_probs, simulation_results(confidential_data_list=X[[subex]], protected_data_list=Xp[[subex]], sample_size=E, num_simulations=S))
        
        }
      }
    }
    
    weighted_ident[[current_name]][[current_param]] <- ident_probs
    
  } 
}

library(tidyverse)

totals <- lapply(weighted_ident, function(x) sapply(x, function(y) sum(y)))

totals <- lapply(1:length(totals), function(x) tibble("Method"=names(privacy_methods)[x],
                                                      "AvgIdentificationProb"=totals[[x]],
                                                      "Parameter"=names(totals[[x]])))

totals <- do.call(rbind, totals)

write.csv(totals, "../../Outputs/Results/Tables/overall_privacy_averages.csv", row.names = FALSE)

totals %>%
  filter(Method == "gratis-full-k-nts-plus_")

all_length_counts <- unlist(length_counts)

################################################################################
################################################################################
################################################################################

# here is the average identification probability for each subset of M3 data.
probs <- weighted_ident[['k-nts-plus_']][['3']] * (2363/all_length_counts)

ul_length_counts <- unlist(length_counts)

prob_counts <- sapply(length_counts, length)

new_probs <- c()
for (i in prob_counts){
  p <- sum(ul_length_counts[1:i]/sum(ul_length_counts[1:i]) * probs[1:i])
  ul_length_counts <- ul_length_counts[(i+1):length(ul_length_counts)]
  probs <- probs[(i+1):length(probs)]
  new_probs <- append(new_probs, p)
  print(ul_length_counts)
  print(probs)
}

# what is the correlation between these identification probabilities and the
# similarities of the time series? We are looking for time series with similar
# features, so let's start with the 'fitness' of the time series. Let's start
# with calculating the three highest fitness values from the neighbors of
# each time series.










# 
# #### loop over privacy methods
# #### calculate the average identification disclosure risk for each method
# 
# privacy_methods = list("AN_" = c(0.25, 0.5, 1, 1.5, 2), 
#                        "DP_" = c(0.1, 1, 4.6, 10, 20), 
#                        "k-nts_" = c(3, 5, 7, 10, 15), 
#                        "k-nts-corr_" = c(3, 5, 7, 10, 15), 
#                        "k-nts-plus_" = c(3, 5, 7, 10, 15),
#                        "k-nts-plus-corr_" = c(3, 5, 7, 10, 15))
# 
# pnames <- names(privacy_methods)
# 
# res_list <- list()
# 
# # loop over privacy methods
# for (pm in seq_along(privacy_methods)){
#   res_list[[pnames[pm]]] <- list()
#   for (param in privacy_methods[[pm]]){
#     print(param)
#     # res_list[[pnames[pm]]][[str(param)]] <- list()
#     # res_list[[pnames[pm]]][[str(param)]][[1]] <- "stuff"
#   }
# }
# 
# #   
# #   pm_file_names <- grep(pm, protected_file_names, value=TRUE)
# #   
# #   for (cf in file_names){
# #     
# #     res_list[[pm]][[cf]] <- list()
# #     
# #     # import the confidential series
# #     X <- process_series(read.csv(paste0("../../Data/Cleaned/", cf)))
# #     
# #     # loop over confidential subsets
# #     file_type <- strsplit(cf, split="_")[[1]][1]
# #     
# #     spf <- grep(file_type, pm_file_names, value=TRUE)
# #     
# #     for (pf in spf){
# #       
# #       params <- strsplit(pf, split="_")[[1]][1:2]
# #       
# #       X_p <- read.csv(paste0("../../Data/Cleaned/", pf))
# #       
# #       if (pm %in% c("k-nts", "k-nts-corr")){
# #         X_p <- X_p[,2:ncol(X_p)]
# #       }
# #       
# #       X_p <- process_series(X_p)
# #       
# #       res_list[[pm]][[cf]][[params[2]]] <- simulation_results(confidential_data_list=X, protected_data_list=X_p, sample_size=E, num_simulations=S)
# #       
# #     }
# #     
# #   }
# #   
# # }
# 
# temp <- res_list
# 
# an_temp <- res_list[[1]]
# 
# # there are 2,363 series total
# 
# for (i in an_temp){
#   for (j in i){
#     ttt <- j
#   }
# }
# 
# 
# 
# for (i in seq_along(1:length(temp))){
#   for (j in seq_along(1:length(i))){
#     for (k in seq_along(1:length(j))){
#       temp[[i]][[j]][[k]] <- as.data.frame(do.call(rbind, temp[[i]][[j]][[k]]))
#     }
#   }
# }
# 
# 
# temp[[1]][[1]][[1]]
# 




