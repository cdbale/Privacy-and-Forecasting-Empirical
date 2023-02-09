### Assessing the privacy of protected time series against 
### identification and attribute disclosure.

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
  
  # split X into three separate datasets, one for each series length
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
    
    max_index <- length(series) - sample_size # not allowing for last value to be included in sample so that it can be assessed in attribute disclosure
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
  
  ########### Attribute disclosure risk ###########
  
  # calculate the predicted match for each confidential series
  id_preds <- lapply(pmfs, which.max)
  
  # known confidential values
  knowns <- c(do.call(cbind, lapply(csamps, function(x) x$sample)))
  
  # get the protected sample from each of the id_pred series
  id_samples <- c(do.call(cbind, lapply(1:length(id_preds), function(x) sapply(id_preds[[x]], function(y) psamps[[x]][[y]]$sample))))
  
  df <- data.frame("knowns"=knowns, "id_samples"=id_samples)
  
  # regress known confidential values on protected values
  mod <- lm(knowns ~ id_samples, data=df)
  
  # get the next protected value from each predicted match series
  next_protected <- data.frame("id_samples"=t(do.call(cbind, lapply(1:length(id_preds), function(x) protected_data[[id_preds[[x]]]][csamps[[x]]$starting_index+sample_size]))))
  
  # predict confidential values
  predicted_vals <- predict(mod, next_protected)
  
  # collect actual values to compare to predicted
  actual_vals <- sapply(1:length(csamps), function(x) confidential_data[[x]][csamps[[x]]$starting_index+sample_size])
  
  # calculate mean absolute error
  sum_abs_errors <- abs(predicted_vals - actual_vals)
  
  return(list("pmfs"=pmfs, "errors"=sum_abs_errors))
}

## perform S privacy simulations

run_simulations <- function(confidential_data, protected_data, sample_size, num_simulations){
  
  PMFS <- list()
  ERRORS <- c()
  
  for (s in 1:num_simulations){
    sim_results <- privacy_assessment(confidential_data=confidential_data, protected_data=protected_data, sample_size=sample_size)
    PMFS[[s]] <- sim_results$pmfs
    ERRORS <- c(ERRORS, sim_results$errors)
  }
  
  # create list of matrices, each matrix has pmfs from the simulation,
  # each row is for a series, the pmf across all series
  # The diagonal is the probability of correct match
  PMFS <- lapply(PMFS, function(x) do.call(rbind, x))
  
  # calculate the average percent change in the probability of identification disclosure
  # across all series and simulations
  ProbIdent <- unlist(lapply(PMFS, function(x) (diag(x) - (1/nrow(x))) * nrow(x) * 100))
  
  # calculate the average proportion of correctly identified time series across all external data
  # samples and confidential time series
  IsIdentified <- unlist(lapply(PMFS, function(x) apply(x, 1, which.max) == 1:nrow(x)))
  
  return(list("ProbIdent"=ProbIdent, "IsIdentified"=IsIdentified, "Errors"=ERRORS))
}

simulation_results <- function(confidential_data_list, protected_data_list, sample_size, num_simulations){
  
  results <- lapply(1:length(confidential_data_list), function(x) run_simulations(confidential_data_list[[x]], protected_data_list[[x]], sample_size=E, num_simulations=S))
  
  AvgProbIdent <- mean(unlist(sapply(results, function(x) x$ProbIdent)))
  
  AvgPropIdent <- mean(unlist(sapply(results, function(x) x$IsIdentified)))
  
  AvgAbsError <- mean(unlist(sapply(results, function(x) x$Errors)))
  
  return(list("AvgProbIdent"=AvgProbIdent, "AvgPropIdent"=AvgPropIdent, "AttPredError"=AvgAbsError))
  
}

# number of values to sample from each series
E <- 10

# number of simulations
S <- 20

# confidential series
X <- process_series(read.csv("../../Data/Train/Clean/m3_monthly_micro_h1.csv"))

# protected_file_names <- grep("protected", list.files("../../Data/Train/Clean/"), value=TRUE)

protected_file_names <- grep("h1", list.files("../../Data/Train/Clean/"), value=TRUE)

protected_file_names <- grep("h18", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- grep("Top", protected_file_names, value=TRUE, invert=TRUE)

protected_file_names <- grep("Bottom", protected_file_names, value=TRUE, invert=TRUE)

res_list <- list()

for (f in protected_file_names){
  
  if (substr(f, start=1, stop=3) == "m3_"){
    fname <- "original"
  } else{
    fname <- substr(f, start=31, stop=nchar(f)-4)
  }
  
  X_p <- process_series(read.csv(paste0("../../Data/Train/Clean/", f)))
  
  res <- simulation_results(confidential_data_list=X, protected_data_list=X_p, sample_size=E, num_simulations=S)
  
  res_list[[fname]] <- res
  
}

all_results <- t(do.call(cbind, res_list))

all_results
