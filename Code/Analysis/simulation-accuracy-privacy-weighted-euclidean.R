# Simulation to Show How Acceptable Accuracy/Privacy is Not Always Possible

# Author: Cameron Bale

library(plyr)
library(forecast)
library(tsfeatures)
library(gratis)
library(ExtDist)
library(e1071)
library(ranger)
library(tidytext)
library(CORElearn)
library(ggpubr)
library(ggh4x)
library(tidyverse)

source('custom_feature_functions.R')

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# Function to return spectral entropy, and ACF at lags 1 and 2
# given a numeric vector input
my_features <- function(y) {
  c(entropy = entropy(y),
    acf = acf(y, plot = FALSE)$acf[2:3, 1, 1])
}

# function to perform additive noise protection
additive_noise <- function(time_series, s){
  noise_vals <- rnorm(n=length(time_series), sd=s*sd(time_series))
  return(time_series + noise_vals)
}

# function to perform differential privacy protection
differential_privacy <- function(time_series, epsilon, original=FALSE){

  # calculate global sensitivity
  gs = max(time_series) - min(time_series)
  
  # add random noise sampled from 0-centered laplace
  # distribution with scale parameter = GS/epsilon
  noise_vals <- rLaplace(n=length(time_series), mu=0, b=gs/epsilon)
  
  return(time_series + noise_vals)
}

# sample num_samples of sample_size points from a time series, or at the specified
# sample indices. used for privacy simulation
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
  
  return(list("pmfs"=pmfs))
}

## perform S privacy simulations

run_simulations <- function(confidential_data, protected_data, sample_size, num_simulations){
  
  PMFS <- list()
  
  for (s in 1:num_simulations){
    sim_results <- privacy_assessment(confidential_data=confidential_data, protected_data=protected_data, sample_size=sample_size)
    PMFS[[s]] <- sim_results$pmfs
  }
  
  # create list of matrices, each matrix has pmfs from the simulation,
  # each row is for a series, the pmf across all series
  # The diagonal is the probability of correct match
  PMFS <- lapply(PMFS, function(x) do.call(rbind, x))
  
  # calculate the average proportion of correctly identified time series across all external data
  # samples and confidential time series
  IsIdentified <- unlist(lapply(PMFS, function(x) apply(x, 1, which.max) == 1:nrow(x)))
  
  return(IsIdentified)
}

simulation_results <- function(confidential_data_list, protected_data_list, sample_size, num_simulations){
  
  results <- run_simulations(confidential_data_list, protected_data_list, sample_size=sample_size, num_simulations=num_simulations)
  
  return(mean(results))
  
}

feature_selection <- function(scaled_feature_data, num_rfe_iters, models){
  
  ##############################################################################
  
  relief_start <- Sys.time()
  
  # Stage 1: RReliefF
  evals <- lapply(scaled_feature_data, function(x) attrEval("values", data=x, estimator="RReliefFexpRank"))
  
  evals_combined <- lapply(1:length(evals), function(x) as_tibble(evals[[x]], rownames="feature") %>% mutate(model = models[x]))
  
  evals_combined <- do.call(rbind, evals_combined)
  
  # features selected by RReliefF for each model
  relief_selection <- lapply(evals, function(x) names(x[x > 0]))
  
  relief_stop <- Sys.time()
  
  ##############################################################################
  
  rfe_start <- Sys.time()
  
  # Stage 2: RFE
  # setting seed
  set.seed(42)
  
  # list for oob errors
  oob_list <- list()
  
  # list for variable rankings
  rank_list <- list()
  
  for (i in seq_along(scaled_feature_data)){
    
    df <- scaled_feature_data[[i]]
    
    # list for oob errors
    oob_list[[i]] <- list()
    
    # list for variable rankings
    rank_list[[i]] <- list()
    
    for (j in 1:num_rfe_iters){
      
      # features to consider for cross validation
      rf_feature_names <- relief_selection[[i]]
      
      # out of bag errors
      oob_errors <- c()
      
      # loop variable ranks
      loop_ranks <- c()
      
      while(length(rf_feature_names) > 0){
        
        # create train data
        train <- df[, c("values", rf_feature_names)]
        
        # train random forest with current feature set
        rf_res <- ranger(values ~ ., data=train, importance="permutation", num.trees=500)
        
        oob_errors <- c(oob_errors, mean(abs(rf_res$predictions-train$values)))
        
        least_imp <- names(sort(importance(rf_res))[1])
        
        loop_ranks <- append(loop_ranks, least_imp)
        
        rf_feature_names <- rf_feature_names[rf_feature_names != least_imp]
        
        print(paste0("Dataframe ", i, ", Iteration ", j, ". Number of features: ", length(rf_feature_names)+1))
        
      }
      
      rank_list[[i]][[j]] <- loop_ranks
      
      oob_list[[i]][[j]] <- oob_errors
      
    }
    
  }
  
  avg_oob <- lapply(lapply(oob_list, function(x) do.call(cbind, x)), function(y) rowMeans(y))
  
  combined_oob <- do.call(rbind, lapply(1:length(avg_oob), function(x) tibble("num_features"=length(avg_oob[[x]]):1, "value"=avg_oob[[x]], "model"=models[x])))
  
  ns <- combined_oob %>%
    group_by(model) %>%
    mutate(min_error = min(value)) %>%
    ungroup() %>%
    filter(value == min_error) %>%
    group_by(model) %>%
    summarize(num_selected = min(num_features), .groups='drop') %>%
    mutate(avg_selected = floor(mean(num_selected))) %>%
    distinct(avg_selected) %>%
    pull()
  
  rank_df <- do.call(rbind, lapply(1:length(rank_list), function(y) do.call(rbind, lapply(rank_list[[y]], function(x) tibble("var"=x, "rank"=length(x):1, "model"=models[[y]])))))
  
  sf <- rank_df %>%
    group_by(var) %>%
    summarize(avg_rank = mean(rank)) %>%
    arrange(avg_rank) %>%
    slice(1:ns) %>%
    pull(var)
  
  # calculate feature importances
  importances <- lapply(scaled_feature_data, function(x) ranger(values ~ ., data=x[,c('values', sf)], importance="permutation", num.trees=500)$variable.importance)
  
  total_importances <- apply(do.call(rbind, importances), 2, sum)
  
  importance_weights <- total_importances/sum(total_importances)
  
  rfe_stop <- Sys.time()
  
  return(list("rank_df" = rank_df, 
              "combined_oob" = combined_oob, 
              "evals_combined" = evals_combined,
              "selected_features" = sf,
              "importance_weights" = importance_weights,
              "relief_time" = difftime(relief_stop, relief_start, units="mins"),
              "rfe_time" = difftime(rfe_stop, rfe_start, units="mins"),
              "rf" = rf_res))
}

knts_alg <- function(time_series, sp, window_length, k, features_to_calculate, selected_features, importance_weights, corr_based=FALSE){

  # number of time series
  num_series <- length(time_series)

  # number of time periods
  num_periods <- length(time_series[[1]])

  # matrix to hold new series
  X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)

  # restrict the data to the beginning window
  X_window <- lapply(time_series, function(x) ts(x[1:window_length], frequency=sp))

  if (corr_based){
    X_cor <- cor(do.call(cbind, X_window))
  }

  # calculate the features for the current window
  C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]

  # normalize features
  C <- as.data.frame(scale(C))

  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  # create weights matrix
  W <- diag(x=importance_weights)
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)

  # for each time period in the initial window
  for (j in 1:num_series){

    # select the jth column
    d <- D[,j]

    # sort the distances in the jth column smallest to largest
    sorted <- sort(d, index.return=TRUE)

    # select from index 2 to k+1 since first index corresponds to the series itself
    K <- sorted$ix[2:(k+1)]

    # for each series
    for (t in 1:window_length){

      if (corr_based){
        # sample an index based on correlation
        i <- sample(K, size=1, prob=swap_weights)
      } else {
        # sample an index
        i <- sample(K, size=1)
      }

      # replace the value
      X_new[t,j] <- time_series[[i]][t]

    }
  }

  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################

  for (t in (window_length+1):num_periods){

    # restrict the data to the current window
    X_window <- lapply(time_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))

    ## calculate the features for the current window
    C <- tsfeatures(X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
    # normalize features
    C <- as.data.frame(scale(C))
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)

    ## Calculate the feature distance matrix D
    # ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
    D <- ones_column %*% diag(t(C)%*%W%*%C) - 2*t(C)%*%W%*%C + diag(t(C)%*%W%*%C) %*% t(ones_column)

    for (j in 1:num_series){

      # select the jth column
      d <- D[,j]

      # sort the distances in the jth column smallest to largest
      sorted <- sort(d, index.return=TRUE)

      # select from index 2 to k+1 since first index corresponds to the series itself
      K <- sorted$ix[2:(k+1)]

      if (corr_based){
        # sample an index based on correlation
        i <- sample(K, size=1, prob=swap_weights)
      } else{
        # sample an index
        i <- sample(K, size=1)
      }

      # replace the value
      X_new[t,j] <- time_series[[i]][t]

    }
  }

  # remove outliers using tsoutliers
  X_new <- as.list(as.data.frame(X_new))
  
  # convert each series to a TS object with appropriate seasonal frequency
  X_new <- lapply(X_new, function(x) ts(x, frequency=sp))
  
  X_new <- lapply(X_new, outlier_removal)
  
  X_new <- as.matrix(do.call(cbind, X_new))
  
  return(X_new)

}

# function for replacing outliers
outlier_removal <- function(ts){
  temp_ts <- ts
  outlier_test <- tsoutliers(temp_ts, lambda=NULL)
  temp_ts[outlier_test$index] <- outlier_test$replacement
  return(temp_ts)
}

################################################################################
################################################################################

# write a function that accepts a set of time series and returns VAR
# forecasts for those series

var_forecast <- function(time_series, horizon, take_log=TRUE, return_model=FALSE, model_to_use=NULL){
  if (take_log){
    starting_values <- sapply(time_series, function(x) log(x[2]))
    diffed_series <- lapply(time_series, function(x) diff(log(x)))
  } else {
    starting_values <- sapply(time_series, function(x) x[2])
    diffed_series <- lapply(time_series, function(x) diff(x))
  }
  
  var_data <- do.call(cbind, diffed_series)
  # if a pre-trained model is provided
  if (!is.null(model_to_use)){
    fcasts <- predict(model_to_use, n.ahead=horizon)
    time_series_df <- do.call(cbind, time_series)
    fcasts <- apply(rbind(log(time_series_df[nrow(time_series_df),]), fcasts), 2, cumsum)[2,]
    return(fcasts)
  }
  # otherwise train the model
  var_mod <- lineVar(data=var_data,
                     lag=1,
                     include="const",
                     model="VAR",
                     I="level"
  )
  # return a model if that is the desired object
  if (return_model){
    return(var_mod)
  }
  # otherwise return forecasts
  fcasts <- predict(var_mod, n.ahead=horizon)
  time_series_df <- do.call(cbind, time_series)
  if (take_log){
    fcasts <- apply(rbind(log(time_series_df[nrow(time_series_df),]), fcasts), 2, cumsum)[2,]
  } else {
    fcasts <- apply(rbind(time_series_df[nrow(time_series_df),], fcasts), 2, cumsum)[2,]
  }
  return(fcasts)
}

# write a function that simulates a set of time series 
var_simulation <- function(time_series, take_log=TRUE){
  if (take_log){
    starting_values <- matrix(sapply(time_series, function(x) log(x[2])), nrow=1)
    diff_series <- lapply(time_series, function(x) diff(log(x)))
  } else {
    starting_values <- matrix(sapply(time_series, function(x) x[2]), nrow=1)
    diff_series <- lapply(time_series, function(x) diff(x))
  }
  var_data <- do.call(cbind, diff_series)
  var_mod <- lineVar(data=var_data,
                     lag=1,
                     include="const",
                     model="VAR",
                     I="level"
  )
  coefs <- var_mod$coefficients
  # we used one lag, so simulate one value using the true initial data
  # point
  
  # make three dimensional array to average the simulated values
  nsim <- 5000
  sim1 <- array(data=NA, dim=c(3, ncol(var_data), nsim))
  
  for (i in 1:nsim){
    sim1[,,i] <- VAR.sim(B=coefs,
                         n=2,
                         lag=1,
                         include="const",
                         starting=matrix(var_data[1,], nrow=1),
                         varcov=cov(var_mod$residuals),
                         returnStarting=TRUE)
  }
  
  # average simulated values across simulations
  # take the first simulated value (second one returned) to use
  # in next step of simulation
  sim2_start <- apply(sim1, c(1,2), mean)[2:3,]
  
  sim2 <- array(data=NA, dim=c(nrow(var_data)-2, ncol(var_data), nsim))
  
  for (i in 1:nsim){
    sim2[,,i] <- VAR.sim(B=coefs,
                         n=nrow(var_data)-3,
                         lag=1,
                         include="const",
                         starting=matrix(sim2_start[2,], nrow=1),
                         varcov=cov(var_mod$residuals),
                         returnStarting=TRUE)
  }
  
  sim2 <- apply(sim2, c(1,2), mean)
  
  # first simulated value owned by forecaster
  starting_values <- apply(rbind(starting_values, sim2_start[1,]), 2, cumsum)[2,]
  
  simmed_series <- apply(rbind(starting_values, sim2), 2, cumsum)
  
  simmed_series <- lapply(1:ncol(simmed_series), function(x) unname(simmed_series[,x]))
  
  return(simmed_series)
  
}

# write a function that accepts a set of time series, splits those series into
# groups of 5 and returns VAR forecasts for those series

split_and_var_forecast <- function(time_series, horizon, take_log=TRUE, return_model=FALSE){
  n <- length(time_series)
  k <- 5
  split_series <- split(time_series, rep(1:ceiling(n/k), each=k)[1:n])
  if (return_model){
    return(lapply(split_series, function(x) var_forecast(x, take_log=take_log, horizon=horizon, return_model=return_model)))
  } else {
    return(unname(unlist(lapply(split_series, function(x) var_forecast(x, take_log=take_log, horizon=horizon, return_model=return_model)))))
  }
}

split_and_var_simulate <- function(time_series, take_log=TRUE){
  n <- length(time_series)
  k <- 5
  split_series <- split(time_series, rep(1:ceiling(n/k), each=k)[1:n])
  simulated <- lapply(split_series, function(x) var_simulation(x, take_log=take_log))
  return(unlist(simulated, recursive=FALSE))
}

forecast_for_simulated <- function(simulated_series, var_model, horizon, take_log=TRUE){
  if (take_log){
    diffed_series <- lapply(simulated_series, function(x) diff(log(x)))
  } else {
    diffed_series <- lapply(simulated_series, function(x) diff(x))
  }
  var_data <- do.call(cbind, diffed_series)
  fcasts <- predict(var_model, newdata=matrix(var_data[nrow(var_data),], nrow=1), n.ahead=horizon)
  time_series_df <- do.call(cbind, simulated_series)
  if (take_log){
    fcasts <- exp(apply(rbind(log(time_series_df[nrow(time_series_df),]), fcasts), 2, cumsum)[2,])
  } else {
    fcasts <- apply(rbind(time_series_df[nrow(time_series_df),], fcasts), 2, cumsum)[2,]
  }
  return(fcasts)
}

split_and_forecast_for_simulated <- function(simulated_series, var_models, horizon, take_log=TRUE){
  n <- length(simulated_series)
  k <- 5
  split_series <- split(simulated_series, rep(1:ceiling(n/k), each=k)[1:n])
  fcasts <- lapply(1:length(split_series), function(x) forecast_for_simulated(simulated_series=split_series[[x]],
                                                                              var_model=var_models[[x]],
                                                                              horizon=1,
                                                                              take_log=take_log))
  return(unname(unlist(fcasts, recursive=FALSE)))
}

# evaluate privacy of forecasts

forecast_privacy <- function(true_data, forecasts){
  forecast_matrix <- matrix(rep(forecasts, length(forecasts)), ncol=length(forecasts), byrow=TRUE)
  
  diff_matrix <- apply(forecast_matrix, 2, function(x) pmf(1/(abs(x-true_data)+.Machine$double.eps)))
  
  props_identified <- mean(apply(diff_matrix, 2, which.max) == (1:length(true_data)))
  
  return(props_identified)
}

################################################################################

# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# number of time series to simulate
num_series <- 10

# target feature values
target_spectral_entropy <- runif(n=num_series, min=0.3, max=0.4)
target_acf1 <- runif(n=num_series, min=0.7, max=0.9)
target_acf2 <- c()
for(i in seq_along(target_acf1)){
  target_acf2[i] <- runif(n=1, min=target_acf1[i]-0.2, max=target_acf1[i]-0.1)
}
means <- runif(n=num_series, min=2000, max=12000)
stdevs <- runif(n=num_series, min=50, max=500)

# simulate series with target feature values
g_simulated_series <- tibble()

for (i in 1:num_series){
  g_simulated_series <- g_simulated_series %>%
    bind_rows(generate_target(
    nseries = 1,
    length = 50, 
    feature_function = my_features, 
    target = c(target_spectral_entropy[i], target_acf1[i], target_acf2[i]),
    tolerance = 0.05) %>%
    as_tibble() %>%
    mutate(Series=i) %>%
    mutate(value = value*stdevs[i] + means[i]) %>%
    mutate(value = ifelse(rep(min(value) < 1, n()), value + abs(min(value)) + 1, value))) 
}

###########################################################################################

# repeat for series with undesirable features

target_spectral_entropy <- runif(n=num_series, min=0.6, max=0.7)
target_acf1 <- runif(n=num_series, min=0.2, max=0.4)
target_acf2 <- c()
for(i in seq_along(target_acf1)){
  target_acf2[i] <- runif(n=1, min=target_acf1[i]-0.2, max=target_acf1[i]-0.1)
}
means <- runif(n=num_series, min=2000, max=12000)
stdevs <- runif(n=num_series, min=50, max=500)

b_simulated_series <- tibble()

for (i in 1:num_series){
  b_simulated_series <- b_simulated_series %>%
    bind_rows(generate_target(
      nseries = 1,
      length = 50, 
      feature_function = my_features, 
      target = c(target_spectral_entropy[i], target_acf1[i], target_acf2[i]),
      tolerance = 0.05) %>%
        as_tibble() %>%
        mutate(Series=i) %>%
        mutate(value = value*stdevs[i] + means[i]) %>%
        mutate(value = ifelse(rep(min(value) < 1, n()), value + abs(min(value)) + 1, value))) 
}

################################################################################

# now generate forecasts for each of the sets of time series. Use
# SES, DES. Compare forecast accuracies

# combine and plot simulated series
full_simulated_series <- b_simulated_series %>%
  bind_rows(g_simulated_series) %>%
  mutate(Set = rep(c("Low Autocorrelation/High Spectral Entropy", "High Autocorrelation/Low Spectral Entropy"), each=nrow(b_simulated_series)))

full_plot<- full_simulated_series %>%
  ggplot(aes(x = index, y = value, color = as.factor(Series))) +
  geom_line() +
  facet_wrap(~Set) +
  theme(legend.position='none') +
  labs(x = 'Time',
       y = 'Value') +
  ylim(0, 13000)
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #       text = element_text(size=19),
  #       plot.title = element_text(face= "bold", colour= "black"),
  #       axis.title.x = element_text(face="bold", colour = "black"),    
  #       axis.title.y = element_text(face="bold", colour = "black"))

print(full_plot)

if (file.exists(paste0("../../Outputs/Figures/Simulation/"))){
  ggsave(filename="10_simulated_series.pdf", plot=full_plot, path="../../Outputs/Figures/Simulation/")
} else {
  dir.create(paste0("../../Outputs/Figures/Simulation/"), recursive=TRUE)
  ggsave(filename="10_simulated_series.pdf", plot=full_plot, path="../../Outputs/Figures/Simulation/")
}

######## Forecast using SES, DES, TES

# split into a list of series, convert to ts objects, create train and test data
b_simulated_series <- group_split(b_simulated_series, Series)
b_simulated_series <- lapply(b_simulated_series, function(x) x %>% pull(value))
b_simulated_series <- lapply(b_simulated_series, function(x) ts(x, frequency=1))
b_simulated_series_test <- sapply(b_simulated_series, function(x) x[length(x)])
b_simulated_series_train <- lapply(b_simulated_series, function(x) x[1:(length(x)-1)])

# split into a list of series, convert to ts objects, create train and test data
g_simulated_series <- group_split(g_simulated_series, Series)
g_simulated_series <- lapply(g_simulated_series, function(x) x %>% pull(value))
g_simulated_series <- lapply(g_simulated_series, function(x) ts(x, frequency=1))
g_simulated_series_test <- sapply(g_simulated_series, function(x) x[length(x)])
g_simulated_series_train <- lapply(g_simulated_series, function(x) x[1:(length(x)-1)])

## create vectors of privacy parameters for additive noise and differential privacy
svals <- seq(from=0, to=30.0, length.out=40)
epsvals <- seq(from=20, to=0.01, length.out=39)

# loop over privacy parameters, creating protected versions of the time series
# and saving forecasts for each version

# number of times to simulated noise protection
nsims <- 25

# matrices for storing additive noise forecasts
an_fcasts_ses_b <- array(NA, dim=c(num_series, length(svals), nsims))
an_fcasts_ses_g <- array(NA, dim=c(num_series, length(svals), nsims))
an_fcasts_des_b <- array(NA, dim=c(num_series, length(svals), nsims))
an_fcasts_des_g <- array(NA, dim=c(num_series, length(svals), nsims))

# lists for storing differential privacy forecasts
dp_fcasts_ses_b <- array(NA, dim=c(num_series, length(svals), nsims))
dp_fcasts_ses_g <- array(NA, dim=c(num_series, length(svals), nsims))
dp_fcasts_des_b <- array(NA, dim=c(num_series, length(svals), nsims))
dp_fcasts_des_g <- array(NA, dim=c(num_series, length(svals), nsims))

# for privacy simulation, number of points to sample and number of 
# privacy simulations to conduct
sample_size <- 10
npsims <- 20

# matrices for storing the identification disclosure risk for each 
# simulation and privacy parameter
dp_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))
an_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))

dp_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))
an_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))

for (j in 1:nsims){
  for (i in seq_along(svals)){
    
    if (i == 1){
      dp_fcasts_ses_b[,i,j] <- unname(sapply(lapply(b_simulated_series_train, log), function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_b[,i,j] <- unname(sapply(lapply(b_simulated_series_train, log), function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_fcasts_ses_g[,i,j] <- unname(sapply(lapply(g_simulated_series_train, log), function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_g[,i,j] <- unname(sapply(lapply(g_simulated_series_train, log), function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_idr_b[j,i] <- an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=b_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      dp_idr_g[j,i] <- an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=g_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast differentially private series
      dp_b_simulated <- lapply(b_simulated_series_train, function(x) differential_privacy(x, epsvals[i-1]))
      dp_b_simulated <- lapply(dp_b_simulated, function(x) ifelse(x < 1, 1, x))
      dp_fcasts_ses_b[,i,j] <- unname(sapply(lapply(dp_b_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_b[,i,j] <- unname(sapply(lapply(dp_b_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_g_simulated <- lapply(g_simulated_series_train, function(x) differential_privacy(x, epsvals[i-1]))
      dp_g_simulated <- lapply(dp_g_simulated, function(x) ifelse(x < 1, 1, x))
      dp_fcasts_ses_g[,i,j] <- unname(sapply(lapply(dp_g_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_g[,i,j] <- unname(sapply(lapply(dp_g_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=dp_b_simulated, sample_size=sample_size, num_simulations=npsims)
      dp_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=dp_g_simulated, sample_size=sample_size, num_simulations=npsims)
      
    }
    
    # create and forecast additive noise series
    an_b_simulated <- lapply(b_simulated_series_train, function(x) additive_noise(x, svals[i]))
    an_b_simulated <- lapply(an_b_simulated, function(x) ifelse(x < 1, 1, x))
    an_fcasts_ses_b[,i,j] <- unname(sapply(lapply(an_b_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
    an_fcasts_des_b[,i,j] <- unname(sapply(lapply(an_b_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    an_g_simulated <- lapply(g_simulated_series_train, function(x) additive_noise(x, svals[i]))
    an_g_simulated <- lapply(an_g_simulated, function(x) ifelse(x < 1, 1, x))
    an_fcasts_ses_g[,i,j] <- unname(sapply(lapply(an_g_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
    an_fcasts_des_g[,i,j] <- unname(sapply(lapply(an_g_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=an_b_simulated, sample_size=sample_size, num_simulations=npsims)
    an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=an_g_simulated, sample_size=sample_size, num_simulations=npsims)
    
  }
}

## perform forecasting and feature extraction for k-nTS+
## use the differentially private data sets as a baseline
## for how data protection affects time series features

features_ses_g <- tibble()
features_des_g <- tibble()
features_ses_b <- tibble()
features_des_b <- tibble()

b_simulated_series_test_2 <- sapply(b_simulated_series_train, function(x) x[length(x)])
b_simulated_series_train_2 <- lapply(b_simulated_series_train, function(x) x[1:(length(x)-1)])

g_simulated_series_test_2 <- sapply(g_simulated_series_train, function(x) x[length(x)])
g_simulated_series_train_2 <- lapply(g_simulated_series_train, function(x) x[1:(length(x)-1)])

for (i in seq_along(svals)){
  
  if (i == 1){
    
    # extract features
    temp_features_b <- tsfeatures(tslist=lapply(b_simulated_series_train_2, log), features=fv, scale=FALSE)
    temp_features_g <- tsfeatures(tslist=lapply(g_simulated_series_train_2, log), features=fv, scale=FALSE)
    
    # compute forecasts for h2
    ses_fcasts_b2 <- unname(sapply(lapply(b_simulated_series_train_2, log), function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_b2 <- unname(sapply(lapply(b_simulated_series_train_2, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    ses_fcasts_g2 <- unname(sapply(lapply(g_simulated_series_train_2, log), function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_g2 <- unname(sapply(lapply(g_simulated_series_train_2, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    ses_mae_b2 <- abs(exp(ses_fcasts_b2) - b_simulated_series_test_2)
    des_mae_b2 <- abs(exp(des_fcasts_b2) - b_simulated_series_test_2)
    
    ses_mae_g2 <- abs(exp(ses_fcasts_g2) - g_simulated_series_test_2)
    des_mae_g2 <- abs(exp(des_fcasts_g2) - g_simulated_series_test_2)
    
    features_ses_b <- features_ses_b %>% bind_rows(bind_cols("values"=ses_mae_b2, temp_features_b))
    features_des_b <- features_des_b %>% bind_rows(bind_cols("values"=des_mae_b2, temp_features_b))
    
    features_ses_g <- features_ses_g %>% bind_rows(bind_cols("values"=ses_mae_g2, temp_features_g))
    features_des_g <- features_des_g %>% bind_rows(bind_cols("values"=des_mae_g2, temp_features_g))
    
  } else {
    
    # create and forecast differentially private series
    dp_b_simulated <- lapply(b_simulated_series_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    dp_g_simulated <- lapply(g_simulated_series_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    
    # ensure all values are at least 1
    dp_b_simulated <- lapply(dp_b_simulated, function(x) ifelse(x < 1, 1, x))
    dp_g_simulated <- lapply(dp_g_simulated, function(x) ifelse(x < 1, 1, x))
    
    # extract features
    temp_features_b <- tsfeatures(tslist=lapply(dp_b_simulated, log), features=fv, scale=FALSE)
    temp_features_g <- tsfeatures(tslist=lapply(dp_g_simulated, log), features=fv, scale=FALSE)
    
    # compute forecasts for h2
    ses_fcasts_b2 <- unname(sapply(lapply(dp_b_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_b2 <- unname(sapply(lapply(dp_b_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    ses_fcasts_g2 <- unname(sapply(lapply(dp_g_simulated, log), function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_g2 <- unname(sapply(lapply(dp_g_simulated, log), function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    ses_mae_b2 <- abs(exp(ses_fcasts_b2) - b_simulated_series_test_2)
    des_mae_b2 <- abs(exp(des_fcasts_b2) - b_simulated_series_test_2)
    
    ses_mae_g2 <- abs(exp(ses_fcasts_g2) - g_simulated_series_test_2)
    des_mae_g2 <- abs(exp(des_fcasts_g2) - g_simulated_series_test_2)
    
    features_ses_b <- features_ses_b %>% bind_rows(bind_cols("values"=ses_mae_b2, temp_features_b))
    features_des_b <- features_des_b %>% bind_rows(bind_cols("values"=des_mae_b2, temp_features_b))
    
    features_ses_g <- features_ses_g %>% bind_rows(bind_cols("values"=ses_mae_g2, temp_features_g))
    features_des_g <- features_des_g %>% bind_rows(bind_cols("values"=des_mae_g2, temp_features_g))
    
  }
}

# remove nperiods and seasonal_period features
features_ses_b <- features_ses_b %>% select(-nperiods, -seasonal_period)
features_des_b <- features_des_b %>% select(-nperiods, -seasonal_period)
features_ses_g <- features_ses_g %>% select(-nperiods, -seasonal_period)
features_des_g <- features_des_g %>% select(-nperiods, -seasonal_period)

features_b <- list(features_ses_b, features_des_b)
features_g <- list(features_ses_g, features_des_g)

features_b <- lapply(features_b, function(x) as.data.frame(scale(x)))
features_g <- lapply(features_g, function(x) as.data.frame(scale(x)))

rfe_iters <- 25

fs_b <- feature_selection(scaled_feature_data=features_b, num_rfe_iters=rfe_iters, models=c("SES", "DES"))
fs_g <- feature_selection(scaled_feature_data=features_g, num_rfe_iters=rfe_iters, models=c("SES", "DES"))

# now perform swapping using the selected features for the series with 'good'
# and 'bad' features

kvals <- 2:9

# matrices for storing knts+ forecasts
knts_fcasts_ses_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
knts_fcasts_ses_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
knts_fcasts_des_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
knts_fcasts_des_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
knts_fcasts_var_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
knts_fcasts_var_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))

knts_idr_b <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)
knts_idr_g <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)

sp <- 1

# create nsims knts+ swapped data sets
for (j in 1:nsims){
  for (i in 1:(length(kvals)+1)){
    
    if (i == 1){
      knts_fcasts_ses_b[,i,j] <- unname(sapply(lapply(b_simulated_series_train, log), function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_b[,i,j] <- unname(sapply(lapply(b_simulated_series_train, log), function(x) as.vector(holt(x, h=1)$mean)))
      knts_fcasts_var_b[,i,j] <- split_and_var_forecast(b_simulated_series_train, 1)
      
      knts_fcasts_ses_g[,i,j] <- unname(sapply(lapply(g_simulated_series_train, log), function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_g[,i,j] <- unname(sapply(lapply(g_simulated_series_train, log), function(x) as.vector(holt(x, h=1)$mean)))
      knts_fcasts_var_g[,i,j] <- split_and_var_forecast(g_simulated_series_train, 1)
      
      knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=b_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=g_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast differentially private series
      knts_b_simulated <- as.list(as.data.frame(knts_alg(time_series=lapply(b_simulated_series_train, log), sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=fs_b$selected_features, importance_weights=fs_b$importance_weights)))
      knts_b_simulated <- lapply(knts_b_simulated, function(x) ts(x, frequency=sp))
      knts_fcasts_ses_b[,i,j] <- unname(sapply(knts_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_b[,i,j] <- unname(sapply(knts_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      knts_fcasts_var_b[,i,j] <- split_and_var_forecast(lapply(knts_b_simulated, exp), 1)
      
      knts_g_simulated <- as.list(as.data.frame(knts_alg(time_series=lapply(g_simulated_series_train, log), sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=fs_g$selected_features, importance_weights=fs_g$importance_weights)))
      knts_g_simulated <- lapply(knts_g_simulated, function(x) ts(x, frequency=sp))
      knts_fcasts_ses_g[,i,j] <- unname(sapply(knts_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_g[,i,j] <- unname(sapply(knts_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      knts_fcasts_var_g[,i,j] <- split_and_var_forecast(lapply(knts_g_simulated, exp), 1)
      
      knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=lapply(knts_b_simulated, exp), sample_size=sample_size, num_simulations=npsims)
      knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=lapply(knts_g_simulated, exp), sample_size=sample_size, num_simulations=npsims)
      
    }
  }
}

# reverse log transformation on forecasts
an_fcasts_ses_b <- exp(an_fcasts_ses_b)
an_fcasts_ses_g <- exp(an_fcasts_ses_g)
an_fcasts_des_b <- exp(an_fcasts_des_b)
an_fcasts_des_g <- exp(an_fcasts_des_g)

dp_fcasts_ses_b <- exp(dp_fcasts_ses_b)
dp_fcasts_ses_g <- exp(dp_fcasts_ses_g)
dp_fcasts_des_b <- exp(dp_fcasts_des_b)
dp_fcasts_des_g <- exp(dp_fcasts_des_g)

knts_fcasts_ses_b <- exp(knts_fcasts_ses_b)
knts_fcasts_ses_g <- exp(knts_fcasts_ses_g)
knts_fcasts_des_b <- exp(knts_fcasts_des_b)
knts_fcasts_des_g <- exp(knts_fcasts_des_g)

# calculate MAE
dp_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(dp_fcasts_ses_b[,,n]), function(x) mean(abs(dp_fcasts_ses_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
dp_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(dp_fcasts_ses_g[,,n]), function(x) mean(abs(dp_fcasts_ses_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

dp_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(dp_fcasts_des_b[,,n]), function(x) mean(abs(dp_fcasts_des_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
dp_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(dp_fcasts_des_g[,,n]), function(x) mean(abs(dp_fcasts_des_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

an_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(an_fcasts_ses_b[,,n]), function(x) mean(abs(an_fcasts_ses_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
an_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(an_fcasts_ses_g[,,n]), function(x) mean(abs(an_fcasts_ses_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

an_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(an_fcasts_des_b[,,n]), function(x) mean(abs(an_fcasts_des_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
an_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(an_fcasts_des_g[,,n]), function(x) mean(abs(an_fcasts_des_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

knts_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_ses_b[,,n]), function(x) mean(abs(knts_fcasts_ses_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
knts_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_ses_g[,,n]), function(x) mean(abs(knts_fcasts_ses_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

knts_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_des_b[,,n]), function(x) mean(abs(knts_fcasts_des_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
knts_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_des_g[,,n]), function(x) mean(abs(knts_fcasts_des_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

knts_mae_var_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_var_b[,,n]), function(x) mean(abs(knts_fcasts_var_b[,,n][,x] - b_simulated_series_test))))), 2, mean)
knts_mae_var_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(knts_fcasts_var_g[,,n]), function(x) mean(abs(knts_fcasts_var_g[,,n][,x] - g_simulated_series_test))))), 2, mean)

# calculate P-bar
dp_pbar_b <- apply(dp_idr_b, 2, mean)
dp_pbar_g <- apply(dp_idr_g, 2, mean)

an_pbar_b <- apply(an_idr_b, 2, mean)
an_pbar_g <- apply(an_idr_g, 2, mean)

knts_pbar_b <- apply(knts_idr_b, 2, mean)
knts_pbar_g <- apply(knts_idr_g, 2, mean)

# store results
results <- tibble(
  MAE = c(unlist(dp_mae_ses_b),
          unlist(dp_mae_ses_g),
          unlist(an_mae_ses_b),
          unlist(an_mae_ses_g),
          unlist(dp_mae_des_b),
          unlist(dp_mae_des_g),
          unlist(an_mae_des_b),
          unlist(an_mae_des_g)),
  Pbar = c(dp_pbar_b,
           dp_pbar_g,
           an_pbar_b,
           an_pbar_g,
           dp_pbar_b,
           dp_pbar_g,
           an_pbar_b,
           an_pbar_g),
  Model = rep(c("SES", "DES"), each=length(dp_mae_ses_b)*4),
  Method = rep(c("DP", "AN", "DP", "AN"), each=length(dp_mae_ses_b)*2),
  Parameter = c(c(20.5, epsvals), c(20.5, epsvals), svals, svals, c(20.5, epsvals), c(20.5, epsvals), svals, svals),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(dp_mae_ses_b)), 4))

knts_results <- tibble(
  MAE = c(unlist(knts_mae_ses_b),
          unlist(knts_mae_ses_g),
          unlist(knts_mae_des_b),
          unlist(knts_mae_des_g),
          unlist(knts_mae_var_b),
          unlist(knts_mae_var_g)),
  Pbar = c(knts_pbar_b,
           knts_pbar_g,
           knts_pbar_b,
           knts_pbar_g,
           knts_pbar_b,
           knts_pbar_g),
  Model = rep(c("SES", "DES", "VAR"), each=length(knts_mae_ses_b)*2),
  Method = "k-nTS+",
  Parameter = rep(c(1, kvals), 6),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(knts_mae_ses_b)), 3)
)

results <- results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

knts_results <- knts_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

avg_results <- results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

avg_knts_results <- knts_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

# what is the forecast accuracy under the privacy parameter that 
# achieves acceptable privacy?

if (any(an_pbar_g <= 0.09)){
  best_an_param_g <- sort(svals[an_pbar_g <= 0.09], decreasing=FALSE)[1]
} else {
  best_an_param_g <- svals[length(svals)]
}

if (any(an_pbar_b <= 0.09)){
  best_an_param_b <- sort(svals[an_pbar_b <= 0.09], decreasing=FALSE)[1]
} else {
  best_an_param_b <- svals[length(svals)]
}

if (any(dp_pbar_g <= 0.09)){
  best_dp_param_g <- sort(c(20.5, epsvals)[dp_pbar_g <= 0.09], decreasing=TRUE)[1]
} else {
  best_dp_param_g <- c(20.5, epsvals)[length(c(20.5, epsvals))]
}

if (any(dp_pbar_b <= 0.09)){
  best_dp_param_b <- sort(c(20.5, epsvals)[dp_pbar_b <= 0.09], decreasing=TRUE)[1]
} else {
  best_dp_param_b <- c(20.5, epsvals)[length(c(20.5, epsvals))]
}

if (any(knts_pbar_g <= 0.09)){
  best_knts_param_g <- sort(c(1, kvals)[knts_pbar_g <= 0.09], decreasing=FALSE)[1]
} else {
  best_knts_param_g <- c(1, kvals)[length(c(1, kvals))]
}

if (any(knts_pbar_b <= 0.09)){
  best_knts_param_b <- sort(c(1, kvals)[knts_pbar_b <= 0.09], decreasing=FALSE)[1]
} else {
  best_knts_param_b <- c(1, kvals)[length(c(1, kvals))]
}

# privacy parameters that offer the best accuracy with 9% identification
# risk or less
best_an_param_g
best_an_param_b

best_dp_param_g
best_dp_param_b

best_knts_param_g
best_knts_param_b

# identification disclosure risks corresponding to parameters above
an_pbar_g[svals == best_an_param_g]
an_pbar_b[svals == best_an_param_b]

dp_pbar_g[c(20.5, epsvals) == best_dp_param_g]
dp_pbar_b[c(20.5, epsvals) == best_dp_param_b]

knts_pbar_g[c(1, kvals) == best_knts_param_g]
knts_pbar_b[c(1, kvals) == best_knts_param_b]

# percent increases in average MAE corresponding to parameters above
an_avg_mae_g <- rowMeans(cbind(an_mae_ses_g, an_mae_des_g))
an_pct_mae_g <- (an_avg_mae_g - an_avg_mae_g[1])/an_avg_mae_g[1] * 100
an_pct_mae_g[svals == best_an_param_g]

an_avg_mae_b <- rowMeans(cbind(an_mae_ses_b, an_mae_des_b))
an_pct_mae_b <- (an_avg_mae_b - an_avg_mae_b[1])/an_avg_mae_b[1] * 100
an_pct_mae_b[svals == best_an_param_b]

dp_avg_mae_g <- rowMeans(cbind(dp_mae_ses_g, dp_mae_des_g))
dp_pct_mae_g <- (dp_avg_mae_g - dp_avg_mae_g[1])/dp_avg_mae_g[1] * 100
dp_pct_mae_g[c(20.5, epsvals) == best_dp_param_g]

dp_avg_mae_b <- rowMeans(cbind(dp_mae_ses_b, dp_mae_des_b))
dp_pct_mae_b <- (dp_avg_mae_b - dp_avg_mae_b[1])/dp_avg_mae_b[1] * 100
dp_pct_mae_b[c(20.5, epsvals) == best_dp_param_b]

knts_avg_mae_g <- rowMeans(cbind(knts_mae_ses_g, knts_mae_des_g))
knts_pct_mae_g <- (knts_avg_mae_g - knts_avg_mae_g[1])/knts_avg_mae_g[1] * 100
knts_pct_mae_g[c(1, kvals) == best_knts_param_g]

knts_avg_mae_b <- rowMeans(cbind(knts_mae_ses_b, knts_mae_des_b))
knts_pct_mae_b <- (knts_avg_mae_b - knts_avg_mae_b[1])/knts_avg_mae_b[1] * 100
knts_pct_mae_b[c(1, kvals) == best_knts_param_b]

################################################################################
################################################################################
################################################################################

# VAR simulated series

# generate simulated time series for the good and bad series
b_var_simulated <- unname(split_and_var_simulate(b_simulated_series_train))
g_var_simulated <- unname(split_and_var_simulate(g_simulated_series_train))

# how many periods to trim from beginning of confidential series
length(b_simulated_series_train[[1]]) - length(b_var_simulated[[1]])

# evaluate privacy for the simulated series.
var_idr_b <-  simulation_results(confidential_data_list=lapply(b_simulated_series_train, function(x) x[3:length(x)]), protected_data_list=b_var_simulated, sample_size=sample_size, num_simulations=npsims)
var_idr_g <-  simulation_results(confidential_data_list=lapply(g_simulated_series_train, function(x) x[3:length(x)]), protected_data_list=g_var_simulated, sample_size=sample_size, num_simulations=npsims)

# forecasts for simulated series

# require fitting a model to the unprotected data then estimating forecasts
# based on the simulated time series
b_var_models <- split_and_var_forecast(b_simulated_series_train, horizon=1, return_model=TRUE)
g_var_models <- split_and_var_forecast(g_simulated_series_train, horizon=1, return_model=TRUE)

b_sim_fcasts <- split_and_forecast_for_simulated(simulated_series=b_var_simulated, var_models=b_var_models, horizon=1)
g_sim_fcasts <- split_and_forecast_for_simulated(simulated_series=g_var_simulated, var_models=g_var_models, horizon=1)

b_sim_mae <- mean(abs(b_sim_fcasts - b_simulated_series_test))
g_sim_mae <- mean(abs(g_sim_fcasts - g_simulated_series_test))

# calculate the percentage change in forecast accuracy for the VAR model on
# the simulated and k-nTS+ series

# percent change under var sim
(b_sim_mae - knts_mae_var_b[1])/knts_mae_var_b[1]
(g_sim_mae - knts_mae_var_g[1])/knts_mae_var_g[1]

# percent change under k-nTS+ for VAR
(knts_mae_var_b[best_knts_param_b]-knts_mae_var_b[1])/knts_mae_var_b[1] * 100

(knts_mae_var_g[best_knts_param_g]-knts_mae_var_g[1])/knts_mae_var_g[1] * 100

# evaluate privacy of forecasts
b_var_fcast_privacy <- forecast_privacy(b_simulated_series_test, b_sim_fcasts)

g_var_fcast_privacy <- forecast_privacy(g_simulated_series_test, g_sim_fcasts)

## evaluate forecast privacy from k-nTS+ method for the chosen parameter

# proportion identified from k-nTS+ forecasts
mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_var_b[,best_knts_param_b,x]), function(y) forecast_privacy(b_simulated_series_test, y))))

mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_var_g[,best_knts_param_g,x]), function(y) forecast_privacy(g_simulated_series_test, y))))

## evaluate average privacy of SES and DES forecasts
## for best k-nTS+, AN, and DES parameters

# additive noise undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) an_fcasts_ses_b[,which(svals == best_an_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) an_fcasts_des_b[,which(svals == best_an_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y))))))

# additive noise desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) an_fcasts_ses_g[,which(svals == best_an_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) an_fcasts_des_g[,which(svals == best_an_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y))))))

# differential privacy undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) dp_fcasts_ses_b[,which(c(20.5, epsvals) == best_dp_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) dp_fcasts_des_b[,which(c(20.5, epsvals) == best_dp_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y))))))

# differential privacy desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) dp_fcasts_ses_g[,which(c(20.5, epsvals) == best_dp_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) dp_fcasts_des_g[,which(c(20.5, epsvals) == best_dp_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y))))))

# k-nTS+ undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_ses_b[,which(c(1, kvals) == best_knts_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_des_b[,which(c(1, kvals) == best_knts_param_b),x]), function(y) forecast_privacy(b_simulated_series_test, y))))))

# k-nTS+ desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_ses_g[,which(c(1, kvals) == best_knts_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) knts_fcasts_des_g[,which(c(1, kvals) == best_knts_param_g),x]), function(y) forecast_privacy(g_simulated_series_test, y))))))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# now scale the time series to rates and compare the 
# results in terms of forecast accuracy and privacy

# using (end - beginning)/mean(end, beginning) to convert to a rate

rate_conversion <- function(time_series){
  rate_series <- rep(NA, length(time_series))
  
  for(i in 1:length(time_series)){
    if (i == 1){
      rate_series[i] <- 0
    } else {
      rate_series[i] <- (time_series[i]-time_series[i-1])/mean(c(time_series[i], time_series[i-1]))
    }
  }
  
  rate_series <- ts(rate_series, frequency=frequency(time_series))
  
  return(rate_series)
}

b_simulated_rates <- bind_cols(lapply(lapply(b_simulated_series, log), rate_conversion))
g_simulated_rates <- bind_cols(lapply(lapply(g_simulated_series, log), rate_conversion))

colnames(b_simulated_rates) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
colnames(g_simulated_rates) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# combine rate series into a dataframe for plotting
b_simulated_rates_df <- b_simulated_rates %>%
  mutate(Time = 1:n()) %>%
  gather(key='Series', value="Value", -Time) %>%
  mutate(Series = as.numeric(Series))

g_simulated_rates_df <- g_simulated_rates %>%
  mutate(Time = 1:n()) %>%
  gather(key='Series', value="Value", -Time) %>%
  mutate(Series = as.numeric(Series))

full_simulated_rates_df <- b_simulated_rates_df %>%
  bind_rows(g_simulated_rates_df) %>%
  mutate(Set = rep(c("Low Autocorrelation/High Spectral Entropy", "High Autocorrelation/Low Spectral Entropy"), each=length(b_simulated_series[[1]])*num_series))

full_simulated_rates_df %>%
  ggplot(aes(x = Time, y = Value, color = as.factor(Series))) +
  geom_line() +
  facet_wrap(~Set) +
  theme(legend.position='none')

######## Forecast using SES, DES

# split into a list of series, convert to ts objects, create train and test data
b_simulated_rates_test <- sapply(b_simulated_rates, function(x) x[length(x)])
b_simulated_rates_train <- lapply(b_simulated_rates, function(x) x[1:(length(x)-1)])

# split into a list of series, convert to ts objects, create train and test data
g_simulated_rates_test <- sapply(g_simulated_rates, function(x) x[length(x)])
g_simulated_rates_train <- lapply(g_simulated_rates, function(x) x[1:(length(x)-1)])

# loop over privacy parameters, creating protected versions of the time series
# and saving forecasts for each version

# matrices for storing additive noise forecasts
r_an_fcasts_ses_b <- array(NA, dim=c(num_series, length(svals), nsims))
r_an_fcasts_ses_g <- array(NA, dim=c(num_series, length(svals), nsims))
r_an_fcasts_des_b <- array(NA, dim=c(num_series, length(svals), nsims))
r_an_fcasts_des_g <- array(NA, dim=c(num_series, length(svals), nsims))

# lists for storing differential privacy forecasts
r_dp_fcasts_ses_b <- array(NA, dim=c(num_series, length(svals), nsims))
r_dp_fcasts_ses_g <- array(NA, dim=c(num_series, length(svals), nsims))
r_dp_fcasts_des_b <- array(NA, dim=c(num_series, length(svals), nsims))
r_dp_fcasts_des_g <- array(NA, dim=c(num_series, length(svals), nsims))

r_dp_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))
r_an_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))

r_dp_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))
r_an_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))

for (j in 1:nsims){
  for (i in seq_along(svals)){
    
    if (i == 1){
      r_dp_fcasts_ses_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_dp_fcasts_des_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_dp_fcasts_ses_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_dp_fcasts_des_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_dp_idr_b[j,i] <- r_an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=b_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      r_dp_idr_g[j,i] <- r_an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=g_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast differentially private series
      dp_b_simulated <- lapply(b_simulated_rates_train, function(x) differential_privacy(x, epsvals[i-1]))
      r_dp_fcasts_ses_b[,i,j] <- unname(sapply(dp_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_dp_fcasts_des_b[,i,j] <- unname(sapply(dp_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_g_simulated <- lapply(g_simulated_rates_train, function(x) differential_privacy(x, epsvals[i-1]))
      r_dp_fcasts_ses_g[,i,j] <- unname(sapply(dp_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_dp_fcasts_des_g[,i,j] <- unname(sapply(dp_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_dp_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=dp_b_simulated, sample_size=sample_size, num_simulations=npsims)
      r_dp_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=dp_g_simulated, sample_size=sample_size, num_simulations=npsims)
      
    }
    
    # create and forecast additive noise series
    an_b_simulated <- lapply(b_simulated_rates_train, function(x) additive_noise(x, svals[i]))
    r_an_fcasts_ses_b[,i,j] <- unname(sapply(an_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    r_an_fcasts_des_b[,i,j] <- unname(sapply(an_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    an_g_simulated <- lapply(g_simulated_rates_train, function(x) additive_noise(x, svals[i]))
    r_an_fcasts_ses_g[,i,j] <- unname(sapply(an_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    r_an_fcasts_des_g[,i,j] <- unname(sapply(an_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    r_an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=an_b_simulated, sample_size=sample_size, num_simulations=npsims)
    r_an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=an_g_simulated, sample_size=sample_size, num_simulations=npsims)
    
  }
}

## perform forecasting and feature extraction for k-nTS+

r_features_ses_g <- tibble()
r_features_des_g <- tibble()
r_features_ses_b <- tibble()
r_features_des_b <- tibble()

b_simulated_rates_test_2 <- sapply(b_simulated_rates_train, function(x) x[length(x)])
b_simulated_rates_train_2 <- lapply(b_simulated_rates_train, function(x) x[1:(length(x)-1)])

g_simulated_rates_test_2 <- sapply(g_simulated_rates_train, function(x) x[length(x)])
g_simulated_rates_train_2 <- lapply(g_simulated_rates_train, function(x) x[1:(length(x)-1)])

for (i in seq_along(svals)){
  
  if (i == 1){
    
    # extract features
    r_temp_features_b <- tsfeatures(tslist=b_simulated_rates_train_2, features=fv, scale=FALSE)
    r_temp_features_g <- tsfeatures(tslist=g_simulated_rates_train_2, features=fv, scale=FALSE)
    
    # compute forecasts for h2
    r_ses_fcasts_b2 <- unname(sapply(b_simulated_rates_train_2, function(x) as.vector(ses(x, h=1)$mean)))
    r_des_fcasts_b2 <- unname(sapply(b_simulated_rates_train_2, function(x) as.vector(holt(x, h=1)$mean)))
    
    r_ses_fcasts_g2 <- unname(sapply(g_simulated_rates_train_2, function(x) as.vector(ses(x, h=1)$mean)))
    r_des_fcasts_g2 <- unname(sapply(g_simulated_rates_train_2, function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    r_ses_mae_b2 <- abs(r_ses_fcasts_b2 - b_simulated_rates_test_2)
    r_des_mae_b2 <- abs(r_des_fcasts_b2 - b_simulated_rates_test_2)
    
    r_ses_mae_g2 <- abs(r_ses_fcasts_g2 - g_simulated_rates_test_2)
    r_des_mae_g2 <- abs(r_des_fcasts_g2 - g_simulated_rates_test_2)
    
    r_features_ses_b <- r_features_ses_b %>% bind_rows(bind_cols("values"=r_ses_mae_b2, r_temp_features_b))
    r_features_des_b <- r_features_des_b %>% bind_rows(bind_cols("values"=r_des_mae_b2, r_temp_features_b))
    
    r_features_ses_g <- r_features_ses_g %>% bind_rows(bind_cols("values"=r_ses_mae_g2, r_temp_features_g))
    r_features_des_g <- r_features_des_g %>% bind_rows(bind_cols("values"=r_des_mae_g2, r_temp_features_g))
    
  } else {
    
    # create and forecast differentially private series
    dp_b_simulated_rates <- lapply(b_simulated_rates_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    dp_g_simulated_rates <- lapply(g_simulated_rates_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    
    # extract features
    r_temp_features_b <- tsfeatures(tslist=dp_b_simulated_rates, features=fv, scale=FALSE)
    r_temp_features_g <- tsfeatures(tslist=dp_g_simulated_rates, features=fv, scale=FALSE)
    
    # compute forecasts for h2
    r_ses_fcasts_b2 <- unname(sapply(dp_b_simulated_rates, function(x) as.vector(ses(x, h=1)$mean)))
    r_des_fcasts_b2 <- unname(sapply(dp_b_simulated_rates, function(x) as.vector(holt(x, h=1)$mean)))
    
    r_ses_fcasts_g2 <- unname(sapply(dp_g_simulated_rates, function(x) as.vector(ses(x, h=1)$mean)))
    r_des_fcasts_g2 <- unname(sapply(dp_g_simulated_rates, function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    r_ses_mae_b2 <- abs(r_ses_fcasts_b2 - b_simulated_rates_test_2)
    r_des_mae_b2 <- abs(r_des_fcasts_b2 - b_simulated_rates_test_2)
    
    r_ses_mae_g2 <- abs(r_ses_fcasts_g2 - g_simulated_rates_test_2)
    r_des_mae_g2 <- abs(r_des_fcasts_g2 - g_simulated_rates_test_2)
    
    r_features_ses_b <- r_features_ses_b %>% bind_rows(bind_cols("values"=r_ses_mae_b2, r_temp_features_b))
    r_features_des_b <- r_features_des_b %>% bind_rows(bind_cols("values"=r_des_mae_b2, r_temp_features_b))
    
    r_features_ses_g <- r_features_ses_g %>% bind_rows(bind_cols("values"=r_ses_mae_g2, r_temp_features_g))
    r_features_des_g <- r_features_des_g %>% bind_rows(bind_cols("values"=r_des_mae_g2, r_temp_features_g))
    
  }
}

r_features_ses_b <- r_features_ses_b %>% select(-nperiods, -seasonal_period)
r_features_des_b <- r_features_des_b %>% select(-nperiods, -seasonal_period)
r_features_ses_g <- r_features_ses_g %>% select(-nperiods, -seasonal_period)
r_features_des_g <- r_features_des_g %>% select(-nperiods, -seasonal_period)

r_features_b <- list(r_features_ses_b, r_features_des_b)
r_features_g <- list(r_features_ses_g, r_features_des_g)

r_features_b <- lapply(r_features_b, function(x) as.data.frame(scale(x)))
r_features_g <- lapply(r_features_g, function(x) as.data.frame(scale(x)))

r_fs_b <- feature_selection(scaled_feature_data=r_features_b, num_rfe_iters=rfe_iters, models=c("SES", "DES"))
r_fs_g <- feature_selection(scaled_feature_data=r_features_g, num_rfe_iters=rfe_iters, models=c("SES", "DES"))

# now perform swapping using the selected features for the series with 'good'
# and 'bad' features

# matrices for storing knts+ forecasts
r_knts_fcasts_ses_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
r_knts_fcasts_ses_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
r_knts_fcasts_des_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
r_knts_fcasts_des_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
r_knts_fcasts_var_b <- array(NA, dim=c(num_series, length(kvals)+1, nsims))
r_knts_fcasts_var_g <- array(NA, dim=c(num_series, length(kvals)+1, nsims))

r_knts_idr_b <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)
r_knts_idr_g <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)

sp <- 1

# create nsims knts+ swapped data sets
for (j in 1:nsims){
  for (i in 1:(length(kvals)+1)){
    
    if (i == 1){
      r_knts_fcasts_ses_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))
      r_knts_fcasts_var_b[,i,j] <- split_and_var_forecast(b_simulated_rates_train, horizon=1, take_log=FALSE)
      
      r_knts_fcasts_ses_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))\
      r_knts_fcasts_var_g[,i,j] <- split_and_var_forecast(g_simulated_rates_train, horizon=1, take_log=FALSE)
      
      r_knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=b_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      r_knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=g_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast knts+ private series
      r_knts_b_simulated <- as.list(as.data.frame(knts_alg(time_series=b_simulated_rates_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=r_fs_b$selected_features, importance_weights=r_fs_b$importance_weights)))
      r_knts_b_simulated <- lapply(r_knts_b_simulated, function(x) ts(x, frequency=sp))
      r_knts_fcasts_ses_b[,i,j] <- unname(sapply(r_knts_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_b[,i,j] <- unname(sapply(r_knts_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      r_knts_fcasts_var_b[,i,j] <- split_and_var_forecast(r_knts_b_simulated, horizon=1, take_log=FALSE)
      
      r_knts_g_simulated <- as.list(as.data.frame(knts_alg(time_series=g_simulated_rates_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=r_fs_g$selected_features, importance_weights=r_fs_g$importance_weights)))
      r_knts_g_simulated <- lapply(r_knts_g_simulated, function(x) ts(x, frequency=sp))
      r_knts_fcasts_ses_g[,i,j] <- unname(sapply(r_knts_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_g[,i,j] <- unname(sapply(r_knts_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      r_knts_fcasts_var_g[,i,j] <- split_and_var_forecast(r_knts_g_simulated, horizon=1, take_log=FALSE)
      
      r_knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=r_knts_b_simulated, sample_size=sample_size, num_simulations=npsims)
      r_knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=r_knts_g_simulated, sample_size=sample_size, num_simulations=npsims)
      
    }
  }
}

# calculate MAE
r_dp_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_dp_fcasts_ses_b[,,n]), function(x) mean(abs(r_dp_fcasts_ses_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_dp_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_dp_fcasts_ses_g[,,n]), function(x) mean(abs(r_dp_fcasts_ses_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_dp_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_dp_fcasts_des_b[,,n]), function(x) mean(abs(r_dp_fcasts_des_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_dp_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_dp_fcasts_des_g[,,n]), function(x) mean(abs(r_dp_fcasts_des_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_an_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_an_fcasts_ses_b[,,n]), function(x) mean(abs(r_an_fcasts_ses_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_an_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_an_fcasts_ses_g[,,n]), function(x) mean(abs(r_an_fcasts_ses_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_an_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_an_fcasts_des_b[,,n]), function(x) mean(abs(r_an_fcasts_des_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_an_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_an_fcasts_des_g[,,n]), function(x) mean(abs(r_an_fcasts_des_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_knts_mae_ses_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_ses_b[,,n]), function(x) mean(abs(r_knts_fcasts_ses_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_knts_mae_ses_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_ses_g[,,n]), function(x) mean(abs(r_knts_fcasts_ses_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_knts_mae_des_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_des_b[,,n]), function(x) mean(abs(r_knts_fcasts_des_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_knts_mae_des_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_des_g[,,n]), function(x) mean(abs(r_knts_fcasts_des_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

r_knts_mae_var_b <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_var_b[,,n]), function(x) mean(abs(r_knts_fcasts_var_b[,,n][,x] - b_simulated_rates_test))))), 2, mean)
r_knts_mae_var_g <- apply(do.call(rbind, lapply(1:nsims, function(n) sapply(1:ncol(r_knts_fcasts_var_g[,,n]), function(x) mean(abs(r_knts_fcasts_var_g[,,n][,x] - g_simulated_rates_test))))), 2, mean)

# calculate P-bar
r_dp_pbar_b <- apply(r_dp_idr_b, 2, mean)
r_dp_pbar_g <- apply(r_dp_idr_g, 2, mean)

r_an_pbar_b <- apply(r_an_idr_b, 2, mean)
r_an_pbar_g <- apply(r_an_idr_g, 2, mean)

r_knts_pbar_b <- apply(r_knts_idr_b, 2, mean)
r_knts_pbar_g <- apply(r_knts_idr_g, 2, mean)

# store results
r_results <- tibble(
  MAE = c(unlist(r_dp_mae_ses_b),
          unlist(r_dp_mae_ses_g),
          unlist(r_an_mae_ses_b),
          unlist(r_an_mae_ses_g),
          unlist(r_dp_mae_des_b),
          unlist(r_dp_mae_des_g),
          unlist(r_an_mae_des_b),
          unlist(r_an_mae_des_g)),
  Pbar = c(r_dp_pbar_b,
           r_dp_pbar_g,
           r_an_pbar_b,
           r_an_pbar_g,
           r_dp_pbar_b,
           r_dp_pbar_g,
           r_an_pbar_b,
           r_an_pbar_g),
  Model = rep(c("SES", "DES"), each=length(r_dp_mae_ses_b)*4),
  Method = rep(c("DP", "AN", "DP", "AN"), each=length(r_dp_mae_ses_b)*2),
  Parameter = c(c(20.5, epsvals), c(20.5, epsvals), svals, svals, c(20.5, epsvals), c(20.5, epsvals), svals, svals),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(r_dp_mae_ses_b)), 4))

r_knts_results <- tibble(
  MAE = c(unlist(r_knts_mae_ses_b),
          unlist(r_knts_mae_ses_g),
          unlist(r_knts_mae_des_b),
          unlist(r_knts_mae_des_g),
          unlist(r_knts_mae_var_b),
          unlist(r_knts_mae_var_g)),
  Pbar = c(r_knts_pbar_b,
           r_knts_pbar_g,
           r_knts_pbar_b,
           r_knts_pbar_g,
           r_knts_pbar_b,
           r_knts_pbar_g),
  Model = rep(c("SES", "DES", "VAR"), each=length(r_knts_mae_ses_b)*2),
  Method = "k-nTS+",
  Parameter = rep(c(1, kvals), 6),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(r_knts_mae_ses_b)), 3))

r_results <- r_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

r_knts_results <- r_knts_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

r_avg_results <- r_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

r_knts_avg_results <- r_knts_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

# what is the best privacy parameter that still maintains less than
# or equal to 9% identification disclosure risk

if (any(r_an_pbar_g <= 0.09)){
  r_best_an_param_g <- sort(svals[r_an_pbar_g <= 0.09], decreasing=FALSE)[1]
} else {
  r_best_an_param_g <- svals[length(svals)]
}

if (any(r_an_pbar_b <= 0.09)){
  r_best_an_param_b <- sort(svals[r_an_pbar_b <= 0.09], decreasing=FALSE)[1]
} else {
  r_best_an_param_b <- svals[length(svals)]
}

if (any(r_dp_pbar_g <= 0.09)){
  r_best_dp_param_g <- sort(c(20.5, epsvals)[r_dp_pbar_g <= 0.09], decreasing=TRUE)[1]
} else {
  r_best_dp_param_g <- c(20.5, epsvals)[length(c(20.5, epsvals))]
}

if (any(r_dp_pbar_b <= 0.09)){
  r_best_dp_param_b <- sort(c(20.5, epsvals)[r_dp_pbar_b <= 0.09], decreasing=TRUE)[1]
} else {
  r_best_dp_param_b <- c(20.5, epsvals)[length(c(20.5, epsvals))]
}

if (any(r_knts_pbar_g <= 0.09)){
  r_best_knts_param_g <- sort(c(1, kvals)[r_knts_pbar_g <= 0.09], decreasing=FALSE)[1]
} else {
  r_best_knts_param_g <- c(1, kvals)[length(c(1, kvals))]
}

if (any(r_knts_pbar_b <= 0.09)){
  r_best_knts_param_b <- sort(c(1, kvals)[r_knts_pbar_b <= 0.09], decreasing=FALSE)[1]
} else {
  r_best_knts_param_b <- c(1, kvals)[length(c(1, kvals))]
}

### double check the p-bars manually. additive noise (differential privacy)
# p-bars may converge to random guessing, in which case you can pick the
# smallest (largest) value of s (epsilon) that results in a probability
# equivalent to random guessing

r_best_an_param_g <- svals[11]
r_best_an_param_g
r_best_an_param_b <- svals[8]
r_best_an_param_b

r_best_dp_param_g <- c(20.5, epsvals)[39]
r_best_dp_param_g
r_best_dp_param_b

r_best_knts_param_g
r_best_knts_param_b

# identification disclosure risks corresponding to parameters above
r_an_pbar_g[svals == r_best_an_param_g]
r_an_pbar_b[svals == r_best_an_param_b]

r_dp_pbar_g[c(20.5, epsvals) == r_best_dp_param_g]
r_dp_pbar_b[c(20.5, epsvals) == r_best_dp_param_b]

r_knts_pbar_g[c(1, kvals) == r_best_knts_param_g]
r_knts_pbar_b[c(1, kvals) == r_best_knts_param_b]

# percent increases in average MAE corresponding to parameters above
r_an_avg_mae_g <- rowMeans(cbind(r_an_mae_ses_g, r_an_mae_des_g))
r_an_pct_mae_g <- (r_an_avg_mae_g - r_an_avg_mae_g[1])/r_an_avg_mae_g[1] * 100
r_an_pct_mae_g[svals == r_best_an_param_g]

r_an_avg_mae_b <- rowMeans(cbind(r_an_mae_ses_b, r_an_mae_des_b))
r_an_pct_mae_b <- (r_an_avg_mae_b - r_an_avg_mae_b[1])/r_an_avg_mae_b[1] * 100
r_an_pct_mae_b[svals == r_best_an_param_b]

r_dp_avg_mae_g <- rowMeans(cbind(r_dp_mae_ses_g, r_dp_mae_des_g))
r_dp_pct_mae_g <- (r_dp_avg_mae_g - r_dp_avg_mae_g[1])/r_dp_avg_mae_g[1] * 100
r_dp_pct_mae_g[c(20.5, epsvals) == r_best_dp_param_g]

r_dp_avg_mae_b <- rowMeans(cbind(r_dp_mae_ses_b, r_dp_mae_des_b))
r_dp_pct_mae_b <- (r_dp_avg_mae_b - r_dp_avg_mae_b[1])/r_dp_avg_mae_b[1] * 100
r_dp_pct_mae_b[c(20.5, epsvals) == r_best_dp_param_b]

r_knts_avg_mae_g <- rowMeans(cbind(r_knts_mae_ses_g, r_knts_mae_des_g))
r_knts_pct_mae_g <- (r_knts_avg_mae_g - r_knts_avg_mae_g[1])/r_knts_avg_mae_g[1] * 100
r_knts_pct_mae_g[c(1, kvals) == r_best_knts_param_g]

r_knts_avg_mae_b <- rowMeans(cbind(r_knts_mae_ses_b, r_knts_mae_des_b))
r_knts_pct_mae_b <- (r_knts_avg_mae_b - r_knts_avg_mae_b[1])/r_knts_avg_mae_b[1] * 100
r_knts_pct_mae_b[c(1, kvals) == r_best_knts_param_b]

################################################################################
################################################################################
################################################################################

# VAR simulated series

# generate simulated time series for the good and bad series
r_b_var_simulated <- unname(split_and_var_simulate(b_simulated_rates_train, take_log=FALSE))
r_g_var_simulated <- unname(split_and_var_simulate(g_simulated_rates_train, take_log=FALSE))

# how many periods to trim from beginning of confidential series
length(b_simulated_rates_train[[1]]) - length(r_b_var_simulated[[1]])

# evaluate privacy for the simulated series.
r_var_idr_b <-  simulation_results(confidential_data_list=lapply(b_simulated_rates_train, function(x) x[3:length(x)]), protected_data_list=r_b_var_simulated, sample_size=sample_size, num_simulations=npsims)
r_var_idr_g <-  simulation_results(confidential_data_list=lapply(g_simulated_rates_train, function(x) x[3:length(x)]), protected_data_list=r_g_var_simulated, sample_size=sample_size, num_simulations=npsims)

# calculate the percentage change in forecast accuracy for the VAR model on
# the simulated and k-nTS+ series

# percent change under var sim
(r_b_sim_mae - r_knts_mae_var_b[1])/r_knts_mae_var_b[1]
(r_g_sim_mae - r_knts_mae_var_g[1])/r_knts_mae_var_g[1]

# percent change under k-nTS+ for VAR
(r_knts_mae_var_b[r_best_knts_param_b]-r_knts_mae_var_b[1])/r_knts_mae_var_b[1] * 100

(r_knts_mae_var_g[r_best_knts_param_g]-r_knts_mae_var_g[1])/r_knts_mae_var_g[1] * 100

# evaluate privacy of forecasts

r_b_var_fcast_privacy <- forecast_privacy(b_simulated_rates_test, r_b_sim_fcasts)
r_g_var_fcast_privacy <- forecast_privacy(g_simulated_rates_test, r_g_sim_fcasts)

## evaluate forecast privacy from k-nTS+ method for the chosen parameter

# proportion identified from k-nTS+ forecasts
mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_var_b[,r_best_knts_param_b,x]), function(y) forecast_privacy(b_simulated_rates_test, y))))

mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_var_g[,r_best_knts_param_g,x]), function(y) forecast_privacy(g_simulated_rates_test, y))))

## evaluate average privacy of SES and DES forecasts
## for best k-nTS+, AN, and DES parameters

# additive noise undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_an_fcasts_ses_b[,which(svals == r_best_an_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_an_fcasts_des_b[,which(svals == r_best_an_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y))))))

# additive noise desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_an_fcasts_ses_g[,which(svals == r_best_an_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_an_fcasts_des_g[,which(svals == r_best_an_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y))))))

# differential privacy undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_dp_fcasts_ses_b[,which(c(20.5, epsvals) == r_best_dp_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_dp_fcasts_des_b[,which(c(20.5, epsvals) == r_best_dp_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y))))))

# differential privacy desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_dp_fcasts_ses_g[,which(c(20.5, epsvals) == r_best_dp_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_dp_fcasts_des_g[,which(c(20.5, epsvals) == r_best_dp_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y))))))

# k-nTS+ undesirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_ses_b[,which(c(1, kvals) == r_best_knts_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_des_b[,which(c(1, kvals) == r_best_knts_param_b),x]), function(y) forecast_privacy(b_simulated_rates_test, y))))))

# k-nTS+ desirable series
mean(c(mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_ses_g[,which(c(1, kvals) == r_best_knts_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y)))),
       mean(unlist(lapply(lapply(1:nsims, function(x) r_knts_fcasts_des_g[,which(c(1, kvals) == r_best_knts_param_g),x]), function(y) forecast_privacy(g_simulated_rates_test, y))))))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# an, dp, knts forecast error on original time series space
scales <- list(
  scale_x_continuous(),
  scale_x_reverse(),
  scale_x_continuous(breaks = c(1, kvals))
)

avg_mae_plot <- avg_results %>%
  bind_rows(avg_knts_results) %>%
  filter(Type == "MAE") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  ylim(0, 7000) +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales) + 
  labs(x = "Privacy Parameter",
       y = "Average MAE",
       title = "Average MAE Across Forecasting Models and Protected Data Sets (Original Scale)")

# an, dp, knts identification disclosure on original time series space
avg_pbar_plot <- avg_results %>%
  bind_rows(avg_knts_results) %>%
  filter(Type == "Pbar") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  ylim(-0.01, 1.00) +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales) +
  labs(x = "Privacy Parameter",
       y = "Average Proportion Identified",
       title = "Average Identification Disclosure Across Protected Data Sets (Original Scale)")

curve_plot <- ggarrange(avg_pbar_plot, avg_mae_plot, nrow=2, ncol=1)

print(curve_plot)

if (file.exists(paste0("../../Outputs/Figures/Simulation/"))){
  ggsave(filename="idr-mae-10-orig.pdf", plot=curve_plot, path="../../Outputs/Figures/Simulation/")
} else {
  dir.create(paste0("../../Outputs/Figures/Simulation/"), recursive=TRUE)
  ggsave(filename="idr-mae-10-orig.pdf", plot=curve_plot, path="../../Outputs/Figures/Simulation/")
}

# an, dp, knts on rate time series space

r_avg_mae_plot <- r_avg_results %>%
  bind_rows(r_knts_avg_results) %>%
  filter(Type == "MAE") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  ylim(0, 0.03) +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales) + 
  labs(x = "Privacy Parameter",
       y = "Average MAE",
       title = "Average MAE Across Forecasting Models and Protected Data Sets (Rate Scale)")

# an, dp, knts identification disclosure on original time series space
r_avg_pbar_plot <- r_avg_results %>%
  bind_rows(r_knts_avg_results) %>%
  filter(Type == "Pbar") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  ylim(-0.01, 1.00) +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales) +
  labs(x = "Privacy Parameter",
       y = "Average Proportion Identified",
       title = "Average Identification Disclosure Across Protected Data Sets (Rate Scale)")

ggarrange(r_avg_pbar_plot, r_avg_mae_plot, nrow=2, ncol=1)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# how much has forecast accuracy decreased for each data set at the strongest
# privacy parameter?
original_avg_mae <- avg_results %>%
  filter(Parameter == 0, Type == "MAE") %>%
  pull(Average_Value)

pct_change_desirable <- avg_results %>% 
  filter(Type == "MAE", Set == "Desirable") %>%
  mutate(Desirable_Original_Avg_MAE = original_avg_mae[1],
         Pct_Change = (Average_Value - Desirable_Original_Avg_MAE)/Desirable_Original_Avg_MAE * 100)

pct_change_undesirable <- avg_results %>% 
  filter(Type == "MAE", Set == "Undesirable") %>%
  mutate(Undesirable_Original_Avg_MAE = original_avg_mae[2],
         Pct_Change = (Average_Value - Undesirable_Original_Avg_MAE)/Undesirable_Original_Avg_MAE * 100)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
