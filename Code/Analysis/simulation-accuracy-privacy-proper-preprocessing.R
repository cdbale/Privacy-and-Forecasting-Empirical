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
  # calculate the:
  # - first coefficient of ACF
  # - slope of time series
  # 
  c(entropy = entropy(y),
    acf = acf(y, plot = FALSE)$acf[2:3, 1, 1])
    # slope = unname(lm(y~ seq(1, length(y)))[['coefficients']][2]))
}

additive_noise <- function(time_series, s){
  noise_vals <- rnorm(n=length(time_series), sd=s*sd(time_series))
  return(time_series + noise_vals)
}

differential_privacy <- function(time_series, epsilon, original=FALSE){

  # calculate global sensitivity
  gs = max(time_series) - min(time_series)
  
  # add random noise sampled from 0-centered laplace
  # distribution with scale parameter = GS/epsilon
  noise_vals <- rLaplace(n=length(time_series), mu=0, b=gs/epsilon)
  
  return(time_series + noise_vals)
}

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
    mutate(min_error = min(value),
           within_5p = ifelse((value-min_error)/min_error <= 0.05, 1, 0)) %>%
    ungroup() %>%
    filter(within_5p == 1) %>%
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
  
  rfe_stop <- Sys.time()
  
  return(list("rank_df" = rank_df, 
              "combined_oob" = combined_oob, 
              "evals_combined" = evals_combined,
              "selected_features" = sf,
              "relief_time" = difftime(relief_stop, relief_start, units="mins"),
              "rfe_time" = difftime(rfe_stop, rfe_start, units="mins"),
              "rf" = rf_res))
}

is_seasonal <- function(time_series, sp){
  
  if ((sp > 1) & (length(time_series) >= 3*sp)){
    
    acfs <- acf(time_series, lag.max=sp, plot=FALSE, type="correlation")[['acf']][,,1]
    
    test_result <- abs(acfs[sp+1]) > (1.645*sqrt((1+2*sum(acfs[1:sp]^2))/length(time_series)))
  } else {
    test_result <- FALSE
  }
  return(test_result)
}

knts_alg <- function(time_series, type_of_series, sp, window_length, k, features_to_calculate, selected_features, corr_based=FALSE){
  
  # note that we won't need to consider any seasonal features for swapping
  
  # and these can be removed from the RFE as well
  
  # for each series
  
  # select the point immediately prior to the forecast origin
  # for scaling the series
  # scalers <- sapply(time_series, function(x) x[length(x)-1])
  
  adjusted_series <- list()
  seasonal_components <- list()
  
  for (s in seq_along(time_series)){
    
    # perform a seasonality test
    seasonal_test <- is_seasonal(time_series[[s]], sp)
    
    # if seasonal,
    if (seasonal_test){
      
      # apply box-cox transformation
      lmda <- BoxCox.lambda(time_series[[s]], method='guerrero')
      transformed <- BoxCox(time_series[[s]], lambda=lmda)
      
      # STL decomposition
      decomposed <- stl(transformed, s.window="periodic")[['time.series']]
      
      # inverse box-cox on trend + remainder component 
      sadj <- InvBoxCox(decomposed[,2] + decomposed[,3], lambda=lmda)
      
      # keep seasonal component to add back in at the end
      seasonal_components[[s]] <- decomposed[,1]
      
    } else {
      
      sadj <- time_series[[s]]
      
      seasonal_components[[s]] <- ts(rep(0, length(sadj)), frequency=sp)
      
    }
    
    # # smooth the seasonally adjusted series using Loess
    # alpha <- ifelse(type_of_series=="monthly", 0.10, 0.25)
    # treg <- 1:length(sadj)
    # smoothed_model <- loess(sadj ~ treg, span=alpha)
    # smoothed_sadj <- ts(predict(smoothed_model), frequency=sp)
    
    adjusted_series[[s]] <- sadj
  }
  
  # number of time series
  num_series <- length(time_series)
  
  # number of time periods
  num_periods <- length(time_series[[1]])
  
  # matrix to hold new series
  X_new <- matrix(0.0, nrow=num_periods, ncol=num_series)
  
  # restrict the data to the beginning window
  X_window <- lapply(adjusted_series, function(x) ts(x[1:window_length], frequency=sp))
  
  # extract scaling points - the time period prior to forecast origin
  scalers <- sapply(X_window, function(x) x[length(x)-1])
  
  # scale series
  scaled_X_window <- lapply(1:length(X_window), function(x) X_window[[x]]/scalers[x])
  
  # if (corr_based){
  #   X_cor <- cor(do.call(cbind, X_window))
  # }
  
  # calculate the features for the current window
  C <- tsfeatures(scaled_X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
  
  # normalize features
  # C <- as.data.frame(scale(C))
  
  # convert C to a c x J matrix (num features by num series)
  C <- t(C)
  
  ## Calculate the feature distance matrix D
  ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
  D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
  
  # for each time period in the initial window
  for (j in 1:num_series){
    
    # select the jth column
    d <- D[,j]
    
    # sort the distances in the jth column smallest to largest
    sorted <- sort(d, index.return=TRUE)
    
    # select from index 2 to k+1 since first index corresponds to the series itself
    K <- sorted$ix[2:(k+1)]
    
    # if (corr_based){
    #   #############################
    #   
    #   # obtain the correlations corresponding to the K indexes
    #   cors <- X_cor[,j][K]
    #   
    #   swap_weights <- exp(cors)/sum(exp(cors))
    #   
    #   #############################
    # }
    
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
      X_new[t,j] <- scaled_X_window[[i]][t] * scalers[j]
      
    }
  }
  
  ########################################
  ### Continue swapping for the rest of the time periods using a rolling window approach
  ########################################
  
  for (t in (window_length+1):num_periods){
    
    # restrict the data to the current window
    X_window <- lapply(adjusted_series, function(x) ts(x[(t-window_length+1):t], frequency=sp))
    
    # extract scaling points - the time period prior to forecast origin
    scalers <- sapply(X_window, function(x) x[length(x)-1])
    
    # scale series
    scaled_X_window <- lapply(1:length(X_window), function(x) X_window[[x]]/scalers[x])
    
    ## calculate the features for the current window
    C <- tsfeatures(scaled_X_window, features=features_to_calculate, scale=FALSE)[,selected_features]
    
    # transpose C to a c x J matrix (num features by num series)
    C <- t(C)
    
    ## Calculate the feature distance matrix D
    # ones_column <- as.matrix(rep(1, num_series), nrow=num_series)
    D <- ones_column %*% diag(t(C)%*%C) - 2*t(C)%*%C + diag(t(C)%*%C) %*% t(ones_column)
    
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
      X_new[t,j] <- scaled_X_window[[i]][window_length] * scalers[j]
      
    }
  }
  
  # add seasonal component back
  for (i in 1:ncol(X_new)){
    X_new[,i] + seasonal_components[[i]]
  }
  
  return(X_new)
  
}

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

# set seed for reproducibility

num_series <- 10

target_spectral_entropy <- runif(n=num_series, min=0.3, max=0.4)
target_acf1 <- runif(n=num_series, min=0.7, max=0.9)
target_acf2 <- c()
for(i in seq_along(target_acf1)){
  target_acf2[i] <- runif(n=1, min=target_acf1[i]-0.2, max=target_acf1[i]-0.1)
}
means <- runif(n=num_series, min=2000, max=12000)
stdevs <- runif(n=num_series, min=50, max=500)

g_simulated_series <- tibble()

for (i in 1:num_series){
  g_simulated_series <- g_simulated_series %>%
    bind_rows(generate_target(
    nseries = 1,
    length = 50, 
    feature_function = my_features, 
    target = c(target_spectral_entropy[i], target_acf1[i], target_acf2[i]),
    tolerance = 0.05) %>%
    mutate(Series=i) %>%
    as_tibble() %>%
    mutate(value = value*stdevs[i] + means[i]))
}
# simulate time series with the target features

# g_simulated_series %>%
#   ggplot(aes(x=index, y=value, color=as.factor(Series))) +
#   geom_line()

###########################################################################################

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
        mutate(Series=i) %>%
        as_tibble() %>%
        mutate(value = value*stdevs[i] + means[i]))
}
# simulate time series with the target features

# b_simulated_series %>%
#   ggplot(aes(x=index, y=value, color=as.factor(Series))) +
#   geom_line()

################################################################################

# now generate forecasts for each of the sets of time series. Use
# SES, DES, TES. Compare initial forecast accuracies

# Next Steps:
# - generate forecasts using SES, DES, TES
# - measure MAE for each set of series
# - measure how much noise it takes to reach 25% increase in forecast error
# - - do this using both additive noise (to put it in terms of the standard
#     deviation of the series) and differential privacy to put it in terms
#     of epsilon   
# - measure how privacy changes as a function of the noise - how much noise
#   is required to reach acceptable privacy, and where does this land you
#   on the accuracy curve
# - repeat the above but with the scaled "rate" version of the time series

full_simulated_series <- b_simulated_series %>%
  bind_rows(g_simulated_series) %>%
  mutate(Set = rep(c("Low Autocorrelation/High Spectral Entropy", "High Autocorrelation/Low Spectral Entropy"), each=nrow(b_simulated_series)))

full_simulated_series %>%
  ggplot(aes(x = index, y = value, color = as.factor(Series))) +
  geom_line() +
  facet_wrap(~Set) +
  theme(legend.position='none')
  
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
svals <- seq(from=0, to=3.0, length.out=40)
epsvals <- seq(from=20, to=1, length.out=39)

# loop over privacy parameters, creating protected versions of the time series
# and saving forecasts for each version

# number of times to simulated noise protection
nsims <- 50

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

sample_size <- 10
npsims <- 20

dp_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))
an_idr_g <- matrix(NA, nrow=nsims, ncol=length(svals))

dp_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))
an_idr_b <- matrix(NA, nrow=nsims, ncol=length(svals))

for (j in 1:nsims){
  for (i in seq_along(svals)){
    
    if (i == 1){
      dp_fcasts_ses_b[,i,j] <- unname(sapply(b_simulated_series_train, function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_b[,i,j] <- unname(sapply(b_simulated_series_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_fcasts_ses_g[,i,j] <- unname(sapply(g_simulated_series_train, function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_g[,i,j] <- unname(sapply(g_simulated_series_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_idr_b[j,i] <- an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=b_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      dp_idr_g[j,i] <- an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=g_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast differentially private series
      dp_b_simulated <- lapply(b_simulated_series_train, function(x) differential_privacy(x, epsvals[i-1]))
      dp_fcasts_ses_b[,i,j] <- unname(sapply(dp_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_b[,i,j] <- unname(sapply(dp_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_g_simulated <- lapply(g_simulated_series_train, function(x) differential_privacy(x, epsvals[i-1]))
      dp_fcasts_ses_g[,i,j] <- unname(sapply(dp_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      dp_fcasts_des_g[,i,j] <- unname(sapply(dp_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      dp_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=dp_b_simulated, sample_size=sample_size, num_simulations=npsims)
      dp_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=dp_g_simulated, sample_size=sample_size, num_simulations=npsims)
      
    }
    
    # create and forecast additive noise series
    an_b_simulated <- lapply(b_simulated_series_train, function(x) additive_noise(x, svals[i]))
    an_fcasts_ses_b[,i,j] <- unname(sapply(an_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    an_fcasts_des_b[,i,j] <- unname(sapply(an_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    an_g_simulated <- lapply(g_simulated_series_train, function(x) additive_noise(x, svals[i]))
    an_fcasts_ses_g[,i,j] <- unname(sapply(an_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    an_fcasts_des_g[,i,j] <- unname(sapply(an_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    an_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=an_b_simulated, sample_size=sample_size, num_simulations=npsims)
    an_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=an_g_simulated, sample_size=sample_size, num_simulations=npsims)
    
  }
}

## perform forecasting and feature extraction for k-nTS+

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
    temp_features_b <- tsfeatures(tslist=b_simulated_series_train_2, features=fv, scale=FALSE)
    temp_features_g <- tsfeatures(tslist=g_simulated_series_train_2, features=fv, scale=FALSE)
    
    # compute forecasts for h2
    ses_fcasts_b2 <- unname(sapply(b_simulated_series_train_2, function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_b2 <- unname(sapply(b_simulated_series_train_2, function(x) as.vector(holt(x, h=1)$mean)))
    
    ses_fcasts_g2 <- unname(sapply(g_simulated_series_train_2, function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_g2 <- unname(sapply(g_simulated_series_train_2, function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    ses_mae_b2 <- abs(ses_fcasts_b2 - b_simulated_series_test_2)
    des_mae_b2 <- abs(des_fcasts_b2 - b_simulated_series_test_2)
    
    ses_mae_g2 <- abs(ses_fcasts_g2 - g_simulated_series_test_2)
    des_mae_g2 <- abs(des_fcasts_g2 - g_simulated_series_test_2)
    
    features_ses_b <- features_ses_b %>% bind_rows(bind_cols("values"=ses_mae_b2, temp_features_b))
    features_des_b <- features_des_b %>% bind_rows(bind_cols("values"=des_mae_b2, temp_features_b))
    
    features_ses_g <- features_ses_g %>% bind_rows(bind_cols("values"=ses_mae_g2, temp_features_g))
    features_des_g <- features_des_g %>% bind_rows(bind_cols("values"=des_mae_g2, temp_features_g))
    
  } else {
    
    # create and forecast differentially private series
    dp_b_simulated <- lapply(b_simulated_series_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    dp_g_simulated <- lapply(g_simulated_series_train_2, function(x) differential_privacy(x, epsvals[i-1]))
    
    # extract features
    temp_features_b <- tsfeatures(tslist=dp_b_simulated, features=fv, scale=FALSE)
    temp_features_g <- tsfeatures(tslist=dp_g_simulated, features=fv, scale=FALSE)
    
    # compute forecasts for h2
    ses_fcasts_b2 <- unname(sapply(dp_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_b2 <- unname(sapply(dp_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    ses_fcasts_g2 <- unname(sapply(dp_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
    des_fcasts_g2 <- unname(sapply(dp_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
    
    # compute forecast error (MAE)
    ses_mae_b2 <- abs(ses_fcasts_b2 - b_simulated_series_test_2)
    des_mae_b2 <- abs(des_fcasts_b2 - b_simulated_series_test_2)
    
    ses_mae_g2 <- abs(ses_fcasts_g2 - g_simulated_series_test_2)
    des_mae_g2 <- abs(des_fcasts_g2 - g_simulated_series_test_2)
    
    features_ses_b <- features_ses_b %>% bind_rows(bind_cols("values"=ses_mae_b2, temp_features_b))
    features_des_b <- features_des_b %>% bind_rows(bind_cols("values"=des_mae_b2, temp_features_b))
    
    features_ses_g <- features_ses_g %>% bind_rows(bind_cols("values"=ses_mae_g2, temp_features_g))
    features_des_g <- features_des_g %>% bind_rows(bind_cols("values"=des_mae_g2, temp_features_g))
    
  }
}

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

knts_idr_b <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)
knts_idr_g <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)

sp <- 1

# create nsims knts+ swapped data sets
for (j in 1:nsims){
  for (i in 1:(length(kvals)+1)){
    
    if (i == 1){
      knts_fcasts_ses_b[,i,j] <- unname(sapply(b_simulated_series_train, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_b[,i,j] <- unname(sapply(b_simulated_series_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      knts_fcasts_ses_g[,i,j] <- unname(sapply(g_simulated_series_train, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_g[,i,j] <- unname(sapply(g_simulated_series_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=b_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=g_simulated_series_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast differentially private series
      knts_b_simulated <- as.list(as.data.frame(knts_alg(time_series=b_simulated_series_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=fs_b$selected_features)))
      knts_b_simulated <- lapply(knts_b_simulated, function(x) ts(x, frequency=sp))
      knts_fcasts_ses_b[,i,j] <- unname(sapply(knts_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_b[,i,j] <- unname(sapply(knts_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      knts_g_simulated <- as.list(as.data.frame(knts_alg(time_series=g_simulated_series_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=fs_g$selected_features)))
      knts_g_simulated <- lapply(knts_g_simulated, function(x) ts(x, frequency=sp))
      knts_fcasts_ses_g[,i,j] <- unname(sapply(knts_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      knts_fcasts_des_g[,i,j] <- unname(sapply(knts_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_series_train, protected_data_list=knts_b_simulated, sample_size=sample_size, num_simulations=npsims)
      knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_series_train, protected_data_list=knts_g_simulated, sample_size=sample_size, num_simulations=npsims)
      
    }
  }
}

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

# calculate P-bar
dp_pbar_b <- apply(dp_idr_b, 2, mean)
dp_pbar_g <- apply(dp_idr_g, 2, mean)

an_pbar_b <- apply(an_idr_b, 2, mean)
an_pbar_g <- apply(an_idr_g, 2, mean)

knts_pbar_b <- apply(knts_idr_b, 2, mean)
knts_pbar_g <- apply(knts_idr_g, 2, mean)

# plot MAE curves
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
          unlist(knts_mae_des_g)),
  Pbar = c(knts_pbar_b,
           knts_pbar_g,
           knts_pbar_b,
           knts_pbar_g),
  Model = rep(c("SES", "DES"), each=length(knts_mae_ses_b)*2),
  Method = "k-nTS+",
  Parameter = rep(c(1, kvals), 4),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(knts_mae_ses_b)), 2)
)

results <- results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

knts_results <- knts_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

results %>%
  filter(Method == "AN", Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  facet_wrap(~Set)

results %>%
  filter(Method == "DP", Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~Set)

knts_results %>%
  filter(Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  facet_wrap(~Set)

avg_results <- results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

avg_knts_results <- knts_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

avg_results %>%
  filter(Method == "AN") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Type, scales='free')

avg_results %>%
  filter(Method == "DP") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~Type, scales='free')

avg_knts_results %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Type, scales='free')

# what is the best privacy parameter we can have that still preserves
# forecast accuracy within 25%?

best_an_param_g <- rowMeans(cbind(an_mae_ses_g, an_mae_des_g))
best_an_param_g <- (best_an_param_g - best_an_param_g[1])/best_an_param_g[1] * 100
best_an_param_g <- sort(svals[best_an_param_g <= 25], decreasing=TRUE)[1]

best_an_param_b <- rowMeans(cbind(an_mae_ses_b, an_mae_des_b))
best_an_param_b <- (best_an_param_b - best_an_param_b[1])/best_an_param_b[1] * 100
best_an_param_b <- sort(svals[best_an_param_b <= 25], decreasing=TRUE)[1]

best_dp_param_g <- rowMeans(cbind(dp_mae_ses_g, dp_mae_des_g))
best_dp_param_g <- (best_dp_param_g - best_dp_param_g[1])/best_dp_param_g[1] * 100
best_dp_param_g <- sort(c(20.5, epsvals)[best_dp_param_g <= 25])[1]

best_dp_param_b <- rowMeans(cbind(dp_mae_ses_b, dp_mae_des_b))
best_dp_param_b <- (best_dp_param_b - best_dp_param_b[1])/best_dp_param_b[1] * 100
best_dp_param_b <- sort(c(20.5, epsvals)[best_dp_param_b <= 25])[1]

best_knts_param_g <- rowMeans(cbind(knts_mae_ses_g, knts_mae_des_g))
best_knts_param_g <- (best_knts_param_g - best_knts_param_g[1])/best_knts_param_g[1] * 100
best_knts_param_g <- sort(c(1, kvals)[best_knts_param_g <= 25], decreasing=TRUE)[1]

best_knts_param_b <- rowMeans(cbind(knts_mae_ses_b, knts_mae_des_b))
best_knts_param_b <- (best_knts_param_b - best_knts_param_b[1])/best_knts_param_b[1] * 100
best_knts_param_b <- sort(c(1, kvals)[best_knts_param_b <= 25], decreasing=TRUE)[1]

best_an_param_g
best_dp_param_g
best_knts_param_g
best_an_param_b
best_dp_param_b
best_knts_param_b

dp_pbar_b[c(20.5, epsvals) == best_dp_param_b]
dp_pbar_g[c(20.5, epsvals) == best_dp_param_g]

an_pbar_b[svals == best_an_param_b]
an_pbar_g[svals == best_an_param_g]

knts_pbar_b
knts_pbar_g

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

# now scale the time series to rates (cite Census paper) and compare the 
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

b_simulated_rates <- lapply(b_simulated_series, rate_conversion)
g_simulated_rates <- lapply(g_simulated_series, rate_conversion)

# combine rate series into a dataframe for plotting
b_simulated_rates_df <- bind_cols(b_simulated_rates) %>%
  mutate(Time = 1:n()) %>%
  gather(key='Series', value="Value", -Time)

g_simulated_rates_df <- bind_cols(g_simulated_rates) %>%
  mutate(Time = 1:n()) %>%
  gather(key='Series', value="Value", -Time)

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

r_knts_idr_b <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)
r_knts_idr_g <- matrix(NA, nrow=nsims, ncol=length(kvals)+1)

sp <- 1

# create nsims knts+ swapped data sets
for (j in 1:nsims){
  for (i in 1:(length(kvals)+1)){
    
    if (i == 1){
      r_knts_fcasts_ses_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_b[,i,j] <- unname(sapply(b_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_knts_fcasts_ses_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_g[,i,j] <- unname(sapply(g_simulated_rates_train, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_knts_idr_b[j,i] <-  simulation_results(confidential_data_list=b_simulated_rates_train, protected_data_list=b_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      r_knts_idr_g[j,i] <-  simulation_results(confidential_data_list=g_simulated_rates_train, protected_data_list=g_simulated_rates_train, sample_size=sample_size, num_simulations=npsims)
      
    } else {
      # create and forecast knts+ private series
      r_knts_b_simulated <- as.list(as.data.frame(knts_alg(time_series=b_simulated_rates_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=r_fs_b$selected_features)))
      r_knts_b_simulated <- lapply(r_knts_b_simulated, function(x) ts(x, frequency=sp))
      r_knts_fcasts_ses_b[,i,j] <- unname(sapply(r_knts_b_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_b[,i,j] <- unname(sapply(r_knts_b_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
      r_knts_g_simulated <- as.list(as.data.frame(knts_alg(time_series=g_simulated_rates_train, sp=1, window_length=25, k=kvals[i-1], features_to_calculate=fv, selected_features=r_fs_g$selected_features)))
      r_knts_g_simulated <- lapply(r_knts_g_simulated, function(x) ts(x, frequency=sp))
      r_knts_fcasts_ses_g[,i,j] <- unname(sapply(r_knts_g_simulated, function(x) as.vector(ses(x, h=1)$mean)))
      r_knts_fcasts_des_g[,i,j] <- unname(sapply(r_knts_g_simulated, function(x) as.vector(holt(x, h=1)$mean)))
      
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

# calculate P-bar
r_dp_pbar_b <- apply(r_dp_idr_b, 2, mean)
r_dp_pbar_g <- apply(r_dp_idr_g, 2, mean)

r_an_pbar_b <- apply(r_an_idr_b, 2, mean)
r_an_pbar_g <- apply(r_an_idr_g, 2, mean)

r_knts_pbar_b <- apply(r_knts_idr_b, 2, mean)
r_knts_pbar_g <- apply(r_knts_idr_g, 2, mean)

# plot MAE curves
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
          unlist(r_knts_mae_des_g)),
  Pbar = c(r_knts_pbar_b,
           r_knts_pbar_g,
           r_knts_pbar_b,
           r_knts_pbar_g),
  Model = rep(c("SES", "DES"), each=length(r_knts_mae_ses_b)*2),
  Method = "k-nTS+",
  Parameter = rep(c(1, kvals), 4),
  Set = rep(rep(c("Undesirable", "Desirable"), each=length(r_knts_mae_ses_b)), 2))

r_results <- r_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

r_knts_results <- r_knts_results %>%
  gather(key="Type", value="Value", -Set, -Method, -Parameter, -Model)

r_results %>%
  filter(Method == "AN", Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  facet_wrap(~Set)

r_results %>%
  filter(Method == "DP", Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~Set)

r_knts_results %>%
  filter(Type=="MAE") %>%
  ggplot(aes(x=Parameter, y=Value, color=Model)) +
  geom_line() +
  facet_wrap(~Set)

r_avg_results <- r_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

r_knts_avg_results <- r_knts_results %>%
  group_by(Set, Method, Parameter, Type) %>%
  summarize(Average_Value = mean(Value), .groups='drop')

r_avg_results %>%
  filter(Method == "AN") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Type, scales='free')

r_avg_results %>%
  filter(Method == "DP") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  scale_x_reverse() +
  facet_wrap(~Type, scales='free')

r_knts_avg_results %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Type, scales='free')

# what is the best privacy parameter we can have that still preserves
# forecast accuracy within 25%?

r_best_an_param_g <- rowMeans(cbind(r_an_mae_ses_g, r_an_mae_des_g))
r_best_an_param_g <- (r_best_an_param_g - r_best_an_param_g[1])/r_best_an_param_g[1] * 100
r_best_an_param_g <- sort(svals[r_best_an_param_g <= 25], decreasing=TRUE)[1]

r_best_an_param_b <- rowMeans(cbind(r_an_mae_ses_b, r_an_mae_des_b))
r_best_an_param_b <- (r_best_an_param_b - r_best_an_param_b[1])/r_best_an_param_b[1] * 100
r_best_an_param_b <- sort(svals[r_best_an_param_b <= 25], decreasing=TRUE)[1]

r_best_dp_param_g <- rowMeans(cbind(r_dp_mae_ses_g, r_dp_mae_des_g))
r_best_dp_param_g <- (r_best_dp_param_g - r_best_dp_param_g[1])/r_best_dp_param_g[1] * 100
r_best_dp_param_g <- sort(c(20.5, epsvals)[r_best_dp_param_g <= 25])[1]

r_best_dp_param_b <- rowMeans(cbind(r_dp_mae_ses_b, r_dp_mae_des_b))
r_best_dp_param_b <- (r_best_dp_param_b - r_best_dp_param_b[1])/r_best_dp_param_b[1] * 100
r_best_dp_param_b <- sort(c(20.5, epsvals)[r_best_dp_param_b <= 25])[1]

r_best_knts_param_g <- rowMeans(cbind(r_knts_mae_ses_g, r_knts_mae_des_g))
r_best_knts_param_g <- (r_best_knts_param_g - r_best_knts_param_g[1])/r_best_knts_param_g[1] * 100
r_best_knts_param_g <- sort(c(1, kvals)[r_best_knts_param_g <= 25], decreasing=TRUE)[1]

r_best_knts_param_b <- rowMeans(cbind(r_knts_mae_ses_b, r_knts_mae_des_b))
r_best_knts_param_b <- (r_best_knts_param_b - r_best_knts_param_b[1])/r_best_knts_param_b[1] * 100
r_best_knts_param_b <- sort(c(1, kvals)[r_best_knts_param_b <= 25], decreasing=TRUE)[1]

r_best_an_param_g
r_best_dp_param_g
r_best_knts_param_g
r_best_an_param_b
r_best_dp_param_b
r_best_knts_param_b

r_dp_pbar_b[c(20.5, epsvals) == r_best_dp_param_b]
r_dp_pbar_g[c(20.5, epsvals) == r_best_dp_param_g]

r_an_pbar_b[svals == r_best_an_param_b]
r_an_pbar_g[svals == r_best_an_param_g]

r_knts_pbar_b[c(1, kvals) == r_best_knts_param_b]
r_knts_pbar_g[c(1, kvals) == r_best_knts_param_g]

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

# plot the average forecast accuracy curves for each method on each data set
library(ggh4x)

# an, dp, knts forecast error on original time series space
scales <- list(
  scale_x_continuous(),
  scale_x_reverse(),
  scale_x_continuous()
)

avg_results %>%
  bind_rows(avg_knts_results) %>%
  filter(Type == "MAE") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales)

# an, dp, knts identification disclosure on original time series space
avg_results %>%
  bind_rows(avg_knts_results) %>%
  filter(Type == "Pbar") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales)

# an, dp, knts on rate time series space

r_avg_results %>%
  bind_rows(r_knts_avg_results) %>%
  filter(Type == "MAE") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales)

# an, dp, knts identification disclosure on original time series space
r_avg_results %>%
  bind_rows(r_knts_avg_results) %>%
  filter(Type == "Pbar") %>%
  ggplot(aes(x=Parameter, y=Average_Value, color=Set)) +
  geom_line() +
  facet_wrap(~Method, scales='free_x') +
  facetted_pos_scales(x = scales)

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

# plot time series features to examine which ones are changing the most
# from data protection. We want to see whether the closeness of time series values
# and autocorrelation are the main factors determining the accuracy of protected
# data, or whether other features should be considered

# extract features from the original time series

# extract from DP, AN, and k-nTS+ with acceptable accuracy in original
# time series space

# extract from DP, AN, and k-nTS+ with acceptable accuracy in rate 
# time series space

# orig_features_b <- tsfeatures(tslist=b_simulated_series, features=fv, scale=FALSE)
# orig_features_g <- tsfeatures(tslist=g_simulated_series, features=fv, scale=FALSE)
# 
# dp_features_b <- tsfeatures(tslist=b_simulated_series, features=fv, scale=FALSE)
# dp_features_g
# 
# an_features_b
# an_features_g
# 
# knts_features_b
# knts_features_g

# repeat the above but for rates
