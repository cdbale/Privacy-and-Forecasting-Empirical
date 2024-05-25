## Custom feature functions

# Author: Cameron Bale

# feature extraction function
extract_features <- function(time_series, sp, feature_vector, truncate, take_log, calculate_cross_correlations=FALSE){
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(time_series)))
  
  # remove NA values from the end of each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  if (truncate){
    # truncate data to strictly positive
    ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  }

  if (take_log){
    # take the log of the data
    ts_data <- lapply(ts_data, log)
  }

  # calculate time series features
  features <- tsfeatures(ts_data, features=feature_vector, scale=FALSE)
  
  if (calculate_cross_correlations){
    cor_vars <- cross_correlations(ts_data)
    return(bind_cols(features, cor_vars))
  } else {
    return(features)
  }
  
}

# function to calculate cross-series correlations and perform PCA
cross_correlations <- function(X){
  
  # split X into separate datasets, one for each series length
  Xs <- list()
  lengths <- sapply(X, length)
  unique_lengths <- unique(lengths)
  for (l in seq_along(unique_lengths)){
    ids <- lengths==unique_lengths[l]
    Xs[[l]] <- X[ids]
  }
  
  new_vars <- tibble()
  
  for (Y in Xs){
    temp_ts_data <- lapply(Y, as.vector)
    # combine into data frame with series in columns
    temp_ts_data <- do.call(cbind, temp_ts_data)
    # perform PCA
    pc <- prcomp(temp_ts_data, center=TRUE, scale=TRUE)
    # take first 5 components
    cor_vars <- pc$rotation[,1:5]
    # add identifying names
    colnames(cor_vars) <- c("cross_cor_1", "cross_cor_2", "cross_cor_3", "cross_cor_4", "cross_cor_5")
    new_vars <- bind_rows(new_vars, as_tibble(cor_vars))
  }
  return(new_vars)
}

# functions to calculate the mean and variance of the series (window)
series_mean <- function(x){
  return(mean(x))
}

series_variance <- function(x){
  return(var(x))
}

# custom spectral entropy
# handles case of zero variance series
entropy_c <- function(x){
  if (var(x) == 0){
    return(return(c(entropy = 0)))
  }
  spec <- try(stats::spec.ar(na.contiguous(x), plot = FALSE, 
                             method = "burg", n.freq = ceiling(length(x)/2 + 1)))
  if ("try-error" %in% class(spec)) {
    entropy <- NA
  }
  else {
    fx <- c(rev(spec$spec[-1]), spec$spec)/length(x)
    fx <- fx/sum(fx)
    prior.fx = rep(1/length(fx), length = length(fx))
    prior.weight = 0.001
    fx <- (1 - prior.weight) * fx + prior.weight * prior.fx
    entropy <- pmin(1, -sum(fx * log(fx, base = length(x))))
  }
  return(c(entropy = entropy))
}

# max level shift with custom width
max_level_shift_c <- function (x, width = ifelse(frequency(x) > 1, frequency(x), floor(length(x)/3))) {
  suppressWarnings(rollmean <- try(RcppRoll::roll_mean(x, width, 
                                                       na.rm = TRUE), silent = TRUE))
  if ("try-error" %in% class(rollmean)) {
    maxmeans <- NA_real_
    maxidx <- NA_real_
  }
  else {
    means <- abs(diff(rollmean, width))
    if (length(means) == 0L) {
      maxmeans <- 0
      maxidx <- NA_real_
    }
    else if (all(is.na(means))) {
      maxmeans <- NA_real_
      maxidx <- NA_real_
    }
    else {
      maxmeans <- max(means, na.rm = TRUE)
      maxidx <- which.max(means) + width - 1L
    }
  }
  return(c(max_level_shift = maxmeans, time_level_shift = maxidx))
}

# max var shift with custom width
max_var_shift_c <- function (x, width = ifelse(frequency(x) > 1, frequency(x), floor(length(x)/3))) {
  suppressWarnings(rollvar <- try(RcppRoll::roll_var(x, width, 
                                                     na.rm = TRUE), silent = TRUE))
  if ("try-error" %in% class(rollvar)) {
    maxvar <- NA_real_
    maxidx <- NA_real_
  }
  else {
    vars <- abs(diff(rollvar, width))
    if (length(vars) == 0L) {
      maxvar <- 0
      maxidx <- NA_real_
    }
    else if (all(is.na(vars))) {
      maxvar <- NA_real_
      maxidx <- NA_real_
    }
    else {
      maxvar <- max(vars, na.rm = TRUE)
      maxidx <- which.max(vars) + width - 1L
    }
  }
  return(c(max_var_shift = maxvar, time_var_shift = maxidx))
}

# max kl shift with custom width
max_kl_shift_c <- function (x, width = ifelse(frequency(x) > 1, frequency(x), floor(length(x)/3))){
  gw <- 100
  xgrid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), 
               length = gw)
  grid <- xgrid[2L] - xgrid[1L]
  tmpx <- x[!is.na(x)]
  bw <- bw.nrd0(tmpx)
  lenx <- length(x)
  if (lenx <= (2 * width)) {
    return(c(max_kl_shift = NA_real_, time_kl_shift = NA_real_))
  }
  dens.mat <- matrix(, nrow = lenx, ncol = gw)
  for (i in 1L:lenx) {
    dens.mat[i, ] <- dnorm(xgrid, mean = x[i], sd = bw)
  }
  dens.mat <- pmax(dens.mat, dnorm(38))
  rmean <- RcppRoll::roll_mean(dens.mat, n = width, na.rm = TRUE, 
                               fill = NA, align = "right")
  lo <- seq(1, lenx - width + 1)
  hi <- seq(width + 1, lenx)
  seqidx <- min(length(lo), length(hi))
  kl <- sapply(1:seqidx, function(i) sum(rmean[lo[i], ] * (log(rmean[lo[i], 
  ]) - log(rmean[hi[i], ])) * grid, na.rm = TRUE))
  diffkl <- diff(kl, na.rm = TRUE)
  if (length(diffkl) == 0L) {
    diffkl <- 0
    maxidx <- NA_real_
  }
  else {
    maxidx <- which.max(diffkl) + width - 1L
  }
  return(c(max_kl_shift = max(diffkl, na.rm = TRUE), time_kl_shift = maxidx))
}




