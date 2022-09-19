## Testing REP on model selection for differentially private time series.

# Author: Cameron Bale

# uses the REP function in the `REP_20220531 - code.R` file.

# source file for REP function
source('REP_20220531 - code.R')

library(tidyverse)
library(ggplot2)

# import differentially private data
original_data <- read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_DP_0.1.csv")
test_data <- read.csv("../../Outputs/Forecasts/Test_h1.csv")

# convert to a list of series
ts_data <- as.list(as.data.frame(t(original_data)))

# remove NA values from each series
ts_data <- lapply(ts_data, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
ts_data <- lapply(ts_data, function(x) ts(x, frequency=12))

# truncate data to be strictly positive
ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))

# fit and forecast with SES and TES
ses_mod <- ets(y=ts_data[[1]], model="ANN", lambda=0, biasadj=TRUE)
tes_mod <- ets(y=ts_data[[1]], model="AAA", lambda=0, biasadj=TRUE)

# save fitted values
ses_fitted <- ses_mod$fitted
tes_fitted <- tes_mod$fitted

# save forecasts
ses_fcasts <- forecast(ses_mod, h=1)$mean
tes_fcasts <- forecast(tes_mod, h=1)$mean

# calculate REP scores
REP(y=ts_data[[1]], fitted=ses_fitted, f=ses_fcasts)
REP(y=ts_data[[1]], fitted=tes_fitted, f=tes_fcasts)

ses_reps <- c()
tes_reps <- c()

ses_fcasts <- c()
tes_fcasts <- c()

##### Repeat for all series, make comparisons for each series, and look at distributions of REP scores.
for (i in seq_along(ts_data)){
  
  # fit and forecast with SES and TES
  ses_mod <- ets(y=ts_data[[i]], model="ANN", lambda=0, biasadj=TRUE)
  tes_mod <- ets(y=ts_data[[i]], model="AAA", lambda=0, biasadj=TRUE)
  
  # save fitted values
  ses_fitted <- ses_mod$fitted
  tes_fitted <- tes_mod$fitted
  
  # save forecasts
  ses_fcast <- forecast(ses_mod, h=1)$mean
  tes_fcast <- forecast(tes_mod, h=1)$mean
  
  ses_fcasts <- append(ses_fcasts, ses_fcast)
  tes_fcasts <- append(tes_fcasts, tes_fcast)
  
  # calculate REP scores
  ses_reps <- append(ses_reps, REP(y=ts_data[[i]], fitted=ses_fitted, f=ses_fcast))
  tes_reps <- append(tes_reps, REP(y=ts_data[[i]], fitted=tes_fitted, f=tes_fcast))
  
}

# calculate the percentage of times REP chooses TES over SES
tes_better <- ses_reps > tes_reps
mean(tes_better)

# view the distributions of REP scores for each model
reps_data <- tibble(ses=ses_reps, tes=tes_reps) %>%
  gather(key='model', value='rep_score') 

# density plot
reps_data %>%
  ggplot(aes(x=rep_score, fill=model)) +
  geom_density()

# box plot
reps_data %>%
  ggplot(aes(x=model, y=rep_score)) +
  geom_boxplot()

# what is the MAE when using REP to choose between SES and TES?
fcasts <- lapply(1:length(ts_data), function(x) ifelse(tes_better[x]==1, tes_fcasts[x], ses_fcasts[x]))

fcasts <- as.data.frame(t(as.data.frame(tes_fcasts)))

# calculate MAE for fcasts
fcasts <- as.data.frame(do.call(cbind, fcasts))

apply(abs(fcasts - test_data), 1, mean)


