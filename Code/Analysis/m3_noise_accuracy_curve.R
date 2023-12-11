
library(forecast)
library(tsfeatures)
library(tidyverse)
library(tsne)
library(e1071)
library(ranger)
library(CORElearn)
# library(proxy)
source('custom_feature_functions.R')

# function to calculate time series features
feature_calculator <- function(ts, features_to_calculate, sp){
  
  temp <- tsfeatures(ts, features=features_to_calculate, scale=FALSE) %>%
    select(-nperiods, -seasonal_period)
  
  return(temp)
}

# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# import the M4 time series data
# there are 759 time series with a length of 104, and the percent increase
# in MAE was over 1000

# import the M4 monthly data
monthly_micro <- read.csv("../../Data/Cleaned/M3/monthly-MICRO_h1_train.csv")
yearly_industry <- read.csv("../../Data/Cleaned/M3/yearly-INDUSTRY_h1_train.csv")

# import data and convert to a list of series
monthly_micro <- as.list(as.data.frame(t(monthly_micro)))
yearly_industry <- as.list(as.data.frame(t(yearly_industry)))

# remove NA values from the end of each series
monthly_micro <- lapply(monthly_micro, function(x) x[!is.na(x)])
yearly_industry <- lapply(yearly_industry, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
monthly_micro <- lapply(monthly_micro, function(x) ts(x, frequency=12))
yearly_industry <- lapply(yearly_industry, function(x) ts(x, frequency=1))

# truncate data to strictly positive
monthly_micro <- lapply(monthly_micro, function(x) ifelse(x >= 1, x, 1))
yearly_industry <- lapply(yearly_industry, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
monthly_micro <- lapply(monthly_micro, log)
yearly_industry <- lapply(yearly_industry, log)

# import the test data and filter for the test data for the series of length 104
monthly_micro_test <- read.csv("../../Data/Cleaned/M3/monthly-MICRO_h1_test.csv")[,1]
yearly_industry_test <- read.csv("../../Data/Cleaned/M3/yearly-INDUSTRY_h1_test.csv")[,1]

### now build a for-loop that adds increasing amounts of noise to the time
### series, generates forecasts, and saves the accuracy

# use additive noise for the noise addition since this an intuitive amount
# of noise that we can tie back to the standard deviation of the time series

additive_noise <- function(time_series, s){
  noise_vals <- rnorm(n=length(time_series), sd=s*sd(time_series))
  return(time_series + noise_vals)
}

# define vector of s values for additive noise
svals <- seq(from=0, to=3.0, by=0.05)

# vectors to store mae values for both models
full_ses_mae <- c()
full_des_mae <- c()
avg_spec_entropy <- c()

# loop over s values
for (i in svals){
  
  # add noise
  noisy_X <- lapply(yearly_industry, function(x) additive_noise(x, i))
  
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  full_ses_mae <- c(full_ses_mae, mean(abs(ses_fcasts - yearly_industry_test)))
  full_des_mae <- c(full_des_mae, mean(abs(des_fcasts - yearly_industry_test)))
  
  # calculate the spectral entropy of the time series
  avg_spec_entropy <- c(avg_spec_entropy, mean(sapply(noisy_X, entropy)))
  
}

# combine mae and s values into a tibble
bad_results <- tibble(SES = full_ses_mae,
                      DES = full_des_mae,
                      Avg_Spec_Entropy = avg_spec_entropy,
                      s = svals)

bad_results <- bad_results %>%
  gather(key="Model", value="MAE", -s, -Avg_Spec_Entropy) %>%
  mutate(Set = "Bad Protected Accuracy")

################################################################################

### now build a for-loop that adds increasing amounts of noise to the time
### series, generates forecasts, and saves the accuracy

# use additive noise for the noise addition since this an intuitive amount
# of noise that we can tie back to the standard deviation of the time series
# vectors to store mae values for both models
full_ses_mae <- c()
full_des_mae <- c()
avg_spec_entropy <- c()

# loop over s values
for (i in svals){
  
  # add noise
  noisy_X <- lapply(monthly_micro, function(x) additive_noise(x, i))
  
  # now perform forecasting using SES and DES
  ses_fcasts <- unname(sapply(noisy_X, function(x) as.vector(ses(x, h=1)$mean)))
  des_fcasts <- unname(sapply(noisy_X, function(x) as.vector(holt(x, h=1)$mean)))
  
  # reverse the log
  ses_fcasts <- exp(ses_fcasts)
  des_fcasts <- exp(des_fcasts)
  
  # compute forecast error (MAE)
  full_ses_mae <- c(full_ses_mae, mean(abs(ses_fcasts - monthly_micro_test)))
  full_des_mae <- c(full_des_mae, mean(abs(des_fcasts - monthly_micro_test)))
  
  # calculate the spectral entropy of the time series
  avg_spec_entropy <- c(avg_spec_entropy, mean(sapply(noisy_X, entropy)))
  
}

# combine mae and s values into a tibble
good_results <- tibble(SES = full_ses_mae,
                       DES = full_des_mae,
                       Avg_Spec_Entropy = avg_spec_entropy,
                       s = svals)

good_results <- good_results %>%
  gather(key="Model", value="MAE", -s, -Avg_Spec_Entropy) %>%
  mutate(Set="Good Protected Accuracy")

all_results <- good_results %>% bind_rows(bad_results)

all_results %>%
  ggplot(aes(x=s, y=MAE, color=Model)) +
  geom_line() +
  facet_wrap(~Set) +
  labs(x = "Additive Noise Parameter: s",
       title = "Mean Absolute Error Across Protected Series")


monthly_micro_df <- lapply(1:length(monthly_micro), function(x) tibble(Value=monthly_micro[[x]], 
                                                                       Time=1:length(monthly_micro[[x]]),
                                                                       Series=x))

yearly_industry_df <- lapply(1:length(yearly_industry), function(x) tibble(Value=yearly_industry[[x]],
                                                                           Time=1:length(yearly_industry[[x]]),
                                                                           Series=x))

# now plot the time series themselves
monthly_micro_df <- bind_rows(monthly_micro_df) %>%
  mutate(Set = "Good Protected Accuracy")

# now plot the time series themselves
yearly_industry_df <- bind_rows(yearly_industry_df) %>%
  mutate(Set = "Bad Protected Accuracy")

X_df <- bind_rows(monthly_micro_df, yearly_industry_df)

X_df %>%
  ggplot(aes(x=Time, y=Value, color=as.factor(Series))) +
  geom_line() +
  facet_wrap(~Set, scales='free') +
  theme(legend.position='none') +
  labs(title="Original Time Series")

all_results %>%
  distinct(s, Avg_Spec_Entropy, Set) %>%
  ggplot(aes(x=s, y=Avg_Spec_Entropy)) +
  geom_line() +
  facet_wrap(~Set) +
  labs(x="Additive Noise Parameter: s",
       y="Average Spectral Entropy")

################################################################################
################################################################################
################################################################################
################################################################################

# we know the following features were selected by the machine learning-based
# feature selection when all M4 monthly time series were included together

selected_features_mm <- list()
selected_features_yi <- list()

relief_rankings_mm <- read_csv("../../Outputs/RReliefF Rankings/M3/RReliefF_monthly-MICRO_h1_train.csv")
relief_rankings_yi <- read_csv("../../Outputs/RReliefF Rankings/M3/RReliefF_yearly-INDUSTRY_h1_train.csv")
rfe_oob_mm <- read_csv("../../Outputs/RFE OOB/M3/RFE_monthly-MICRO_h1_train.csv")
rfe_oob_yi <- read_csv("../../Outputs/RFE OOB/M3/RFE_yearly-INDUSTRY_h1_train.csv")
rfe_rankings_mm <- read_csv("../../Outputs/RFE Rankings/M3/RFE_monthly-MICRO_h1_train.csv")
rfe_rankings_yi <- read_csv("../../Outputs/RFE Rankings/M3/RFE_yearly-INDUSTRY_h1_train.csv")

# calculate how many features needed to have within 5% of minimum prediction
# error
nf <- rfe_oob_mm %>%
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

sf_mm <- rfe_rankings_mm %>%
  group_by(var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank) %>%
  slice(1:nf) %>%
  pull(var)

sf_mm

# for monthly micro we selected variance, mean, spike, max variance shift, and max level shift

# calculate how many features needed to have within 5% of minimum prediction
# error
nf <- rfe_oob_yi %>%
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

sf_yi <- rfe_rankings_yi %>%
  group_by(var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank) %>%
  slice(1:nf) %>%
  pull(var)

sf_yi

# for yearly industry we selected variance, spike, mean, x_acf1, hurst,
# max variance shift, and kurtosis

## plot the feature distributions
orig_features_mm <- feature_calculator(ts=monthly_micro, features_to_calculate=fv, sp=12)
orig_features_yi <- feature_calculator(ts=yearly_industry, features_to_calculate=fv, sp=1)

# import k-nTS+ data
knts_mm <- read_csv("../../Data/Cleaned/M3/k-nts-plus_3_monthly-MICRO_h1_train.csv")
knts_yi <- read_csv("../../Data/Cleaned/M3/k-nts-plus_3_yearly-INDUSTRY_h1_train.csv")

# import data and convert to a list of series
knts_mm <- as.list(as.data.frame(t(knts_mm)))
knts_yi <- as.list(as.data.frame(t(knts_yi)))

# remove NA values from the end of each series
knts_mm <- lapply(knts_mm, function(x) x[!is.na(x)])
knts_yi <- lapply(knts_yi, function(x) x[!is.na(x)])

# convert each series to a TS object with appropriate seasonal frequency
knts_mm <- lapply(knts_mm, function(x) ts(x, frequency=12))
knts_yi <- lapply(knts_yi, function(x) ts(x, frequency=1))

# truncate data to strictly positive
knts_mm <- lapply(knts_mm, function(x) ifelse(x >= 1, x, 1))
knts_yi <- lapply(knts_yi, function(x) ifelse(x >= 1, x, 1))

# take the log of the data
knts_mm <- lapply(knts_mm, log)
knts_yi <- lapply(knts_yi, log)

### extract features from knts_g and knts_b

knts_features_mm <- feature_calculator(ts=knts_mm, features_to_calculate=fv, sp=12)
knts_features_yi <- feature_calculator(ts=knts_yi, features_to_calculate=fv, sp=1)

## plot feature distributions like in the paper

knts_features_mm %>%
  bind_rows(orig_features_mm) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_mm))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, seasonal_strength, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')

###### repeat for series with good increase in forecast error

knts_features_yi %>%
  bind_rows(orig_features_yi) %>%
  mutate(Type = rep(c("k-nTS+ (k = 3)", "Original"), each=nrow(knts_features_yi))) %>%
  select(Type, entropy, x_acf1, hurst, skewness, kurtosis, e_acf1, trend, series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  gather(key="Feature", value="Value", -Type) %>%
  ggplot(aes(x=Type, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free')

################################################################################
################################################################################
