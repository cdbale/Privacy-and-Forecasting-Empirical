### Code to assess the time series characteristics before and after data protection.

## Author: Cameron Bale

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(entropy)

###################################################################

KLcalc <- function(x1,x2){
  
  # density estimates
  y1d <- density(x1, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))
  y2d <- density(x2, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))
  
  # density estimates
  y1 <- density(x1, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)), bw=min(y1d$bw,y2d$bw))$y
  y2 <- density(x2, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)), bw=min(y1d$bw,y2d$bw))$y
  
  # convert to probabilities
  y1 <- y1/sum(y1)
  y2 <- y2/sum(y2)
  
  # fix division by 0 issue
  y1 <- y1 + .Machine$double.eps
  y2 <- y2 + .Machine$double.eps
  
  kl <- round(KL.Dirichlet(y1, y2, a1=0, a2=0, unit="log2"),3)
  ent <- entropy.Dirichlet(y1, a=0, unit="log2")
  info <- round(kl*100/ent,2)
  
  return(list(kl=kl,ent=ent,info=info))
}

###################################################################

# import the features from protected data
# combine into one dataframe, with indicators for privacy method and parameter
file_names <- grep("_h1_", list.files("../../Data/Features/"), value=TRUE)

temp <- read.csv(paste0("../../Data/Features/", file_names[1]))

file_names[1]

###############
# read in the protected time series features, combine into one dataframe, 
# adding variables that indicate the forecast horizon, privacy method, and privacy parameter

# can't select seasonal strength since it isn't computable for non-seasonal data

full_features <- tibble()

for (f in file_names){
  
  features <- read_csv(paste0("../../Data/Features/", f)) %>%
    select(entropy, hurst, skewness, kurtosis, e_acf1, trend,
           series_mean, series_variance, spike, max_var_shift, max_level_shift)
  
  params <- strsplit(f, split="_")
  
  features["snum"] <- 1:nrow(features)
  if (!params[[1]][2] %in% c("AN", "DP", "k-nts", "k-nts-plus", "k-nts-corr", "k-nts-plus-corr")){
    features["method"] <- "Original"
    features["parameter"] <- "Original"
  } else {
    features["method"] <- params[[1]][2]
    features["parameter"] <- params[[1]][3]
  }
  
  features["data"] <- params[[1]][length(params[[1]])-2]
  
  full_features <- bind_rows(full_features, features)

}

## Create a plot for every time series feature, each one is a boxplot
## of the original and protected feature distributions

# ac_features <- orig_features %>%
#   select(snum, method, parameter, x_acf1, x_acf10, x_pacf5, seas_acf1) %>%
#   filter(method %in% c("original", "AN", "DP", "k-nts", "k-nts-plus"))
















################################

### visualize a good/bad series (based on feature values)

process_data <- function(time_series, sp){
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(time_series)))
  
  # remove NA values from each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  # truncate data to strictly positive
  ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  
  # take the log of the data
  ts_data <- lapply(ts_data, log)
  
  return(ts_data)
}

orig_series <- process_data(read.csv("../../Data/Train/Clean/m3_monthly_micro_h1.csv"), sp=12)
an_series <- process_data(read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_AN_1.csv"), sp=12)
kntsp_series <- process_data(read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_k-nts-plus_3.csv"), sp=12)

gs_orig <- features %>%
  filter(parameter == 'none') %>%
  arrange(entropy) %>%
  filter(snum==155)

gs_orig

gs_an <- features %>%
  filter(method=='AN', parameter == '1') %>%
  filter(snum==155)

gs_an

gs_kntsp <- features %>%
  filter(method=="k-nts-plus", parameter == '3') %>%
  filter(snum==155)

gs_kntsp

bs_orig <- features %>%
  filter(parameter == 'none') %>%
  arrange(desc(entropy)) %>%
  filter(snum==2)

bs_orig

bs_an <- features %>%
  filter(method=='AN', parameter == '1') %>%
  filter(snum==2)

bs_an

bs_kntsp <- features %>%
  filter(method=="k-nts-plus", parameter == '3') %>%
  filter(snum==2)

bs_kntsp

good_series_unp <- tibble(x = orig_series[[155]], t = 1:length(orig_series[[155]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features",
       x = 'Time',
       y = 'x')

bad_series_unp <- tibble(x = orig_series[[2]], t = 1:length(orig_series[[2]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features",
       x = 'Time',
       y = "")

good_series_an <- tibble(x = an_series[[155]], t = 1:length(an_series[[155]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features (AN, s = 1)",
       x = 'Time',
       y = 'x')

bad_series_an <- tibble(x = an_series[[2]], t = 1:length(an_series[[2]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features (AN, s = 1)",
       x = 'Time',
       y = "")

good_series_kntsp <- tibble(x = kntsp_series[[155]], t = 1:length(kntsp_series[[155]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features (k-nTS+, k = 3)",
       x = 'Time',
       y = 'x')

bad_series_kntsp <- tibble(x = kntsp_series[[2]], t = 1:length(kntsp_series[[2]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(size=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features (k-nTS+, k = 3)",
       x = 'Time',
       y = "")

# unprotected versions
g1 <- ggarrange(good_series_unp, bad_series_unp,
          nrow=1, ncol=2)

annotate_figure(g1, top=text_grob("", face = "bold", size = 14))

# additive noise versions
g2 <- ggarrange(good_series_an, bad_series_an,
          nrow=1, ncol=2)

annotate_figure(g2, top=text_grob("Protected Time Series (AN with s = 1.0)", face = "bold", size = 14))

# k-nTS plus versions
g3 <- ggarrange(good_series_kntsp, bad_series_kntsp,
          nrow=1, ncol=2)

annotate_figure(g3, top=text_grob("Protected Time Series (k-nTS+ with k = 3)", face = "bold", size = 14))

g4 <- ggarrange(good_series_unp, bad_series_unp,
                good_series_kntsp, bad_series_kntsp,
                good_series_an, bad_series_an,
                nrow=3, ncol=2, labels=c("A", "B", "A", "B", "A", "B"))

annotate_figure(g4, top=text_grob("", face = "bold", size = 14))

################################
################################
################################









ac_features <- ac_features %>%
  gather(key="Feature", value="Value", -method, -parameter, -snum) %>%
  mutate(method = factor(method, 
                         levels=c("original", "AN", "DP", "k-nts", "k-nts-plus"),
                         labels=c("Original", "AN", "DP", "k-nTS", "k-nTS+")))

avg_ac_features <- ac_features %>%
  group_by(snum, Feature, method) %>%
  summarize(avg_val = mean(Value), .groups="drop") 

avg_ac_features %>%
  mutate(Feature = factor(Feature, 
                          levels=c("x_acf1", "x_acf10", "x_pacf5", "seas_acf1"),
                          labels=c("X ACF", "X ACF10", "X PACF5", "Seasonal ACF")
                          )) %>%
  ggplot(aes(x=method, y=avg_val)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free') +
  labs(x = "Privacy Method",
       y = "Average Feature Value",
       title = "")















full_features <- full_features %>%
  gather(key="Feature", value="Value", -method, -parameter, -snum, -data) %>%
  mutate(method = factor(method, 
                         levels=c("Original", "AN", "DP", "k-nts", "k-nts-corr", "k-nts-plus", "k-nts-plus-corr"),
                         labels=c("Original", "AN", "DP", "k-nTS", "k-nTS-Cor", "k-nTS+", "k-nTS+-Cor")),
         Feature = factor(Feature,
                          levels=c("entropy", "hurst", "skewness",
                                   "kurtosis", "e_acf1", "trend", "series_mean",
                                   "series_variance", "spike", "max_var_shift",
                                   "max_level_shift"),
                          labels=c("Spectral Entropy", "Hurst", "Skewness",
                                   "Kurtosis", "Error ACF", "Trend", "Mean",
                                   "Variance", "Spike", "Max Variance Shift",
                                   "Max Level Shift")))

full_features %>%
  ggplot(aes(x=method, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free') +
  labs(x = "Privacy Method",
       y = "Average Feature Value",
       title = "")


avg_features <- full_features %>%
  group_by(snum, Feature, method) %>%
  summarize(avg_val = mean(Value), .groups="drop") 

avg_features %>%
  ggplot(aes(x=method, y=avg_val)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free') +
  labs(x = "Privacy Method",
       y = "Average Feature Value",
       title = "")

########## Plot the auto-correlation features for each protection method ##############

avg_features

avg_vals <- avg_features %>%
  group_by(Feature, method) %>%
  summarize(avg_val=mean(avg_val))

## compare distribution of k-nTS and original variance
avg_features %>%
  filter(method %in% c("k-nTS", "Original"), Feature=="SeriesVariance") %>%
  group_by(method) %>%
  summarize(m = mean(avg_val),
            v = var(avg_val),
            lower = range(avg_val)[1],
            upper = range(avg_val)[2])

#############################################################################