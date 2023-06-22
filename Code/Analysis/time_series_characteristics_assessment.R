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
file_names <- grep("tsfeatures", list.files("../../Data/Train/Clean/tsfeatures/"), value=TRUE, invert=TRUE)
file_names <- grep("h1_", file_names, value=TRUE)

# read in the original time series features
orig_features <- read_csv("../../Data/Train/Clean/tsfeatures/tsfeatures_h1.csv")

# reg_features <- orig_features

orig_features["snum"] <- 1:474
orig_features["horizon"] <- "h1"
orig_features["method"] <- "original"
orig_features["parameter"] <- "none"

###############

orig_features %>%
  summarize(pct_high_entropy = mean(entropy >= 0.75))

###############
# read in the protected time series features, combine into one dataframe, 
# adding variables that indicate the forecast horizon, privacy method, and privacy parameter

for (f in file_names){
  
  features <- read_csv(paste0("../../Data/Train/Clean/tsfeatures/", f))
  
  params <- strsplit(f, split="_")
  
  features["snum"] <- 1:474
  features["horizon"] <- params[[1]][1]
  features["method"] <- params[[1]][2]
  features["parameter"] <- strsplit(params[[1]][3], split=".csv")
  
  orig_features <- bind_rows(orig_features, features)

}

## Create a plot for every time series feature, each one is a boxplot
## of the original and protected feature distributions

features <- orig_features %>%
  select(snum, method, parameter, entropy, hurst, 
         skewness, kurtosis, e_acf1, trend, seasonal_strength,
         series_mean, series_variance, spike, max_var_shift, max_level_shift) %>%
  filter(method %in% c("original", "AN", "DP", "k-nts", "k-nts-plus"))

ac_features <- orig_features %>%
  select(snum, method, parameter, x_acf1, x_acf10, x_pacf5, seas_acf1) %>%
  filter(method %in% c("original", "AN", "DP", "k-nts", "k-nts-plus"))

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

features <- features %>%
  gather(key="Feature", value="Value", -method, -parameter, -snum) %>%
  mutate(method = factor(method, 
                         levels=c("original", "AN", "DP", "k-nts", "k-nts-plus"),
                         labels=c("Original", "AN", "DP", "k-nTS", "k-nTS+")),
         Feature = factor(Feature,
                          levels=c("entropy", "hurst", "skewness",
                                   "kurtosis", "e_acf1", "trend",
                                   "seasonal_strength", "series_mean",
                                   "series_variance", "spike", "max_var_shift",
                                   "max_level_shift"),
                          labels=c("Spectral Entropy", "Hurst", "Skewness",
                                   "Kurtosis", "Error ACF", "Trend",
                                   "Seasonality", "Mean",
                                   "Variance", "Spike", "Max Variance Shift",
                                   "Max Level Shift")))

avg_features <- features %>%
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

### Calculate KL-divergence for each distribution

# The plot shows the distribution of the average feature value across protected datasets for each series.

# We calculate the KL-divergence of each of these distributions from the original distribution.

kl_comparison <- function(feature_data, feature_name){
  
  orig_feat <- feature_data %>%
    filter(method=="Original", Feature==feature_name) %>%
    pull(avg_val)
  
  kl_vals <- feature_data %>%
    filter(Feature==feature_name) %>%
    group_by(method) %>%
    summarize(KL = KLcalc(avg_val, orig_feat)[[3]], .groups="drop") %>%
    mutate(Feature = feature_name)
  
  return(kl_vals)
  
}

feats <- c("SpecEntropy", "Hurst", "Skewness",
           "Kurtosis", "E_acf", "Trend",
           "Seasonality", "SeriesMean", "SeriesVariance")

kls <- do.call(bind_rows, lapply(feats, function(x) kl_comparison(avg_features, x)))

kls_table <- kls %>%
  filter(method != "Original") %>%
  spread(key=Feature, value=KL) %>%
  select(method, SpecEntropy, Hurst, Skewness, Kurtosis, E_acf, Trend, Seasonality, SeriesMean, SeriesVariance)

kls_table$AveragePctDiff <- apply(kls_table[,-1], 1, mean)

kls_table

pdf("../../Outputs/figures/KL-table.pdf", paper="USr")
grid.table(kls_table)
dev.off()

#############################################################################

## regress the accuracy of each model on the time series features

# path to files with error distributions
ed_file_path <- "../../Outputs/Results/Error_Distributions/"

eds <- read_csv(paste0(ed_file_path, "all_distributions_h1.csv"))

eds <- eds %>% gather(key="name", value="values") %>%
  mutate(name = gsub("Multivariate_LGBM", "LGBM", name),
         name = gsub("k_nts_plus", "knts+", name),
         name = gsub("k_nts", "knts", name),
         name = substring(name, 1, nchar(name)-4)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter"), sep="_") %>%
  mutate(Parameter = if_else(is.na(Parameter), "Original", Parameter))

should_trim <- function(feat_vec, trim_pct){
  
  qs <- quantile(feat_vec, c(trim_pct, 1-trim_pct))
  
  indicator <- (feat_vec < qs[1]) | (feat_vec > qs[2])
  
  return(indicator)
}

# function for min-max scaling
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

model_acc_reg <- function(error_data, model_name, feats){
  
  error_data <- error_data %>%
    filter(Model==model_name) %>%
    bind_cols(feats) %>%
    select(-Model, -Horizon, -method, -parameter, -snum, -Parameter, -Protection)
  
  trim_data <- do.call(bind_cols, lapply(select(error_data, where(is.numeric)), should_trim, trim_pct=0.01))
  
  trim_data <- apply(trim_data, 1, any)
  
  error_data <- error_data %>%
    mutate(should_trim = trim_data) %>%
    filter(should_trim != TRUE) %>%
    select(-should_trim) %>%
    mutate(values = log(values))
  
  # error_data$values <- log(error_data$values)
  
  error_data <- error_data %>%
    mutate_if(is.numeric, normalize)
  
  mod <- lm(values ~ ., data=error_data)
  
  return(mod)
}

eds %>%
  group_by(Protection, Parameter) %>%
  summarize(n = n())

reg_features %>%
  group_by(method, parameter) %>%
  summarize(n = n())

temp <- eds %>%
  distinct(Protection, Parameter)

temp1 <- reg_features %>%
  distinct(method, parameter)

#############################################

error_data <- eds
model_name <- "TES"
feats <- reg_features

error_data <- error_data %>%
  filter(Model==model_name) %>%
  bind_cols(feats) %>%
  select(-Model, -Horizon, -method, -parameter, -snum, -Parameter, -Protection)

trim_data <- do.call(bind_cols, lapply(select(error_data, where(is.numeric)), should_trim, trim_pct=0.01))

trim_data <- apply(trim_data, 1, any)

error_data <- error_data %>%
  mutate(should_trim = trim_data) %>%
  filter(should_trim != TRUE) %>%
  select(-should_trim) %>%
  mutate(values = log(values))

error_data %>%
  mutate_if(is.numeric, normalize)

#############################################

# reg_features <- reg_features %>%
#   select(-snum, -method, -parameter)

# reg_feats <- do.call(bind_rows, lapply(1:7, function(x) reg_feats))

eds <- eds %>%
  mutate(Protection_Parameter = paste0(Protection, "_", Parameter),
         Protection_Parameter = factor(Protection_Parameter,
                                       levels=c("original_Original",
                                                "AN_0.25", "AN_0.5", "AN_1", "AN_1.5", "AN_2",
                                                "DP_20", "DP_10", "DP_4.6", "DP_1", "DP_0.1",
                                                "knts_3", "knts_5", "knts_7", "knts_10", "knts_15",
                                                "knts+_3", "knts+_5", "knts+_7", "knts+_10", "knts+_15"
                                                )))

# v <- eds %>%
#   filter(Parameter=="Original", Model=="RNN") %>%
#   pull(values)
# 
# h <- reg_features %>%
#   filter(method=="original") %>%
#   pull(hurst)

ses_mod <- model_acc_reg(eds, "SES", reg_features)
summary(ses_mod)

des_mod <- model_acc_reg(eds, "DES", reg_features)
summary(des_mod)

tes_mod <- model_acc_reg(eds, "TES", reg_features)
summary(tes_mod)

arima_mod <- model_acc_reg(eds, "ARIMA", reg_features)
summary(arima_mod)

var_mod <- model_acc_reg(eds, "VAR", reg_features)
summary(var_mod)

lgbm_mod <- model_acc_reg(eds, "LGBM", reg_features)
summary(lgbm_mod)

rnn_mod <- model_acc_reg(eds, "RNN", reg_features)
summary(rnn_mod)

library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(ses_mod, des_mod, tes_mod,
          arima_mod, var_mod, lgbm_mod, rnn_mod, p.style="stars", show.ci=FALSE,
          dv.labels=c("SES", "DES", "TES", "ARIMA", "VAR", "LGBM", "RNN"))


#############################################################################

###############################################################################

### Look at spectral densities before and after differential privacy is applied.
### Compare with the Hurst exponent.

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
dp_series1 <- process_data(read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_DP_20.csv"), sp=12)
dp_series2 <- process_data(read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_DP_10.csv"), sp=12)
dp_series3 <- process_data(read.csv("../../Data/Train/Clean/protected_m3_monthly_micro_h1_DP_4.6.csv"), sp=12)

density_plotter <- function(series_num, ts_data, entval, Hval){
  
  # The code for estimating the spectral density is used from the `entropy`
  # function in the tsfeatures package. Author: Rob hyndman
  s <- ts_data[[series_num]]
  
  s_spec <- spec.ar(s, plot=FALSE, method='burg', n.freq=ceiling(length(s)/2 + 1))
  
  fs <- c(rev(s_spec$spec[-1]),s_spec$spec)/ length(s)
  fs <- fs/sum(fs)
  prior.fs = rep(1 / length(fs), length = length(fs))
  prior.weight = 0.001
  fs <- (1 - prior.weight) * fs + prior.weight * prior.fs
  
  freq_s <- c(-rev(s_spec$freq[-1]), s_spec$freq)
  
  tibble(freq=freq_s, dens=fs) %>%
    ggplot(aes(x=freq, y=dens)) +
    geom_line() +
    labs(y="Spectral Density",
         x="Frequency") +
    ylim(0, 0.55) +
    annotate("text", x=3, y=0.2, label=paste0("Spec. Ent. = ", entval)) +
    annotate("text", x=3, y=0.14, label=paste0("H = ", Hval))
}

features %>%
  filter(Feature=="entropy", method=="Original") %>%
  arrange(Value) %>%
  slice(90:100)

features %>%
  filter(method %in% c("DP", "Original"), parameter %in% c("none", "20", "10", "4.6"), Feature %in% c("entropy", "hurst")) %>%
  filter(snum==85)

p1 <- density_plotter(85, orig_series, 0.527, 0.980)
p2 <- density_plotter(85, dp_series1, 0.686, 0.966)
p3 <- density_plotter(85, dp_series2, 0.845, 0.869)
p4 <- density_plotter(85, dp_series3, 1.000, 0.619)

ggarrange(p1, p2, p3, p4,
          labels=c("Original", "DP \u03f5 = 20", "DP \u03f5 = 10", "DP \u03f5 = 4.6"),
          nrow=2, ncol=2)
