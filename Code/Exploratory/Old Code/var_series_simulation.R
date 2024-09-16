## Visualize instance space (based on features) of the original, k-nTS+
## protected (k = 3), and VAR simulated series from M3 monthly micro.

# Author: Cameron Bale

library(tidyverse)
library(tsfeatures)
library(e1071)

source('custom_feature_functions.R')

import_data <- function(file_string, sp){
  
  ts <- read.csv(file_string)
  
  td <- as.list(as.data.frame(t(ts)))
  
  td <- lapply(td, function(x) x[!is.na(x)])
  
  td <- lapply(td, function(x) ts(x, frequency=sp))
  
  td <- lapply(td, function(x) ifelse(x >= 1, x, 1))
  
  td <- lapply(td, log)
  
  return(td)
  
}

feature_calculator_seasonal <- function(ts, features_to_calculate, scale_series, sp){
  
  temp <- tsfeatures(ts, features=features_to_calculate, scale=scale_series) %>%
    select(-nperiods, -seasonal_period)
  
  return(temp)
}

################################################################################
################################################################################
################################################################################
################################################################################

# import data sets
mm <- import_data("../../Data/Cleaned/monthly-MICRO_h1_train.csv", 12)
knts_mm <- import_data("../../Data/Cleaned/preprocess-k-nts-plus_3_monthly-MICRO_h1_train.csv", 12)
var_mm <- import_data("../../Outputs/VAR Simulated/monthly-MICRO_h1_train.csv", 12)

# features to calculate
# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

# calculate features from each data set
mm_features <- feature_calculator_seasonal(mm, fv, scale=FALSE, 12)
knts_features <- feature_calculator_seasonal(knts_mm, fv, scale=FALSE, 12)
var_features <- feature_calculator_seasonal(var_mm, fv, scale=FALSE, 12)

# perform PCA on the original data
mm_pca <- prcomp(mm_features, center=FALSE, scale=FALSE)

summary(mm_pca)

mm_pcs <- as_tibble(mm_pca$x[,1:2]) %>%
  mutate(data="monthly-MICRO")

combined_pcs <- mm_pcs

pcs <- as_tibble(predict(mm_pca, knts_features)[,1:2]) %>%
    mutate(data="preprocess-k-nTS+ (k = 3)")
  
combined_pcs <- bind_rows(combined_pcs, pcs)
  
pcs <- as_tibble(predict(mm_pca, var_features)[,1:2]) %>%
  mutate(data="VAR Simulated")

combined_pcs <- bind_rows(combined_pcs, pcs) %>%
  mutate(data=factor(data,
                     levels=c("monthly-MICRO", "k-nTS+ (k = 3)", "VAR Simulated"),
                     labels=c("M3 Monthly Micro", "preprocess-k-nTS+ (k = 3)", "VAR Simulated")))

combined_pcs %>%
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point() +
  facet_wrap(~data) +
  labs(x = "Principal Component 1",
       y = "Principal Component 2",
       title="Feature Instance Space for k-nTS+ (k = 3) and VAR Simulated")

mm_pca








