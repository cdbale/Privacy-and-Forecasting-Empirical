## Analyze the features chosen by RReliefF and RFE

library(ggpubr)
library(tsfeatures)
library(e1071)
library(tidyverse)

m3_relief_directory <- "../../Outputs/RReliefF Rankings/M3/"
m3_rate_relief_directory <- "../../Outputs/RReliefF Rankings/M3_rate/"

m3_relief_files <- list.files(m3_relief_directory)
m3_rate_relief_files <- list.files(m3_rate_relief_directory)

m3_full_relief <- tibble()
m3_rate_full_relief <- tibble()

for (f in m3_relief_files){
  temp <- read_csv(paste0(m3_relief_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2])
  
  m3_full_relief <- bind_rows(m3_full_relief, temp)
}

for (f in m3_rate_relief_files){
  temp <- read_csv(paste0(m3_rate_relief_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2])
  
  m3_rate_full_relief <- bind_rows(m3_rate_full_relief, temp)
}

m3_avg_evals <- m3_full_relief %>%
  group_by(feature) %>%
  summarize(avg_value = mean(value)) %>%
  arrange(desc(avg_value))

m3_rate_avg_evals <- m3_rate_full_relief %>%
  group_by(feature) %>%
  summarize(avg_value = mean(value)) %>%
  arrange(desc(avg_value))

m3_sorted_names <- m3_avg_evals$feature

## note that you have to manually type out the feature names in the 
## correct order to have them 'nicely' formatted. The order changes
## slightly based on different runs of k-nTS+

m3_relief_plot <- m3_avg_evals %>%
  mutate(feature=factor(feature, 
                        levels=m3_sorted_names)) %>%
  ggplot(aes(x=feature, y=avg_value)) +
  ylim(-0.25, 0.6) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "RReliefF Weight",
       title = "M3 Data") +
  scale_x_discrete(labels=c("Spike",
                            "Max Variance Shift",
                            "Curvature",
                            "Variance",
                            "Max Level Shift",
                            "Linearity",
                            "Mean",
                            "Max KL Shift",
                            "Time Level Shift",
                            "Time KL Shift",
                            "Peak",
                            "Skewness",
                            "Kurtosis",
                            "Trough",
                            "Lumpiness",
                            "Time Variance Shift",
                            "Unitroot PP",
                            "X ACF",
                            "Hurst",
                            "Trend",
                            "Seasonal ACF",
                            "Flat Spots",
                            "Second Difference PACF5",
                            "Seasonal PACF",
                            "Crossing Points",
                            "Unitroot KPSS",
                            "Second Difference ACF10",
                            "First Difference ACF10",
                            "First Difference PACF5",
                            "Error ACF10",
                            "Second Difference ACF",
                            "Seasonal Strength",
                            "Error ACF",
                            "Stability",
                            "Nonlinearity",
                            "First Difference ACF",
                            "X PACF5",
                            "X ACF10",
                            "Entropy")) +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

m3_rate_relief_plot <- m3_rate_avg_evals %>%
  mutate(feature=factor(feature, 
                        levels=m3_sorted_names)) %>%
  ggplot(aes(x=feature, y=avg_value)) +
  ylim(-0.25, 0.6) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = "RReliefF Weight",
       title = "M3 Rate Data") +
  scale_x_discrete(labels=c("Spike",
                            "Max Variance Shift",
                            "Curvature",
                            "Variance",
                            "Max Level Shift",
                            "Linearity",
                            "Mean",
                            "Max KL Shift",
                            "Time Level Shift",
                            "Time KL Shift",
                            "Peak",
                            "Skewness",
                            "Kurtosis",
                            "Trough",
                            "Lumpiness",
                            "Time Variance Shift",
                            "Unitroot PP",
                            "X ACF",
                            "Hurst",
                            "Trend",
                            "Seasonal ACF",
                            "Flat Spots",
                            "Second Difference PACF5",
                            "Seasonal PACF",
                            "Crossing Points",
                            "Unitroot KPSS",
                            "Second Difference ACF10",
                            "First Difference ACF10",
                            "First Difference PACF5",
                            "Error ACF10",
                            "Second Difference ACF",
                            "Seasonal Strength",
                            "Error ACF",
                            "Stability",
                            "Nonlinearity",
                            "First Difference ACF",
                            "X PACF5",
                            "X ACF10",
                            "Entropy")) +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

ggarrange(m3_relief_plot, m3_rate_relief_plot)

################################################################################
################################################################################

# creating data set and feature heatmap

m3_oob_directory <- "../../Outputs/RFE OOB/M3/"
m3_rate_oob_directory <- "../../Outputs/RFE OOB/M3_rate/"

m3_oob_files <- list.files(m3_oob_directory)
m3_rate_oob_files <- list.files(m3_rate_oob_directory)

m3_full_oob <- tibble()
m3_rate_full_oob <- tibble()

for (f in m3_oob_files){
  temp <- read_csv(paste0(m3_oob_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2],
           data_subset = strsplit(f, "_")[[1]][3])
  
  m3_full_oob <- bind_rows(m3_full_oob, temp)
}

for (f in m3_rate_oob_files){
  temp <- read_csv(paste0(m3_rate_oob_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][3],
           data_subset = strsplit(f, "_")[[1]][4])
  
  m3_rate_full_oob <- bind_rows(m3_rate_full_oob, temp)
}

m3_monthly_micro_oob <- m3_full_oob %>%
  filter(data_set == "monthly-MICRO", data_subset == "1")

m3_rate_monthly_micro_oob <- m3_rate_full_oob %>%
  filter(data_set == "monthly-MICRO", data_subset == "1")

m3_oob_plot <- m3_monthly_micro_oob %>%
  ggplot(aes(x=num_features, y=value, color=model)) +
  geom_line() +
  geom_point() +
  ylim(0.0, 0.85) +
  xlim(0.0, 33) +
  labs(x = "Number of Features",
       y = "OOB MAE",
       title = "M3 Data",
       color = "Model") +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

m3_rate_oob_plot <- m3_rate_monthly_micro_oob %>%
  ggplot(aes(x=num_features, y=value, color=model)) +
  geom_line() +
  geom_point() +
  ylim(0.0, 0.85) +
  xlim(0.0, 33) +
  labs(x = "Number of Features",
       y = "",
       title = "M3 Rate Data",
       color = "Model") +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

ggarrange(m3_oob_plot, m3_rate_oob_plot)

################################################################################
################################################################################

# creating data set and feature heatmap

m3_rfe_directory <- "../../Outputs/RFE Rankings/M3/"
m3_rate_rfe_directory <- "../../Outputs/RFE Rankings/M3_rate/"

m3_rfe_files <- list.files(m3_rfe_directory)
m3_rate_rfe_files <- list.files(m3_rate_rfe_directory)

m3_full_rfe <- tibble()
m3_rate_full_rfe <- tibble()

for (f in m3_rfe_files){
  temp <- read_csv(paste0(m3_rfe_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2],
           data_subset = strsplit(f, "_")[[1]][3])
  
  m3_full_rfe <- bind_rows(m3_full_rfe, temp)
}

for (f in m3_rate_rfe_files){
  temp <- read_csv(paste0(m3_rate_rfe_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][3],
           data_subset = strsplit(f, "_")[[1]][4])
  
  m3_rate_full_rfe <- bind_rows(m3_rate_full_rfe, temp)
}

avg_m3_rfe_evals <- m3_full_rfe %>%
  group_by(data_set, data_subset, var) %>%
  summarize(avg_rank = mean(rank), .groups='drop') %>%
  arrange(data_set, data_subset, avg_rank)

avg_m3_rate_rfe_evals <- m3_rate_full_rfe %>%
  group_by(data_set, data_subset, var) %>%
  summarize(avg_rank = mean(rank), .groups='drop') %>%
  arrange(data_set, data_subset, avg_rank)

## replicate feature selection code from k-nTS+ to determine which features
## were actually selected

# for each data subset, calculate how many features were needed to achieve
# the minimum MAE
m3_ns <- m3_full_oob %>%
  group_by(data_set, data_subset, model) %>%
  mutate(min_error = min(value)) %>%
  ungroup() %>%
  filter(value == min_error) %>%
  group_by(data_set, data_subset) %>%
  summarize(ns = floor(mean(num_features)), .groups='drop')

m3_rate_ns <- m3_rate_full_oob %>%
  group_by(data_set, data_subset, model) %>%
  mutate(min_error = min(value)) %>%
  ungroup() %>%
  filter(value == min_error) %>%
  group_by(data_set, data_subset) %>%
  summarize(ns = floor(mean(num_features)), .groups='drop')

# what are the minimum and maximum numbers of features selected
m3_ns %>%
  summarize(min_n = min(ns),
            max_n = max(ns))

m3_rate_ns %>%
  summarize(min_n = min(ns),
            max_n = max(ns))

avg_m3_rfe_evals <- avg_m3_rfe_evals %>%
  mutate(combined_data = paste0(data_set, "-", data_subset))

avg_m3_rate_rfe_evals <- avg_m3_rate_rfe_evals %>%
  mutate(combined_data = paste0(data_set, "-", data_subset))

data_levels <- unique(avg_m3_rfe_evals$combined_data)
data_levels <- data_levels[-16]
data_levels <- c(data_levels, "other-OTHER-1")

m3_sf <- avg_m3_rfe_evals %>%
  left_join(m3_ns, by=c("data_set", "data_subset")) %>%
  group_by(data_set, data_subset) %>%
  mutate(var_order = 1:n(),
         is_selected = factor(ifelse(var_order <= ns, 1, 0), levels=c(0, 1), labels=c("No", "Yes")),
         combined_data = factor(combined_data,
                                levels = data_levels,
                                labels = c("Monthly Demographic 1",
                                           "Monthly Demographic 2",
                                           "Monthly Finance 1",
                                           "Monthly Finance 2",
                                           "Monthly Industry 1",
                                           "Monthly Industry 2",
                                           "Monthly Industry 3",
                                           "Monthly Macro 1",
                                           "Monthly Macro 2",
                                           "Monthly Macro 3",
                                           "Monthly Micro 1",
                                           "Monthly Micro 2",
                                           "Monthly Micro 3",
                                           "Monthly Other 1",
                                           "Monthly Other 2",
                                           "Quarterly Finance",
                                           "Quarterly Macro 1",
                                           "Quarterly Macro 2",
                                           "Quarterly Macro 3",
                                           "Quarterly Micro 1",
                                           "Quarterly Micro 2",
                                           "Yearly Demographic 1",
                                           "Yearly Demographic 2",
                                           "Yearly Demographic 3",
                                           "Yearly Demographic 4",
                                           "Yearly Finance",
                                           "Yearly Industry",
                                           "Yearly Macro",
                                           "Yearly Micro",
                                           "Other Other")),
         var = factor(var,
                      levels = m3_sorted_names,
                      labels=c("Spike",
                               "Max Variance Shift",
                               "Curvature",
                               "Variance",
                               "Max Level Shift",
                               "Linearity",
                               "Mean",
                               "Max KL Shift",
                               "Time Level Shift",
                               "Time KL Shift",
                               "Peak",
                               "Skewness",
                               "Kurtosis",
                               "Trough",
                               "Lumpiness",
                               "Time Variance Shift",
                               "Unitroot PP",
                               "X ACF",
                               "Hurst",
                               "Trend",
                               "Seasonal ACF",
                               "Flat Spots",
                               "Second Difference PACF5",
                               "Seasonal PACF",
                               "Crossing Points",
                               "Unitroot KPSS",
                               "Second Difference ACF10",
                               "First Difference ACF10",
                               "First Difference PACF5",
                               "Error ACF10",
                               "Second Difference ACF",
                               "Seasonal Strength",
                               "Error ACF",
                               "Stability",
                               "Nonlinearity",
                               "First Difference ACF",
                               "X PACF5",
                               "X ACF10",
                               "Entropy")))

m3_rate_sf <- avg_m3_rate_rfe_evals %>%
  left_join(m3_rate_ns, by=c("data_set", "data_subset")) %>%
  group_by(data_set, data_subset) %>%
  mutate(var_order = 1:n(),
         is_selected = factor(ifelse(var_order <= ns, 1, 0), levels=c(0, 1), labels=c("No", "Yes")),
         combined_data = factor(combined_data,
                                levels = data_levels,
                                labels = c("Monthly Demographic 1",
                                           "Monthly Demographic 2",
                                           "Monthly Finance 1",
                                           "Monthly Finance 2",
                                           "Monthly Industry 1",
                                           "Monthly Industry 2",
                                           "Monthly Industry 3",
                                           "Monthly Macro 1",
                                           "Monthly Macro 2",
                                           "Monthly Macro 3",
                                           "Monthly Micro 1",
                                           "Monthly Micro 2",
                                           "Monthly Micro 3",
                                           "Monthly Other 1",
                                           "Monthly Other 2",
                                           "Quarterly Finance",
                                           "Quarterly Macro 1",
                                           "Quarterly Macro 2",
                                           "Quarterly Macro 3",
                                           "Quarterly Micro 1",
                                           "Quarterly Micro 2",
                                           "Yearly Demographic 1",
                                           "Yearly Demographic 2",
                                           "Yearly Demographic 3",
                                           "Yearly Demographic 4",
                                           "Yearly Finance",
                                           "Yearly Industry",
                                           "Yearly Macro",
                                           "Yearly Micro",
                                           "Other Other")),
         var = factor(var,
                      levels = m3_sorted_names,
                      labels=c("Spike",
                               "Max Variance Shift",
                               "Curvature",
                               "Variance",
                               "Max Level Shift",
                               "Linearity",
                               "Mean",
                               "Max KL Shift",
                               "Time Level Shift",
                               "Time KL Shift",
                               "Peak",
                               "Skewness",
                               "Kurtosis",
                               "Trough",
                               "Lumpiness",
                               "Time Variance Shift",
                               "Unitroot PP",
                               "X ACF",
                               "Hurst",
                               "Trend",
                               "Seasonal ACF",
                               "Flat Spots",
                               "Second Difference PACF5",
                               "Seasonal PACF",
                               "Crossing Points",
                               "Unitroot KPSS",
                               "Second Difference ACF10",
                               "First Difference ACF10",
                               "First Difference PACF5",
                               "Error ACF10",
                               "Second Difference ACF",
                               "Seasonal Strength",
                               "Error ACF",
                               "Stability",
                               "Nonlinearity",
                               "First Difference ACF",
                               "X PACF5",
                               "X ACF10",
                               "Entropy")))

### now plot on heatmap

# rows to be features

# columns to be data sets

# squares to be colored if the feature was selected

# intensity of the color should correspond to the rank of the selected feature

m3_selected_grid <- m3_sf %>%
  ggplot(aes(x=combined_data, y=var, fill=as.factor(is_selected))) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) +
  coord_fixed() +
  labs(x = "Data Subset",
       y = "Feature",
       fill = "Selected",
       title="M3 Data") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        text = element_text(size=19),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black"))

m3_rate_selected_grid <- m3_rate_sf %>%
  ggplot(aes(x=combined_data, y=var, fill=as.factor(is_selected))) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) +
  coord_fixed() +
  labs(x = "Data Subset",
       y = "",
       fill = "Selected",
       title="M3 Rate Data") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        text = element_text(size=19),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black"))

ggarrange(m3_selected_grid, m3_rate_selected_grid)

################################################################################
################################################################################
################################################################################

## may be make a 4x3 plot (columns will be protected versions, rows will be
## series, i.e., good, good rate, bad, bad rate)

# analyze individual time series on features, then expand to principal components
################################

# import the original time series features and find a good/bad series (based
# on features) to illustrate

# import the features from protected data
# combine into one dataframe, with indicators for privacy method and parameter
m3_file_names <- grep("monthly-MICRO_h1_train", list.files("../../Data/Features/M3/"), value=TRUE)

m3_full_features <- tibble()

for (f in m3_file_names){
  
  features <- read_csv(paste0("../../Data/Features/M3/", f))
  
  params <- strsplit(f, split="_")
  
  features["snum"] <- 1:nrow(features)
  if (!params[[1]][2] %in% c("AN", "DP", "k-nts", "k-nts-plus", "k-nts-plus-bounded")){
    features["method"] <- "Original"
    features["parameter"] <- "Original"
  } else {
    features["method"] <- params[[1]][2]
    features["parameter"] <- params[[1]][3]
  }
  
  features["data"] <- params[[1]][length(params[[1]])-2]
  
  m3_full_features <- bind_rows(m3_full_features, features)
  
}

## repeat for the rate data

# import the features from protected data
# combine into one dataframe, with indicators for privacy method and parameter
m3_rate_file_names <- grep("monthly-MICRO_h1_train", list.files("../../Data/Features/M3_rate/"), value=TRUE)

m3_rate_full_features <- tibble()

for (f in m3_rate_file_names){
  
  features <- read_csv(paste0("../../Data/Features/M3_rate/", f))
  
  params <- strsplit(f, split="_")
  
  features["snum"] <- 1:nrow(features)
  if (!params[[1]][2] %in% c("AN", "DP", "k-nts", "k-nts-plus", "k-nts-plus-bounded")){
    features["method"] <- "Original"
    features["parameter"] <- "Original"
  } else {
    features["method"] <- params[[1]][2]
    features["parameter"] <- params[[1]][3]
  }
  
  features["data"] <- params[[1]][length(params[[1]])-2]
  
  m3_rate_full_features <- bind_rows(m3_rate_full_features, features)
  
}

### visualize a good/bad series (based on feature values)

process_data <- function(time_series, sp, truncate_and_log=FALSE){
  
  # convert to a list of series
  ts_data <- as.list(as.data.frame(t(time_series)))
  
  # remove NA values from each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  if (truncate_and_log){
    # truncate data to strictly positive
    ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
    
    # take the log of the data
    ts_data <- lapply(ts_data, log)
  }
  
  return(ts_data)
}

gs_num <- m3_full_features %>%
  filter(parameter == 'Original') %>%
  arrange(entropy) %>%
  select(snum) %>%
  slice(1) %>%
  pull(snum)

bs_num <- m3_full_features %>%
  filter(parameter == 'Original') %>%
  arrange(desc(entropy)) %>%
  select(snum) %>%
  slice(1) %>%
  pull(snum)

orig_series <- process_data(read.csv("../../Data/Cleaned/M3/monthly-MICRO_h1_train.csv"), sp=12, truncate_and_log=TRUE)
an_series <- process_data(read.csv("../../Data/Cleaned/M3/AN_1.5_monthly-MICRO_h1_train.csv"), sp=12, truncate_and_log=TRUE)
kntsp_series <- process_data(read.csv("../../Data/Cleaned/M3/k-nts-plus-bounded_3-1.5_monthly-MICRO_h1_train.csv"), sp=12, truncate_and_log=TRUE)
orig_rate_series <- process_data(read.csv("../../Data/Cleaned/M3_rate/rate_monthly-MICRO_h1_train.csv"), sp=12)
an_rate_series <- process_data(read.csv("../../Data/Cleaned/M3_rate/AN_1.5_rate_monthly-MICRO_h1_train.csv"), sp=12)
kntsp_rate_series <- process_data(read.csv("../../Data/Cleaned/M3_rate/k-nts-plus_3_rate_monthly-MICRO_h1_train.csv"), sp=12)

gs_plot <- tibble(x = orig_series[[gs_num]], t = 1:length(orig_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features",
       x = 'Time',
       y = 'x')

bs_plot <- tibble(x = orig_series[[bs_num]], t = 1:length(orig_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features",
       x = 'Time',
       y = "")

gs_knts_plot <- tibble(x = kntsp_series[[gs_num]], t = 1:length(kntsp_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features (k-nTS+, k = 3, M = 1.5)",
       x = 'Time',
       y = 'x')

bs_knts_plot <- tibble(x = kntsp_series[[bs_num]], t = 1:length(kntsp_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features (k-nTS+, k = 3, M = 1.5)",
       x = 'Time',
       y = "")

gs_an_plot <- tibble(x = an_series[[gs_num]], t = 1:length(an_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Desirable Features (AN, s = 1)",
       x = 'Time',
       y = 'x')

bs_an_plot <- tibble(x = an_series[[bs_num]], t = 1:length(an_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(0, 10) +
  labs(title="Series with Undesirable Features (AN, s = 1)",
       x = 'Time',
       y = "")

# unprotected versions
g1 <- ggarrange(gs_plot, bs_plot, nrow=1, labels=c("A", "B"))

annotate_figure(g1, top=text_grob("", face = "bold", size = 14))

g2 <- ggarrange(gs_plot, bs_plot,
                gs_knts_plot, bs_knts_plot,
                gs_an_plot, bs_an_plot,
                nrow=3, ncol=2, labels=c("A.1", "B.1", "A.2", "B.2", "A.3", "B.3"))

annotate_figure(g2, top=text_grob("", face = "bold", size = 14))

# feature values for the example series.

sf <- c("entropy", "e_acf1", "trend", "seasonal_strength",
        "skewness", "kurtosis", "hurst",
        "series_mean", "series_variance", "spike", "max_level_shift", "max_var_shift")

gs_orig <- m3_full_features %>%
  filter(parameter == 'Original', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

gs_orig

gs_kntsp <- m3_full_features %>%
  filter(method=="k-nts-plus-bounded", parameter == '3-1.5', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

gs_kntsp

gs_an <- m3_full_features %>%
  filter(method=='AN', parameter == '1.5', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

gs_an

#################################################################################

# repeat for the rate version of the series

rgs_plot <- tibble(x = orig_rate_series[[gs_num]], t = 1:length(orig_rate_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Desirable Features",
       x = 'Time',
       y = 'x')

rbs_plot <- tibble(x = orig_rate_series[[bs_num]], t = 1:length(orig_rate_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Undesirable Features",
       x = 'Time',
       y = "")

rgs_knts_plot <- tibble(x = kntsp_rate_series[[gs_num]], t = 1:length(kntsp_rate_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Desirable Features (k-nTS+, k = 3)",
       x = 'Time',
       y = 'x')

rbs_knts_plot <- tibble(x = kntsp_rate_series[[bs_num]], t = 1:length(kntsp_rate_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Undesirable Features (k-nTS+, k = 3)",
       x = 'Time',
       y = "")

rgs_an_plot <- tibble(x = an_rate_series[[gs_num]], t = 1:length(an_rate_series[[gs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Desirable Features (AN, s = 1)",
       x = 'Time',
       y = 'x')

rbs_an_plot <- tibble(x = an_rate_series[[bs_num]], t = 1:length(an_rate_series[[bs_num]])) %>%
  ggplot(aes(x=t, y=x)) +
  geom_line(linewidth=.7, color="#3399CC") +
  geom_point() +
  ylim(-0.6, 0.6) +
  labs(title="Series with Undesirable Features (AN, s = 1)",
       x = 'Time',
       y = "")

# unprotected versions
rg1 <- ggarrange(rgs_plot, rbs_plot, nrow=1)

annotate_figure(rg1, top=text_grob("", face = "bold", size = 14))

rg2 <- ggarrange(rgs_plot, rbs_plot,
                 rgs_knts_plot, rbs_knts_plot,
                 rgs_an_plot, rbs_an_plot,
                 nrow=3, ncol=2, labels=c("A.1", "B.1", "A.2", "B.2", "A.3", "B.3"))

annotate_figure(rg2, top=text_grob("", face = "bold", size = 14))

# feature values for the example series.

rgs_orig <- m3_rate_full_features %>%
  filter(parameter == 'Original', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

rgs_orig

rgs_kntsp <- m3_rate_full_features %>%
  filter(method=="k-nts-plus", parameter == '3', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

rgs_kntsp

rgs_an <- m3_rate_full_features %>%
  filter(method=='AN', parameter == '1.5', snum %in% c(gs_num, bs_num)) %>%
  select(any_of(c("snum", sf)))

rgs_an

#################################################################################
#################################################################################
#################################################################################

# compare PCA plots and loadings from k-nTS+ and unprotected series

# 2x2 plot

# original features dataframe
feature_file_path <- "../../Data/Features/M3/"

og_feat_files <- grep("k-nts", list.files(feature_file_path), value=TRUE, invert=TRUE)
og_feat_files <- grep("DP_", og_feat_files, value=TRUE, invert=TRUE)
og_feat_files <- grep("AN_", og_feat_files, value=TRUE, invert=TRUE)
og_feat_files <- grep("h1_train", og_feat_files, value=TRUE)

full_og_feats <- tibble()

for (f in og_feat_files){
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- read_csv(paste0(feature_file_path, f)) %>%
    mutate(data = strsplit(f, "_")[[1]][2])
  if (sp %in% c(12, 4)){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  full_og_feats <- bind_rows(full_og_feats, temp)
}

# k-nTS features data frame

kntsp_feat_files <- grep("k-nts-plus-bounded_3-1.5", list.files(feature_file_path), value=TRUE)

full_kntsp_feats <- tibble()

for (f in kntsp_feat_files){
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- read_csv(paste0(feature_file_path, f)) %>%
    mutate(data = strsplit(f, "_")[[1]][4])
  if (sp %in% c(12, 4)){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  full_kntsp_feats <- bind_rows(full_kntsp_feats, temp)
}

# k-nTS features data frame

knts_feat_files <- grep("k-nts_3", list.files(feature_file_path), value=TRUE)

full_knts_feats <- tibble()

for (f in knts_feat_files){
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- read_csv(paste0(feature_file_path, f)) %>%
    mutate(data = strsplit(f, "_")[[1]][4])
  if (sp %in% c(12, 4)){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  full_knts_feats <- bind_rows(full_knts_feats, temp)
}

# AN features data frame

an_feat_files <- grep("AN_1.5_", list.files(feature_file_path), value=TRUE)
an_feat_files <- grep("h1_train", an_feat_files, value=TRUE)

full_an_feats <- tibble()

for (f in an_feat_files){
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- read_csv(paste0(feature_file_path, f)) %>%
    mutate(data = strsplit(f, "_")[[1]][4])
  if (sp %in% c(12, 4)){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  full_an_feats <- bind_rows(full_an_feats, temp)
}

# DP features data frame

dp_feat_files <- grep("DP_4.6_", list.files(feature_file_path), value=TRUE)
dp_feat_files <- grep("h1_train", dp_feat_files, value=TRUE)

full_dp_feats <- tibble()

for (f in dp_feat_files){
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- read_csv(paste0(feature_file_path, f)) %>%
    mutate(data = strsplit(f, "_")[[1]][4])
  if (sp %in% c(12, 4)){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  full_dp_feats <- bind_rows(full_dp_feats, temp)
}

# VAR simulated features data frame

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

feature_calculator <- function(ts, features_to_calculate, sp, keep_seasonal=FALSE){
  
  temp <- tsfeatures(ts, features=features_to_calculate, scale=FALSE) %>%
    select(-nperiods, -seasonal_period)
  
  if (sp > 1 & !keep_seasonal){
    temp <- temp %>%
      select(-seasonal_strength, -peak, -trough, -seas_acf1, -seas_pacf)
  }
  
  return(temp)
}

var_sim_series <- grep("k-nts", list.files("../../Outputs/VAR Simulated/M3/"), value=TRUE, invert=TRUE)
var_sim_series <- grep("AN_", var_sim_series, value=TRUE, invert=TRUE)
var_sim_series <- grep("DP_", var_sim_series, value=TRUE, invert=TRUE)
var_sim_series <- grep("rate", var_sim_series, value=TRUE, invert=TRUE)

# features to calculate
# vector of feature names to calculate in k-nTS+
fv <- c("entropy_c", "lumpiness", "stability",
        "max_level_shift_c", "max_var_shift_c", "max_kl_shift_c",
        "crossing_points", "flat_spots", "hurst",
        "unitroot_kpss", "unitroot_pp", "stl_features",
        "acf_features", "pacf_features",
        "nonlinearity", "series_mean", "series_variance",
        "skewness", "kurtosis")

full_var_feats <- tibble()

for (f in var_sim_series){
  print(f)
  sp <- ifelse(grepl("monthly", f), 12, ifelse(grepl("quarterly", f), 4, 1))
  temp <- import_data(paste0("../../Outputs/VAR Simulated/M3/", f), sp)
  contains_infinity <- sapply(temp, function(x) sum(is.infinite(x))) == 0
  temp <- temp[contains_infinity]
  if (grepl("monthly-MICRO", f)){
    var_sim_monthly_micro_feats <- feature_calculator(temp, fv, sp, keep_seasonal=TRUE) %>%
      mutate(data = strsplit(f, "_")[[1]][1])
  }
  temp_feats <- feature_calculator(temp, fv, sp) %>%
    mutate(data = strsplit(f, "_")[[1]][1])
  full_var_feats <- bind_rows(full_var_feats, temp_feats)
}

## combine feature data frames, standardize, and perform PCA

full_og_feats <- full_og_feats %>%
  mutate(series = "Original")

full_kntsp_feats <- full_kntsp_feats %>%
  mutate(series= "k-nTS+")

full_knts_feats <- full_knts_feats %>%
  mutate(series = 'k-nTS')

full_var_feats <- full_var_feats %>%
  mutate(series = "VAR Sim")

full_an_feats <- full_an_feats %>%
  mutate(series = "AN")

full_dp_feats <- full_dp_feats %>%
  mutate(series = "DP")

full_feats <- full_og_feats %>%
  bind_rows(full_kntsp_feats, full_knts_feats, full_var_feats, full_an_feats, full_dp_feats)

## individual feature boxplots
## need to include seasonal feature
var_sim_monthly_micro_feats <- var_sim_monthly_micro_feats %>%
  mutate(snum = 1:n(),
         method = "VAR Sim",
         parameter = "1")

seasonal_mm_feats <- m3_full_features %>%
  filter(method %in% c("AN", "DP", "k-nts", "k-nts-plus-bounded", "Original")) %>%
  bind_rows(var_sim_monthly_micro_feats)

seasonal_mm_feats %>%
  select(method, data, spike, series_variance, max_var_shift, curvature, max_level_shift, linearity, 
         series_mean, max_kl_shift, time_level_shift, skewness, lumpiness, unitroot_pp, x_acf1, seas_acf1, seas_pacf, diff1_acf10) %>%
  gather(key="Feature", value="Value", -method, -data) %>%
  mutate(Feature = factor(Feature, 
                          levels=c("spike",
                                   "series_variance",
                                   "max_var_shift",
                                   "curvature",
                                   "max_level_shift",
                                   "linearity",
                                   "series_mean",
                                   "max_kl_shift",
                                   "time_level_shift",
                                   "skewness",
                                   "lumpiness",
                                   "unitroot_pp",
                                   "x_acf1",
                                   "seas_acf1",
                                   "seas_pacf",
                                   "diff1_acf10"),
                          labels=c("Spike",
                                   "Variance",
                                   "Max Variance Shift",
                                   "Curvature",
                                   "Max Level Shift",
                                   "Linearity",
                                   "Mean",
                                   "Max KL Shift",
                                   "Time Level Shift",
                                   "Skewness",
                                   "Lumpiness",
                                   "Unitroot PP",
                                   "X ACF",
                                   "Seasonal ACF",
                                   "Seasonal PACF",
                                   "First Difference ACF10"))) %>%
  ggplot(aes(x=factor(method, levels=c("Original", "VAR Sim", "k-nts-plus-bounded", "k-nts", "AN", "DP"), labels=c("Unprotected", "VAR Sim", "k-nTS+", "k-nTS", "AN", "DP")), y=Value)) +
  geom_boxplot() +
  facet_wrap(~Feature, scales='free') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom",
        text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black")) +
  labs(x = "Privacy Method",
       y = "Feature Value")

### look at the most frequently selected features plotted using PCA

m3_sf %>%
  filter(is_selected == "Yes") %>%
  group_by(var) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# PCA plots of features from k-nTS+ original and rate data sets
full_pca <- prcomp(full_feats[,names(full_feats) %in% c('max_var_shift',
                                                        'series_variance',
                                                        'max_level_shift',
                                                        'spike',
                                                        'series_mean',
                                                        'kurtosis')], center=TRUE, scale=TRUE)

summary(full_pca)

pcs <- as_tibble(full_pca$x[,1:2]) %>%
  bind_cols(full_feats[,c('data', 'series', 'entropy')])

pcs %>%
  mutate(series = factor(series, levels=c("Original", "VAR Sim", "k-nTS+", "k-nTS", "AN", "DP"), labels=c("Unprotected", "VAR Sim", "k-nTS+", "k-nTS", "AN", "DP"))) %>%
  ggplot(aes(x=PC1, y=PC2, color=entropy)) +
  geom_point(alpha=0.5) +
  facet_wrap(~series) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black")) +
  labs(x  = "Principal Component 1",
       y = "Principal Component 2",
       color = "Spectral Entropy")

full_pca$rotation[,1:2]

# calculate average spectral entropy value for each privacy method
pcs %>%
  group_by(series) %>%
  summarize(avg_entropy = mean(entropy))



### perform PCA using all features
# PCA plots of features from k-nTS+ original and rate data sets
full_pca <- prcomp(full_feats[full_feats$series %in% c("Original", "VAR Sim", "k-nTS+"), !names(full_feats) %in% c('time_level_shift',
                                                                                                                 'time_var_shift',
                                                                                                                 'time_kl_shift',
                                                                                                                 'data',
                                                                                                                 'series')], center=TRUE, scale=TRUE)

summary(full_pca)

pcs <- as_tibble(full_pca$x[,1:2]) %>%
  bind_cols(full_feats[full_feats$series %in% c("Original", "VAR Sim", "k-nTS+"),c('data', 'series', 'entropy')])

pcs %>%
  mutate(series = factor(series, levels=c("Original", "VAR Sim", "k-nTS+", "k-nTS", "AN", "DP"), labels=c("Unprotected", "VAR Sim", "k-nTS+", "k-nTS", "AN", "DP"))) %>%
  ggplot(aes(x=PC1, y=PC2, color=entropy)) +
  geom_point(alpha=0.5) +
  facet_wrap(~series) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"),    
        axis.title.y = element_text(face="bold", colour = "black")) +
  labs(x  = "Principal Component 1",
       y = "Principal Component 2",
       color = "Spectral Entropy")

library(ash)

x <- as.matrix(pcs[,1:2])

ab <- matrix(c(min(x[,1]), min(x[,2]), max(x[,1]), max(x[,2])), 2, 2)

nbin <- c(30, 30)

original_bins <- bin2(x[pcs$series == "Original",], ab, nbin)[[1]]

var_bins <- bin2(x[pcs$series == "VAR Sim",], ab, nbin)[[1]]

kntsplus_bins <- bin2(x[pcs$series == "k-nTS+",], ab, nbin)[[1]]

# calculate KL-divergence of feature distributions of first two
# principal components

original_bins <- original_bins/sum(original_bins) + .Machine$double.eps

var_bins <- var_bins/sum(var_bins) + .Machine$double.eps

kntsplus_bins <- kntsplus_bins/sum(kntsplus_bins) + .Machine$double.eps

# original_entropy <- sum(original_bins * log2(original_bins))
#
# kl <- sum(original_bins * (log2(original_bins) - log2(var_bins)))
#
# kl/original_entropy * 100
#
# kl <- sum(original_bins * (log2(original_bins) - log2(kntsplus_bins)))
#
# kl/original_entropy * 100

library(entropy)


var_kl <- round(KL.Dirichlet(as.vector(original_bins), as.vector(var_bins), a1=0, a2=0, unit="log2"),3)
ent <- entropy.Dirichlet(as.vector(original_bins), a=0, unit="log2")
var_pct_change <- var_kl/ent * 100

knts_kl <- round(KL.Dirichlet(as.vector(original_bins), as.vector(kntsplus_bins), a1=0, a2=0, unit="log2"),3)
knts_pct_change <- knts_kl/ent * 100









