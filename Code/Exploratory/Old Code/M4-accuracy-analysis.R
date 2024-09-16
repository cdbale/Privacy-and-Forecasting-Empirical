# Assess the accuracy results of the monthly-M4 data.

library(tidyverse)

# function to import and process series
import_data <- function(file_name, file_path, sp){
  
  ###
  # Takes the name file_name of a time series data set and the seasonal period
  # of that time series data. Imports the data, pre-processes and converts 
  # to a timeseries object, and returns the data.
  ###
  
  # import data and convert to a list of series
  ts_data <- as.list(as.data.frame(t(read.csv(paste0(file_path, file_name)))))
  
  # remove NA values from the end of each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # convert each series to a TS object with appropriate seasonal frequency
  ts_data <- lapply(ts_data, function(x) ts(x, frequency=sp))
  
  # truncate data to strictly positive
  ts_data <- lapply(ts_data, function(x) ifelse(x >= 1, x, 1))
  
  # take the log of the data
  ts_data <- lapply(ts_data, log)
  
  return(ts_data)
}

####################################################################################

# import all errors on monthly data. Using only SES, DES, TES right now
all_errors <- read_csv("../../Outputs/Results/M4/Error_Distributions/Monthly_all_distributions_h1.csv")

# transform to tidy
all_errors <- all_errors %>% gather(key="name", value="values") %>%
  mutate(name = substring(name, 1, nchar(name)-8)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_") %>%
  mutate(Protection = if_else(is.na(Protection), "Original", ifelse(Protection == "Monthly", "Original", Protection)),
         Parameter = if_else(is.na(Parameter), "Original", Parameter)) %>%
  select(-Data)

# store original errors in one dataframe
original_errors <- all_errors %>%
  filter(Protection == "Original")

# store protected errors in another data frame
protected_errors <- all_errors %>%
  filter(Protection != "Original")

# calculate the overall percent change in MAE for each privacy method
global_avg_MAE <- original_errors %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

protected_errors %>%
  group_by(Protection, Parameter) %>%
  summarize(global_protected_avg_MAE = mean(values),
            pct_change_mae = (global_protected_avg_MAE - global_avg_MAE)/global_avg_MAE * 100)

# now import the time series to calculate their lengths to identify the groupings
# for swapping. Calculate the grouping-specific accuracy to identify if there are
# subsets of time series that could be released with acceptable accuracy
# read in time series
X <- import_data(file_name="Monthly_h1_train.csv", file_path="../../Data/Cleaned/M4/", sp=12)

# split X into separate datasets, one for each series length
Xs <- list()
lengths <- unname(sapply(X, length))
unique_lengths <- unique(lengths)

# now merge the lengths with the original and protected errors
original_errors <- original_errors %>% group_split(Model)
original_errors <- do.call(bind_rows, lapply(original_errors, function(x) bind_cols(x, length=lengths)))

protected_errors <- protected_errors %>% group_split(Protection, Parameter, Model)
protected_errors <- do.call(bind_rows, lapply(protected_errors, function(x) bind_cols(x, length=lengths)))

# Calculate the grouping-specific accuracy to identify if there are
# subsets of time series that could be released with acceptable accuracy
# read in time series

# count the number of series corresponding to each length
length_counts <- original_errors %>%
  filter(Model == "DES") %>%
  group_by(length) %>%
  summarize(num_series = n(), .groups='drop')

# calculate the group level MAE and the number of series in each group
group_level_errors <- original_errors %>%
  group_by(length) %>%
  summarize(average_MAE = mean(values), .groups='drop') %>%
  left_join(length_counts, by="length")

# for the protected data, calculate the group level MAE
group_level_protected_errors <- protected_errors %>%
  group_by(Protection, Parameter, length) %>%
  summarize(average_protected_MAE = mean(values), .groups='drop')

all_group_level <- group_level_protected_errors %>%
  group_by(Protection, Parameter) %>%
  left_join(group_level_errors, by=c("length")) %>%
  ungroup() %>%
  mutate(pct_change_MAE = (average_protected_MAE - average_MAE)/average_MAE * 100) %>%
  filter(Protection == "k-nts-plus-M4") %>%
  arrange(pct_change_MAE)

write.csv(all_group_level, "../../Outputs/Results/Tables/m4_group_level_accuracy.csv")
  
releaseable <- group_level_protected_errors %>%
  group_by(Protection, Parameter) %>%
  left_join(group_level_errors, by=c("length")) %>%
  ungroup() %>%
  mutate(pct_change_MAE = (average_protected_MAE - average_MAE)/average_MAE * 100) %>%
  filter(Protection == "k-nts-plus-M4") %>%
  filter(pct_change_MAE < 25)

# now count how many series have each length
length_counts <- original_errors %>%
  filter(Model=="DES") %>%
  group_by(length) %>%
  summarize(num_series=n())

# join the length counts to the releaseable series
releaseable <- releaseable %>%
  left_join(length_counts, by="length")

# how many series can be released with acceptable accuracy
releaseable %>%
  summarize(num_releasable_series = sum(num_series))

# what are the lengths/subgroups of releaseable series
releaseable %>%
  pull(length)

releaseable %>%
  arrange(pct_change_MAE)

total_num_series <- nrow(filter(original_errors, Model=="DES"))

# what percentage of series are releaseable?
4988/total_num_series


## calculate the percentage of series with a percent increase in MAE less than 25%

series_orig_avg <- original_errors %>%
  group_by(Model, length) %>%
  mutate(snum=1:n()) %>%
  group_by(length, snum) %>%
  summarize(avg_AE = mean(values), .groups='drop')

series_prot_avg <- protected_errors %>%
  group_by(Model, Protection, Parameter, length) %>%
  mutate(snum=1:n()) %>%
  group_by(Protection, Parameter, length, snum) %>%
  summarize(prot_avg_AE = mean(values), .groups='drop')

series_combined_avg <- series_prot_avg %>%
  group_by(Protection, Parameter) %>%
  left_join(series_orig_avg, by=c("length", "snum")) %>%
  ungroup()

series_combined_avg %>%
  mutate(pct_change_ae = (prot_avg_AE - avg_AE)/avg_AE * 100) %>%
  filter(Protection == "k-nts-plus-M4", pct_change_ae < 25) %>%
  summarize(pct_released = n()/total_num_series * 100)






