## assess computational cost of k-nTS+ algorithm

library(tidyverse)
library(ggpubr)

# import computation cost spreadsheets
fcast_times <- read_csv("../../Data/Computation_Time/M3_computation_time.csv")
knts_times <- read_csv("../../Data/Computation_Time/M3_k-nts-plus.csv")

# calculate how many time periods were swapped in total
# (num_series x num_periods)
data_folder = "M3/"

# paths to the data files and feature files
fp <- paste0("../../Data/Cleaned/", data_folder)

# import names of original data files - this may include protected versions
# so we have to remove those
file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# make sure protected versions are excluded
file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)

length_counts <- list()
lengths <- list()

for (f in seq_along(file_names)){
  
  # import data and convert to a list of series
  ts_data <- as.list(as.data.frame(t(read.csv(paste0(fp, file_names[f])))))
  
  # remove NA values from the end of each series
  ts_data <- lapply(ts_data, function(x) x[!is.na(x)])
  
  # split into separate data sets, one for each series length
  ls <- c()
  lengths_temp <- sapply(ts_data, length)
  unique_lengths <- unique(lengths_temp)
  for (l in seq_along(unique_lengths)){
    ls <- c(ls, sum(lengths_temp==unique_lengths[l]))
  }
  
  length_counts[[f]] <- ls
  lengths[[f]] <- unique_lengths
}

# list "lengths" counts how many time periods are in each group of series
# list "length_counts" counts how many series have each length

# "num_swapped" is a vector containing how many time series values were
# swapped out
num_swapped <- c()
for (i in seq_along(length_counts)){
  num_swapped <- c(num_swapped, sum(length_counts[[i]] * lengths[[i]]))
}

file_counts <- tibble(file_name = file_names, 
                      num_swapped = num_swapped,
                      num_series = sapply(length_counts, sum),
                      avg_length = sapply(lengths, mean))

knts_times <- knts_times %>%
  left_join(file_counts, by = c("File"="file_name"))

fcast_times <- fcast_times %>%
  separate(File, c("Protection", "Parameter", "Data", "Horizon", "Data_Type"), sep="_")

sum_fcast_times <- fcast_times %>%
  group_by(Data) %>%
  summarize(across(Baseline.Protection.Time:feature_extraction, sum), .groups='drop') %>%
  select(-num_series)

all_times <- knts_times %>%
  mutate(Data = substr(File, start=1, stop=nchar(File)-13),
         Seasonal = ifelse(grepl("monthly", Data), "Yes", ifelse(grepl("quarterly", Data), "Yes", "No"))) %>%
  select(-File) %>%
  left_join(sum_fcast_times, by=c("Data"))

### create plots and tables of computation time

total_times <- all_times %>%
  gather(key="task", value="time", -num_swapped, -num_series, -avg_length, -Data, -Seasonal) %>%
  group_by(Data, num_swapped, num_series, avg_length, Seasonal) %>%
  summarize(total_time = sum(time), .groups='drop')

# what is the total time without the RNN model
total_times_no_rnn <- all_times %>%
  select(-RNN_baseline) %>%
  gather(key="task", value="time", -num_swapped, -num_series, -avg_length, -Data, -Seasonal) %>%
  group_by(Data, num_swapped, num_series, avg_length, Seasonal) %>%
  summarize(total_time = sum(time), .groups='drop') %>%
  arrange(desc(total_time))
  

total_plot <- total_times %>%
  ggplot(aes(x=num_swapped, y=total_time)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Total Number Periods Swapped",
       y = "Total Time (Seconds)",
       title = "Total Computation Time vs. Total Number of Protected Time Periods") +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

summary(lm(total_time ~ num_swapped, data=total_times))

summary(lm(RReliefF ~ num_series, data=all_times))
summary(lm(RFE ~ num_series, data=all_times))

summary(lm(Swap ~ num_series + avg_length, data=all_times))

## RReliefF as a function of number of series
relief_plot <- all_times %>%
  ggplot(aes(x=num_series, y=RReliefF)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Number of Time Series",
       y = "Time (Seconds)",
       title = "Total RReliefF Time vs. Number of Time Series")

## RReliefF as a function of number of time periods
# all_times %>%
#   ggplot(aes(x=avg_length, y=RReliefF)) +
#   geom_point(size=3) +
#   geom_smooth(method='lm')

## RReliefF as a function of number of series
rfe_plot <- all_times %>%
  ggplot(aes(x=num_series, y=RFE)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Number of Time Series",
       y = "Time (Seconds)",
       title = "Total RFE Time vs. Number of Time Series")

## RReliefF as a function of number of time periods
# all_times %>%
#   ggplot(aes(x=avg_length, y=RFE)) +
#   geom_point(size=3) +
#   geom_smooth(method='lm')

## Swapping time plot as a function of the number of time periods
swap_plot <- all_times %>%
  ggplot(aes(x=avg_length, y=Swap)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Number of Time Periods",
       y = "Time (Seconds)",
       title = "Total Swap Time vs. Average Series Length")

all_times %>%
  ggplot(aes(x=num_series, y=Swap)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Number of Time Periods",
       y = "Time (Seconds)",
       title = "Total Swap Time vs. Number of Time Periods")

## Times for forecasting with each model as a function of total number
## of time periods
# all_times %>%
#   select(Data, contains("baseline", ignore.case=FALSE), num_swapped) %>%
#   gather(key = "model", value="time", -Data, -num_swapped) %>%
#   ggplot(aes(x=num_swapped, y=time, color=model)) +
#   geom_line()

ggarrange(total_plot, relief_plot, rfe_plot, swap_plot)

## data for k-nTS+ specific table
all_times %>%
  select(Data, num_swapped, feature_extraction, RFE, RReliefF, Swap) %>%
  arrange(num_swapped)

## Bar plot showing the relative amount of time of each step
## need to classify each step

comp_breakdown <- all_times %>%
  gather(key='task', value='time', -Data, -num_swapped, -num_series, -avg_length, -Seasonal) %>%
  mutate(Step = ifelse(task == "RReliefF", "RReliefF",
                ifelse(task == "RFE", "RFE",
                ifelse(task == "Swap", "Swapping",
                ifelse(task %in% c("SES_baseline", "DES_baseline", "TES_baseline", "VAR_baseline"), "Other Forecasting",
                ifelse(task == "ARIMA_baseline", "Auto-ARIMA",
                ifelse(task == "LGBM_baseline", "LGBM",
                ifelse(task == "RNN_baseline", "RNN", "Other")))))))) %>%
  filter(Data == "monthly-MICRO") %>%
  group_by(Step) %>%
  summarize(total_time = sum(time)) %>%
  arrange(desc(total_time)) %>%
  ggplot(aes(x=factor(Step, levels=c("RNN", "Auto-ARIMA", "RFE", "Other Forecasting", "LGBM", "Swapping", "Other", "RReliefF")), y=total_time)) +
  geom_col() +
  labs(x = "Step",
       y = "Time (Seconds)",
       title = "Computation Time Breakdown for Protecting Monthly Micro Data") +
  ylim(0, 25000) +
  theme(plot.title = element_text(size=14, face= "bold", colour= "black" ),
        axis.title.x = element_text(size=13, face="bold", colour = "black"),    
        axis.title.y = element_text(size=13, face="bold", colour = "black"))

# how does the total time for feature selection and swapping compare
all_times %>%
  gather(key='task', value='time', -Data, -num_swapped, -num_series, -avg_length, -Seasonal) %>%
  mutate(Step = ifelse(task %in% c("RReliefF", "RFE", "Swap"), "k-nTS+",
                                     ifelse(task %in% c("SES_baseline", "DES_baseline", "TES_baseline", "VAR_baseline"), "Other Forecasting",
                                            ifelse(task == "ARIMA_baseline", "Auto-ARIMA",
                                                   ifelse(task == "LGBM_baseline", "LGBM",
                                                          ifelse(task == "RNN_baseline", "RNN", "Other")))))) %>%
  filter(Data == "monthly-MICRO") %>%
  group_by(Step) %>%
  summarize(total_time = sum(time)) %>%
  arrange(desc(total_time))

ggarrange(total_plot, comp_breakdown)
