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
  select(-swap5, -swap7, -swap10, -swap15) %>%
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
  # removing the time to generate forecasts per reviewer request
  select(-Baseline.Protection.Time, -SES_baseline, -DES_baseline, -TES_baseline, -ARIMA_baseline, -VAR_baseline, -LGBM_baseline, -RNN_baseline, -feature_extraction, -feature_prep, -error_computation) %>%
  gather(key="task", value="time", -num_swapped, -num_series, -avg_length, -Data, -Seasonal) %>%
  group_by(Data, num_swapped, num_series, avg_length, Seasonal) %>%
  summarize(total_time = sum(time), .groups='drop')
  
total_plot <- total_times %>%
  ggplot(aes(x=num_swapped, y=total_time)) +
  geom_point(size=2) +
  geom_smooth(method='lm') +
  labs(x = "Total Number Periods Swapped",
       y = "Total Time (Seconds)",
       title = "Computation Time vs. Total Number of Swapped Values") +
  theme(plot.title = element_text(size=9))

summary(lm(total_time ~ num_swapped, data=total_times))

summary(lm(RReliefF ~ num_series, data=all_times))
summary(lm(RFE ~ num_series, data=all_times))

summary(lm(swap3 ~ num_series + avg_length, data=all_times))

## data for k-nTS+ specific table
all_times 

## Bar plot showing the relative amount of time of each step

comp_breakdown <- all_times %>%
  select(Data, num_swapped, RFE, RReliefF, swap3) %>%
  gather(key='task', value='time', -Data, -num_swapped) %>%
  mutate(Step = ifelse(task == "RReliefF", "RReliefF",
                ifelse(task == "RFE", "RFE", "Swapping"))) %>%
  filter(Data == "monthly-MICRO") %>%
  arrange(desc(time)) %>%
  # reviewer requested to focus only on computation time of K-nTS+ algorithms
  ggplot(aes(x=factor(Step, levels=c("RFE", "Swapping", "RReliefF")), y=time)) +
  geom_col() +
  ylim(c(0, 1500)) +
  labs(x = "k-nTS+ Protection Step",
       y = "Time (Seconds)",
       title = "Computation Time for k-nTS+ Steps") +
  theme(plot.title = element_text(size=9))

combined_plot <- ggarrange(total_plot, comp_breakdown)

print(combined_plot)

if (file.exists(paste0("../../Outputs/Figures/M3/"))){
  ggsave(filename="computation_cost_plot.pdf", plot=combined_plot, path="../../Outputs/Figures/M3/")
} else {
  dir.create(paste0("../../Outputs/Figures/M3/"), recursive=TRUE)
  ggsave(filename="computation_cost_plot.pdf", plot=combined_plot, path="../../Outputs/Figures/M3/")
}
