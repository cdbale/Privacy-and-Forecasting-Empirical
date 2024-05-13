### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

################### Check file counts after all forecasting is done###################

# Author: Cameron Bale

library(tidyverse)
library(forecast)

data_folder <- "M3/"

# import_data <- function(data_folder, file_string, sp){
#   
#   ts <- read.csv(paste0("../../Data/Cleaned/", data_folder, file_string))
#   
#   td <- as.list(as.data.frame(t(ts)))
#   
#   td <- lapply(td, function(x) x[!is.na(x)])
#   
#   td <- lapply(td, function(x) ts(x, frequency=sp))
#   
#   td <- lapply(td, function(x) ifelse(x >= 1, x, 1))
#   
#   return(td)
#   
# }

# compute MASE. Use the denominator from the original data, numerators
# will be from the original and protected data. Take the difference to find
# the change in accuracy

# import real data

# compute denominator for each series using naive method

# use denominator and existing error distributions to compute MASE

# do we want the seasonal MASE for seasonal series?
# 
# # paths to the data files and feature files
# fp <- paste0("../../Data/Cleaned/", data_folder)
# 
# # import names of original data files - this may include protected versions
# # so we have to remove those
# file_names <- grep("_h1_train", list.files(fp), value=TRUE)
# # make sure protected versions are excluded
# file_names <- grep("AN_", file_names, value=TRUE, invert=TRUE)
# file_names <- grep("DP_", file_names, value=TRUE, invert=TRUE)
# file_names <- grep("k-nts", file_names, value=TRUE, invert=TRUE)
# 
# # these are the names of the original data files
# file_names
# 
# # write function to loop over time series and compute the denominator
# # of the MASE
# mase_denominators <- function(time_series_list, seasonal_period){
#   
#   if (seasonal_period > 1){
#     denoms <- sapply(time_series_list, function(x) mean(abs(snaive(y=x, h=1)$residuals), na.rm=TRUE))
#   } else {
#     denoms <- sapply(time_series_list, function(x) mean(abs(naive(y=x, h=1)$residuals), na.rm=TRUE))
#   }
#   
#   return(denoms)
# }
# 
# all_denoms <- tibble()
# 
# # loop over original files, computing the MASE denominators
# for (fname in file_names){
#   
#   file_prefix <- strsplit(fname, split="_")[[1]][1]
#   
#   sp <- ifelse(grepl("monthly", file_prefix), 12, ifelse(grepl("quarterly", file_prefix), 4, 1))
#   
#   X <- import_data(data_folder=data_folder, file_string=fname, sp=sp)
#   
#   denoms <- unname(mase_denominators(time_series_list = X, seasonal_period = sp))
#   
#   denoms_tibble <- tibble(Data=file_prefix,
#                           Snum=1:length(denoms),
#                           Denominators=denoms)
#   
#   all_denoms <- bind_rows(all_denoms, denoms_tibble)
# }

# now import mean absolute errors for all forecasts
error_dist_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

# import results files
res_files <- list.files(error_dist_path)

res_files <- grep("_all_distributions_h1", res_files, value=TRUE)
res_files <- grep("rate", res_files, value=TRUE)

all_results <- lapply(res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_protected_results <- lapply(all_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                      contains("_AN_", ignore.case=FALSE), 
                                                                      contains("k-nts", ignore.case=FALSE)))

# data frame for calculating protected results
all_original_results <- lapply(all_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

all_protected_results <- lapply(all_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
all_original_results <- lapply(all_original_results, function(x) bind_cols(x, Snum=1:nrow(x)))

# transform to tidy data
all_protected_results <- lapply(all_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                                                         mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                         separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data_Type", "Data"), sep="_"))

# transform to tidy data
all_original_results <- lapply(all_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                                                       mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                       separate(name, c("Model", "Horizon", "Data_Type", "Data"), sep="_"))

# combine into a single dataframe
all_protected_results <- do.call(rbind, all_protected_results)
all_original_results <- do.call(rbind, all_original_results)






# all_original_results <- all_original_results %>%
#   group_by(Model) %>%
#   left_join(all_denoms, by=c("Data", "Snum")) %>%
#   mutate(MASE=values/Denominators) %>%
#   ungroup()
# 
# all_protected_results <- all_protected_results %>%
#   group_by(Parameter, Model) %>%
#   left_join(all_denoms, by=c("Data", "Snum")) %>%
#   mutate(MASE=values/Denominators) %>%
#   ungroup()

write.csv(all_original_results, file="../../Outputs/Results/Tables/rate_all_original_results.csv", row.names = FALSE)
write.csv(all_protected_results, file="../../Outputs/Results/Tables/rate_all_protected_results.csv", row.names = FALSE)

################################################################################
################################################################################
################################################################################

# to_exclude <- all_protected_results %>%
#   group_by(Protection, Parameter, Model, Data) %>%
#   summarize(avg_MASE = mean(MASE), .groups='drop') %>%
#   arrange(desc(avg_MASE)) %>%
#   unite('file', Protection:Data) %>%
#   slice(1:5) %>%
#   pull(file)

# notice that there are some outlying errors (VAR tends to be the culprit)
# so we will exclude these from the averages
# somewhat subjective choice here...
# to_exclude <- all_protected_results %>%
#   group_by(Protection, Parameter, Model, Data) %>%
#   summarize(avg_AE = mean(values), .groups='drop') %>%
#   arrange(desc(avg_AE)) %>%
#   unite('file', Protection:Data) %>%
#   slice(1:13) %>%
#   pull(file)
# 
# # remove the large outlying errors
# all_protected_results <- all_protected_results %>%
#   select(Protection, Parameter, Model, Data, Horizon, values, Denominators, MASE) %>%
#   unite('file', Protection:Data) %>%
#   filter(!file %in% to_exclude) %>%
#   separate(file, c("Protection", "Parameter", "Model", "Data"), sep="_")
  








### calculate the average accuracy across all models and data sets
# for each privacy method
original_global_avg_mae <- all_original_results %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

protection_avgs <- all_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100,
         Parameter = as.numeric(Parameter)) %>%
  arrange(Protection, Parameter)

write.csv(protection_avgs, file="../../Outputs/Results/Tables/rate_protection_avgs.csv", row.names=FALSE)





### calculate the overall averages for each privacy method and model
original_avg_model <- all_original_results %>%
  group_by(Model) %>%
  summarize(original_avg_mae = mean(values), .groups="drop")

protection_model_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Model) %>%
  summarize(avg_mae = mean(values), .groups="drop") %>%
  left_join(original_avg_model, by="Model") %>%
  mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)








### calculate the overall averages for each privacy method, model, data set
original_avg_model_data <- all_original_results %>%
  group_by(Model, Data) %>%
  summarize(original_avg_mae = mean(values), .groups="drop")

protection_model_data_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Model, Data) %>%
  summarize(avg_mae = mean(values), .groups="drop") %>%
  left_join(original_avg_model_data, by=c("Model", "Data")) %>%
  mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)










### calculate overall average for each privacy method and data set
original_avg_data <- all_original_results %>%
  group_by(Data) %>%
  summarize(original_avg_mae = mean(values), .groups="drop")

protection_data_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Data) %>%
  summarize(avg_protected_mae = mean(values), .groups="drop") %>%
  left_join(original_avg_data, by=c("Data")) %>%
  mutate(pct_change_mae = (avg_protected_mae - original_avg_mae)/original_avg_mae * 100) %>%
  arrange(Data)

swrite.csv(protection_data_avgs, "../../Outputs/Results/Tables/averages_by_frequency.csv", row.names=FALSE)
