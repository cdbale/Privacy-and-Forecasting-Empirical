### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

## note that we exclude the VAR yearly-micro data ##

# Author: Cameron Bale

library(tidyverse)
library(forecast)

data_folder <- "M3/"

# now import mean absolute errors for all forecasts
error_dist_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

# import results files
res_files <- list.files(error_dist_path)
res_files <- grep("_all_distributions_h1", res_files, value=TRUE)

all_results <- lapply(res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_protected_results <- lapply(all_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                      contains("_AN_", ignore.case=FALSE), 
                                                                      contains("k-nts", ignore.case=FALSE)))

var_protected_results <- lapply(all_results, function(x) x %>% select(contains("var-an", ignore.case=FALSE),
                                                                      contains("var-knts", ignore.case=FALSE),
                                                                      contains("var-sim", ignore.case=FALSE)))

# data frame for calculating protected results
all_original_results <- lapply(all_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x[,grep("var-an", colnames(x[,grep("var-sim", colnames(x[,grep("var-knts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

all_protected_results <- lapply(all_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
var_protected_results <- lapply(var_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
all_original_results <- lapply(all_original_results, function(x) bind_cols(x, Snum=1:nrow(x)))

# transform to tidy data
all_protected_results <- lapply(all_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                                                         mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                         separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_"))

var_protected_results <- lapply(var_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                  mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_"))

# transform to tidy data
all_original_results <- lapply(all_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                                                       mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                       separate(name, c("Model", "Horizon", "Data"), sep="_"))

# combine into a single dataframe
all_protected_results <- do.call(rbind, all_protected_results)
var_protected_results <- do.call(rbind, var_protected_results)
all_original_results <- do.call(rbind, all_original_results)

# import and combine magnitudes
magnitude_path <- paste0("../../Data/Magnitudes/", data_folder)
magnitude_files <- list.files(magnitude_path)

# loop over magnitude files, import, and create variable identifying
# the data set
magnitudes <- tibble()

for (f in magnitude_files){
  df <- read_csv(paste0(magnitude_path, f)) %>%
    select(-`0`) %>%
    mutate(Data = strsplit(f, split="_")[[1]][1])
  
  magnitudes <- bind_rows(magnitudes, df)
}

all_original_results <- all_original_results %>%
  left_join(magnitudes, by=c("Data", "Snum"="snum"))

all_protected_results <- all_protected_results %>%
  left_join(magnitudes, by=c("Data", "Snum"="snum"))

var_protected_results <- var_protected_results %>%
  left_join(magnitudes, by=c("Data", "Snum"="snum"))

# remove yearly micro VAR results
all_protected_results <- all_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

all_original_results <- all_original_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

var_protected_results <- var_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
} else {
  dir.create(paste0("../../Outputs/Results/", data_folder, "Tables/"))
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
}

################################################################################
################################################################################
################################################################################

# notice that there are some outlying errors (VAR tends to be the culprit)
# so we will exclude these from the averages
# somewhat subjective choice here...
# we choose to exclude any cases where the forecast error was over 100,000

to_exclude <- all_protected_results %>%
  group_by(Protection, Parameter, Model, Data) %>%
  summarize(avg_AE = mean(values), .groups='drop') %>%
  arrange(desc(avg_AE)) %>%
  unite('file', Protection:Data) %>%
  slice(1:11) %>%
  pull(file)

# remove the large outlying errors
all_protected_results <- all_protected_results %>%
  select(Protection, Parameter, Model, Data, Horizon, Snum, values, large_magnitude) %>%
  unite('file', Protection:Data) %>%
  filter(!file %in% to_exclude) %>%
  separate(file, c("Protection", "Parameter", "Model", "Data"), sep="_")

### calculate the average accuracy across all models and data sets
# for each privacy method
original_global_avg_mae <- all_original_results %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

protection_avgs <- all_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100) %>%
  arrange(Protection, Parameter)

write.csv(protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/avg_accuracy_by_protection.csv"), row.names=FALSE)

var_original_global_avg_mae <- all_original_results %>%
  filter(Model == "VAR") %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

var_protection_avgs <- var_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=var_original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100) %>%
  arrange(Protection, Parameter)

write.csv(var_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/var_avg_accuracy_by_protection.csv"), row.names=FALSE)

## calculate again for large vs. small magnitude series

magnitude_orig_mae <- all_original_results %>%
  group_by(large_magnitude) %>%
  summarize(global_avg_MAE = mean(values))
  
magnitude_protected_results <- all_protected_results %>%
  group_by(Protection, Parameter, large_magnitude) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  left_join(magnitude_orig_mae, by="large_magnitude") %>%
  mutate(percent_change_mae = (global_avg_MAE.x - global_avg_MAE.y)/global_avg_MAE.y * 100) %>%
  arrange(Protection, Parameter)

write.csv(magnitude_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/avg_accuracy_by_magnitude_protection.csv"), row.names=FALSE)

var_original_magnitude_avg_mae <- all_original_results %>%
  filter(Model == "VAR") %>%
  group_by(large_magnitude) %>%
  summarize(global_avg_MAE = mean(values))

var_magnitude_protection_avgs <- var_protected_results %>%
  group_by(Protection, Parameter, large_magnitude) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  left_join(magnitude_orig_mae, by="large_magnitude") %>%
  mutate(percent_change_mae = (global_avg_MAE.x - global_avg_MAE.y)/global_avg_MAE.y * 100) %>%
  arrange(Protection, Parameter)

write.csv(var_magnitude_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/var_avg_accuracy_by_magnitude_protection.csv"), row.names=FALSE)

# calculate the mae under each model for the original and k-nTS+ (k = 3) data

original_model_ranks_mae <- all_original_results %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  arrange(MAE)

protected_model_ranks_mae <- all_protected_results %>%
  filter(Protection == "k-nts-plus-bounded", Parameter == "3-1.5") %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  arrange(MAE)

mae_by_model <- original_model_ranks_mae %>%
  left_join(protected_model_ranks_mae, by=c("Model")) %>%
  mutate(pct_change = (MAE.y - MAE.x)/MAE.x * 100)

write.csv(mae_by_model, paste0("../../Outputs/Results/", data_folder, "Tables/averages_by_model.csv"), row.names=FALSE)

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

write.csv(protection_data_avgs, paste0("../../Outputs/Results/", data_folder, "Tables/averages_by_data.csv"), row.names=FALSE)

#####################################################################################
