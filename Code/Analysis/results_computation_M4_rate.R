### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

################### Check file counts after all forecasting is done ###################

# Author: Cameron Bale

library(tidyverse)
library(forecast)

data_folder <- "M4_rate/"

# now import mean absolute errors for all forecasts
error_dist_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

# import results files
res_files <- list.files(error_dist_path)

res_files <- grep("_all_distributions_h1", res_files, value=TRUE)
res_files <- grep("_ir_", res_files, value=TRUE, invert=TRUE)

all_results <- lapply(res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_protected_results <- lapply(all_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                      contains("_AN_", ignore.case=FALSE), 
                                                                      contains("k-nts", ignore.case=FALSE)))

# data frame for calculating protected results
all_original_results <- lapply(all_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x[,grep("var-an", colnames(x[,grep("var-sim", colnames(x[,grep("var-knts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

all_protected_results <- lapply(all_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
all_original_results <- lapply(all_original_results, function(x) bind_cols(x, Snum=1:nrow(x)))

# transform to tidy data
all_protected_results <- lapply(all_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                  mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_"))

# transform to tidy data
all_original_results <- lapply(all_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                 mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                 separate(name, c("Model", "Horizon", "Data"), sep="_"))

# combine into a single dataframe
all_protected_results <- do.call(rbind, all_protected_results)
all_original_results <- do.call(rbind, all_original_results)

if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
} else {
  dir.create(paste0("../../Outputs/Results/", data_folder, "Tables/"))
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
}

################################################################################

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

write.csv(protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/protection_avgs.csv"), row.names=FALSE)

################################################################################
################################################################################
################################################################################

### repeat for the inverse rate data

# import results files
ir_res_files <- list.files(error_dist_path)

ir_res_files <- grep("all_distributions_h1", ir_res_files, value=TRUE)
ir_res_files <- grep("_ir_", ir_res_files, value=TRUE)

all_ir_results <- lapply(ir_res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_ir_protected_results <- lapply(all_ir_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                            contains("_AN_", ignore.case=FALSE), 
                                                                            contains("k-nts", ignore.case=FALSE)))

# data frame for calculating protected results
all_ir_original_results <- lapply(all_ir_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x[,grep("var-an", colnames(x[,grep("var-sim", colnames(x[,grep("var-knts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

all_ir_protected_results <- lapply(all_ir_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
all_ir_original_results <- lapply(all_ir_original_results, function(x) bind_cols(x, Snum=1:nrow(x)))

# transform to tidy data
all_ir_protected_results <- lapply(all_ir_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                     mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                     separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_"))

# transform to tidy data
all_ir_original_results <- lapply(all_ir_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                    mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                    separate(name, c("Model", "Horizon", "Data"), sep="_"))

# combine into a single dataframe
all_ir_protected_results <- do.call(rbind, all_ir_protected_results)
all_ir_original_results <- do.call(rbind, all_ir_original_results)

if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_protected_results.csv"), row.names = FALSE)
} else {
  dir.create(paste0("../../Outputs/Results/", data_folder, "Tables/"))
  write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_original_results.csv"), row.names = FALSE)
  write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_protected_results.csv"), row.names = FALSE)
}

# notice that there are some outlying errors
# so we will exclude these from the averages
# somewhat subjective choice here...
# we choose to exclude any cases where the forecast error was over 100,000

# to_exclude <- all_ir_protected_results %>%
#   group_by(Protection, Parameter, Model, Data) %>%
#   summarize(avg_AE = mean(values), .groups='drop') %>%
#   arrange(desc(avg_AE)) %>%
#   unite('file', Protection:Data) %>%
#   slice(1:3) %>%
#   pull(file)
# 
# # remove the large outlying errors
# all_ir_protected_results <- all_ir_protected_results %>%
#   select(Protection, Parameter, Model, Data, Horizon, Snum, values) %>%
#   unite('file', Protection:Data) %>%
#   filter(!file %in% to_exclude) %>%
#   separate(file, c("Protection", "Parameter", "Model", "Data"), sep="_")

### calculate the average accuracy across all models and data sets
# for each privacy method
original_m4_results <- read.csv("../../Outputs/Results/M4/Tables/all_original_results.csv")

# MAE on the original data
ir_original_global_avg_mae <- original_m4_results %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

# protection method-specific change in MAE
ir_protection_avgs <- all_ir_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=ir_original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100) %>%
  arrange(Protection, Parameter)

write.csv(ir_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/ir_avg_accuracy_by_protection.csv"), row.names=FALSE)
