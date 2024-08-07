### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

# Author: Cameron Bale

library(tidyverse)
library(forecast)

data_folder <- "M3_rate/"

# now import mean absolute errors for all forecasts
error_dist_path <- paste0("../../Outputs/Results/", data_folder, "Error_Distributions/")

# import results files
res_files <- list.files(error_dist_path)

res_files <- grep("_all_distributions_h1", res_files, value=TRUE)
res_files <- grep("inverse_rate", res_files, value=TRUE, invert=TRUE)

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
                                  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data_Type", "Data"), sep="_"))

var_protected_results <- lapply(var_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                  mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                  separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data_Type", "Data"), sep="_"))

# transform to tidy data
all_original_results <- lapply(all_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                 mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                 separate(name, c("Model", "Horizon", "Data_Type", "Data"), sep="_"))

# combine into a single dataframe
all_protected_results <- do.call(rbind, all_protected_results)
var_protected_results <- do.call(rbind, var_protected_results)
all_original_results <- do.call(rbind, all_original_results)

all_protected_results <- all_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

all_original_results <- all_original_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

var_protected_results <- var_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

# if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
#   write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
#   write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
# } else {
#   dir.create(paste0("../../Outputs/Results/", data_folder, "Tables/"))
#   write.csv(all_original_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_original_results.csv"), row.names = FALSE)
#   write.csv(all_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_protected_results.csv"), row.names = FALSE)
# }

################################################################################
################################################################################
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

write.csv(protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/rate_protection_avgs.csv"), row.names=FALSE)

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

write.csv(var_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/rate_var_protection_avgs.csv"), row.names=FALSE)

################################################################################

# k-nTS+ (k = 3) model specific results
original_model_ranks_mae <- all_original_results %>%
  group_by(Model) %>%
  summarize(original_avg_mae = mean(values), .groups='drop')

protected_model_ranks_mae <- all_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(avg_mae = mean(values), .groups='drop')

mae_by_model <- protected_model_ranks_mae %>%
  left_join(original_model_ranks_mae, by="Model") %>%
  mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)

write.csv(mae_by_model, file=paste0("../../Outputs/Results/", data_folder, "Tables/avg_knts_model_results.csv"), row.names=FALSE)

################################################################################
################################################################################
################################################################################

# import results files
ir_res_files <- list.files(error_dist_path)

ir_res_files <- grep("all_distributions_h1", ir_res_files, value=TRUE)
ir_res_files <- grep("inverse_rate", ir_res_files, value=TRUE)

all_ir_results <- lapply(ir_res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_ir_protected_results <- lapply(all_ir_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                            contains("_AN_", ignore.case=FALSE), 
                                                                            contains("k-nts", ignore.case=FALSE)))

var_ir_protected_results <- lapply(all_ir_results, function(x) x %>% select(contains("var-an", ignore.case=FALSE),
                                                                            contains("var-knts", ignore.case=FALSE),
                                                                            contains("var-sim", ignore.case=FALSE)))

# data frame for calculating protected results
all_ir_original_results <- lapply(all_ir_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x[,grep("var-an", colnames(x[,grep("var-sim", colnames(x[,grep("var-knts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

all_ir_protected_results <- lapply(all_ir_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
var_ir_protected_results <- lapply(var_ir_protected_results, function(x) bind_cols(x, Snum=1:nrow(x)))
all_ir_original_results <- lapply(all_ir_original_results, function(x) bind_cols(x, Snum=1:nrow(x)))

# transform to tidy data
all_ir_protected_results <- lapply(all_ir_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                   mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                   separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data_Type", "Data"), sep="_"))

var_ir_protected_results <- lapply(var_ir_protected_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                     mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                     separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data_Type", "Data"), sep="_"))

# transform to tidy data
all_ir_original_results <- lapply(all_ir_original_results, function(x) x %>% gather(key="name", value="values", -Snum) %>%
                                  mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                  separate(name, c("Model", "Horizon", "Data_Type", "Data"), sep="_"))

# combine into a single dataframe
all_ir_protected_results <- do.call(rbind, all_ir_protected_results)
var_ir_protected_results <- do.call(rbind, var_ir_protected_results)
all_ir_original_results <- do.call(rbind, all_ir_original_results)

all_ir_protected_results <- all_ir_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

all_ir_original_results <- all_ir_original_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

var_ir_protected_results <- var_ir_protected_results %>%
  filter(Model != "VAR" | Data != "yearly-MICRO")

if (file.exists(paste0("../../Outputs/Results/", data_folder, "Tables/"))){
  write.csv(all_ir_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_protected_results.csv"), row.names = FALSE)
} else {
  dir.create(paste0("../../Outputs/Results/", data_folder, "Tables/"))
  write.csv(all_ir_protected_results, file=paste0("../../Outputs/Results/", data_folder, "Tables/all_ir_protected_results.csv"), row.names = FALSE)
}

### calculate the average accuracy across all models and data sets
# for each privacy method
original_m3_results <- read.csv("../../Outputs/Results/M3/Tables/all_original_results.csv")

ir_original_global_avg_mae <- original_m3_results %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

ir_original_var_global_avg_mae <- original_m3_results %>%
  filter(Model == "VAR") %>%
  summarize(global_avg_MAE = mean(values)) %>%
  pull(global_avg_MAE)

ir_original_avgs <- all_ir_original_results %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=ir_original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100)

original_model_avgs <- original_m3_results %>%
  group_by(Model) %>%
  summarize(original_global_avg_MAE = mean(values))

ir_original_model_avgs <- all_ir_original_results %>%
  group_by(Model) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  left_join(original_model_avgs, by = "Model") %>%
  mutate(percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100)

ir_protection_avgs <- all_ir_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=ir_original_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100) %>%
  arrange(Protection, Parameter)

write.csv(ir_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/ir_rate_protection_avgs.csv"), row.names=FALSE)

ir_var_protection_avgs <- var_ir_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(global_avg_MAE = mean(values), .groups="drop") %>%
  mutate(original_global_avg_MAE=ir_original_var_global_avg_mae,
         percent_change_mae = (global_avg_MAE-original_global_avg_MAE)/original_global_avg_MAE * 100) %>%
  arrange(Protection, Parameter)

write.csv(ir_var_protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/ir_var_rate_protection_avgs.csv"), row.names=FALSE)

# model specific results
original_ir_model_avgs <- original_m3_results %>%
  group_by(Model) %>%
  summarize(original_avg_mae = mean(values), .groups='drop')

protected_ir_model_avgs <- all_ir_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(avg_mae = mean(values), .groups='drop') %>%
  left_join(original_ir_model_avgs, by="Model") %>%
  mutate(pct_change = (avg_mae-original_avg_mae)/original_avg_mae * 100)

write.csv(protected_ir_model_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/ir_averages_by_model.csv"), row.names=FALSE)

# data specific results
original_ir_data_avgs <- original_m3_results %>%
  group_by(Data) %>%
  summarize(original_avg_mae = mean(values), .groups='drop')

protected_ir_data_avgs <- all_ir_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Data) %>%
  summarize(avg_mae = mean(values), .groups='drop')

ir_data_avgs <- protected_ir_data_avgs %>%
  left_join(original_ir_data_avgs, by="Data") %>%
  mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)

################################################################################
################################################################################

# calculate the mae under each model for the original and k-nTS+ (k = 3) data
original_model_ranks_mae <- all_original_results %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  arrange(MAE)

protected_model_ranks_mae <- all_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  arrange(MAE) %>%
  left_join(original_model_ranks_mae, by="Model") %>%
  mutate(pct_change = (MAE.x - MAE.y)/MAE.y * 100)

write.csv(protected_model_ranks_mae, file=paste0("../../Outputs/Results/", data_folder, "Tables/rate_averages_by_model.csv"), row.names=FALSE)

original_ir_model_ranks_mae <- all_ir_original_results %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  arrange(MAE)

# check percentage change in forecast accuracy from inverse rate conversion
original_ir_model_ranks_mae %>%
  ungroup() %>%
  left_join(original_model_avgs, by="Model") %>%
  mutate(pct_change_mae = (MAE - original_global_avg_MAE)/original_global_avg_MAE * 100)

protected_ir_model_ranks_mae <- all_ir_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(MAE = mean(values)) %>%
  left_join()
  arrange(MAE)

################################################################################
################################################################################

# calculate the mae under each model for the original and k-nTS+ (k = 3) data
original_data_ranks_mae <- all_original_results %>%
  group_by(Data) %>%
  summarize(MAE = mean(values))

protected_data_ranks_mae <- all_protected_results %>%
  filter(Protection == "k-nts-plus", Parameter == "3") %>%
  group_by(Data) %>%
  summarize(MAE = mean(values)) %>%
  left_join(original_data_ranks_mae, by="Data") %>%
  mutate(pct_change = (MAE.x - MAE.y)/MAE.y * 100)

write.csv(protected_data_ranks_mae, file=paste0("../../Outputs/Results/", data_folder, "Tables/rate_averages_by_data.csv"), row.names=FALSE)
