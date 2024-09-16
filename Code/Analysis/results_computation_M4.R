### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

################### Check file counts after all forecasting is done ###################

# Author: Cameron Bale

library(tidyverse)
library(forecast)

data_folder <- "M4/"

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
################################################################################
################################################################################

# notice that there are some outlying errors (VAR tends to be the culprit)
# remove those with errors over 100,000

to_exclude <- all_protected_results %>%
  group_by(Protection, Parameter, Model, Data) %>%
  summarize(avg_AE = mean(values), .groups='drop') %>%
  arrange(desc(avg_AE)) %>%
  unite('file', Protection:Data) %>%
  slice(1:5) %>%
  pull(file)

# remove the large outlying errors
all_protected_results <- all_protected_results %>%
  select(Protection, Parameter, Model, Data, Horizon, Snum, values) %>%
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

write.csv(protection_avgs, file=paste0("../../Outputs/Results/", data_folder, "Tables/protection_avgs.csv"), row.names=FALSE)

# k-nTS+ (k = 3) model specific results
# original_model_avgs <- all_original_results %>%
#   group_by(Model) %>%
#   summarize(original_avg_mae = mean(values), .groups='drop')
# 
# protected_model_avgs <- all_protected_results %>%
#   filter(Protection == "k-nts-plus", Parameter == "3") %>%
#   group_by(Model) %>%
#   summarize(avg_mae = mean(values), .groups='drop')
# 
# model_avgs <- protected_model_avgs %>%
#   left_join(original_model_avgs, by="Model") %>%
#   mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)
 
# # data specific results
# original_data_avgs <- all_original_results %>%
#   group_by(Data) %>%
#   summarize(original_avg_mae = mean(values), .groups='drop')
# 
# protected_data_avgs <- all_protected_results %>%
#   filter(Protection == "k-nts-plus", Parameter == "3") %>%
#   group_by(Data) %>%
#   summarize(avg_mae = mean(values), .groups='drop')
# 
# data_avgs <- protected_data_avgs %>%
#   left_join(original_data_avgs, by="Data") %>%
#   mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)
# 
# # data x model specific results
# original_model_data_avgs <- all_original_results %>%
#   group_by(Model, Data) %>%
#   summarize(original_avg_mae = mean(values), .groups='drop')
# 
# protected_model_data_avgs <- all_protected_results %>%
#   filter(Protection == "k-nts-plus", Parameter == "3") %>%
#   group_by(Model, Data) %>%
#   summarize(avg_mae = mean(values), .groups='drop')
# 
# model_data_avgs <- protected_model_data_avgs %>%
#   left_join(original_model_data_avgs, by=c("Model", "Data")) %>%
#   mutate(pct_change = (avg_mae - original_avg_mae)/original_avg_mae * 100)
# 
# model_data_avgs %>%
#   ggplot(aes(x=Data, y=Model, fill=pct_change)) +
#   geom_tile(color = "white",
#             lwd = 1,
#             linetype = 1) +
#   geom_text(aes(label = round(pct_change)), color = "white", size = 3) +
#   # scale_fill_gradient(low = "darkgreen", high = "red") +
#   coord_fixed() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         text = element_text(size=19),
#         plot.title = element_text(face= "bold", colour= "black"),
#         axis.title.x = element_text(face="bold", colour = "black"),    
#         axis.title.y = element_text(face="bold", colour = "black"))

################################################################################
################################################################################
