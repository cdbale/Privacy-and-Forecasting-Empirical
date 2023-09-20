### File for computing forecast accuracy results across models, data sets,
### protection methods, etc.

################### Check file counts after all forecasting is done###################

# Author: Cameron Bale

library(tidyverse)

error_dist_path <- "../../Outputs/Results/Error_Distributions/"

# import results files
res_files <- list.files(error_dist_path)

res_files <- grep("_all_distributions_h1", res_files, value=TRUE)

all_results <- lapply(res_files, function(x) read_csv(paste0(error_dist_path, x)))

# data frame for calculating protected results
all_protected_results <- lapply(all_results, function(x) x %>% select(contains("_DP_", ignore.case=FALSE), 
                                                                      contains("_AN_", ignore.case=FALSE), 
                                                                      contains("_k-nts", ignore.case=FALSE)))

# data frame for calculating protected results
all_original_results <- lapply(all_results, function(x) x[,grep("_DP_", colnames(x[,grep("_AN_", colnames(x[,grep("k-nts", colnames(x), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)]), invert=TRUE, value=TRUE)])

# transform to tidy data
all_protected_results <- lapply(all_protected_results, function(x) x %>% gather(key="name", value="values") %>%
                                                                         mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                         separate(name, c("Model", "Horizon", "Protection", "Parameter", "Data"), sep="_"))

# transform to tidy data
all_original_results <- lapply(all_original_results, function(x) x %>% gather(key="name", value="values") %>%
                                                                       mutate(name = substring(name, 1, nchar(name)-8)) %>%
                                                                       separate(name, c("Model", "Horizon", "Data"), sep="_"))

# combine into a single dataframe
all_protected_results <- do.call(rbind, all_protected_results)
all_original_results <- do.call(rbind, all_original_results)

# notice that there are some outlying errors (VAR tends to be the culprit)
# so we will exclude these from the averages
# somewhat subjective choice here...
to_exclude <- all_protected_results %>%
  group_by(Protection, Parameter, Model, Data) %>%
  summarize(avg_AE = mean(values), .groups='drop') %>%
  arrange(desc(avg_AE)) %>%
  unite('file', Protection:Data) %>%
  slice(1:13) %>%
  pull(file)

# remove the large outlying errors
all_protected_results <- all_protected_results %>%
  select(Protection, Parameter, Model, Data, Horizon, values) %>%
  unite('file', Protection:Data) %>%
  filter(!file %in% to_exclude) %>%
  separate(file, c("Protection", "Parameter", "Model", "Data"), sep="_")
  
### calculate the overall averages for each privacy method
original_avg <- all_original_results %>%
  summarize(avg_ae = mean(values)) %>%
  pull(avg_ae)

protection_avgs <- all_protected_results %>%
  group_by(Protection, Parameter) %>%
  summarize(avg_ae = mean(values), .groups="drop") %>%
  mutate(original_avg=original_avg,
         percent_change = (avg_ae-original_avg)/original_avg * 100,
         Parameter = as.numeric(Parameter)) %>%
  arrange(Protection, Parameter)

write.csv(protection_avgs, file="../../Outputs/Results/Tables/protection_avgs.csv", row.names=FALSE)

### calculate the overall averages for each privacy method and model
original_avg_model <- all_original_results %>%
  group_by(Model) %>%
  summarize(original_avg_ae = mean(values), .groups="drop")

protection_model_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Model) %>%
  summarize(avg_ae = mean(values), .groups="drop") %>%
  left_join(original_avg_model, by="Model") %>%
  mutate(percent_change = (avg_ae-original_avg_ae)/original_avg_ae * 100)

### calculate the overall averages for each privacy method, model, data set
original_avg_model_data <- all_original_results %>%
  group_by(Model, Data) %>%
  summarize(original_avg_ae = mean(values), .groups="drop")

protection_model_data_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Model, Data) %>%
  summarize(avg_ae = mean(values), .groups="drop") %>%
  left_join(original_avg_model_data, by=c("Model", "Data")) %>%
  mutate(percent_change = (avg_ae-original_avg_ae)/original_avg_ae * 100)

### calculate overall average for each privacy method and data set
original_avg_data <- all_original_results %>%
  group_by(Data) %>%
  summarize(original_avg_ae = mean(values), .groups="drop")

protection_data_avgs <- all_protected_results %>%
  group_by(Protection, Parameter, Data) %>%
  summarize(avg_ae = mean(values), .groups="drop") %>%
  left_join(original_avg_data, by=c("Data")) %>%
  mutate(percent_change = (avg_ae-original_avg_ae)/original_avg_ae * 100) %>%
  filter(Protection %in% c("DP", "k-nts-plus-corr-scaled") & Parameter %in% c(3, 1)) %>%
  arrange(Data)

write.csv(protection_data_avgs, "../../Outputs/Results/Tables/averages_by_frequency.csv", row.names=FALSE)





protection_model_data_avgs <- protection_model_data_avgs %>%
  arrange(Protection, Parameter, Data, original_avg_ae)

all_protected_results %>%
  filter(Model=="SES",
         Protection=='k-nts-plus',
         Parameter=="3",
         Data=="quarterly-FINANCE")


