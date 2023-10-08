## Compare Original VAR weights to degraded VAR weights using the M3 Monthly
## Micro.

# Author: Cameron Bale

library(tidyverse)

# import the VAR model weight files

all_var_param_files <- list.files("../../Outputs/VAR Weights/")

all_var_param_files <- grep("monthly-MICRO", all_var_param_files, value=TRUE)

var_param_files <- grep("AN_", all_var_param_files, value=TRUE, invert=TRUE)
var_param_files <- grep("DP_", var_param_files, value=TRUE, invert=TRUE)
var_param_files <- grep("k-nts", var_param_files, value=TRUE, invert=TRUE)

absolute_diffs <- list()

for (k in c(3, 5, 7, 10, 15)){

  knts_string <- paste0("k-nts_", k, "_")
  
  knts_var_param_files <- grep(paste0("k-nts_", k, "_"), all_var_param_files, value=TRUE)
  
  knts_diffs <- c()

  for (i in seq_along(knts_var_param_files)){
    
    split_file <- strsplit(knts_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
  
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", knts_var_param_files[i]))
  
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, var_param_files, value=TRUE)))
  
    knts_diffs <- append(knts_diffs, unname(as_vector(abs(protected_weights - original_weights))))
  
  }
  
  absolute_diffs[[knts_string]] <- knts_diffs
  
}

temp <- absolute_diffs

absolute_diffs <- do.call(cbind, absolute_diffs)

absolute_diffs <- as_tibble(absolute_diffs)

absolute_diffs <- absolute_diffs %>%
  gather(key="data", value="absolute_difference")

################################################################################

# repeat the above with differentially private data

dp_absolute_diffs <- list()

for (epsilon in c(0.1, 1, 4.6, 10, 20)){
  
  dp_string <- paste0("DP_", epsilon, "_")
  
  dp_var_param_files <- grep(paste0("DP_", epsilon, "_"), all_var_param_files, value=TRUE)
  
  dp_diffs <- c()
  
  for (i in seq_along(dp_var_param_files)){
    
    split_file <- strsplit(dp_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", dp_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, var_param_files, value=TRUE)))
    
    dp_diffs <- append(dp_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  dp_absolute_diffs[[dp_string]] <- dp_diffs
  
}

temp1 <- dp_absolute_diffs

dp_absolute_diffs <- do.call(cbind, dp_absolute_diffs)

dp_absolute_diffs <- as_tibble(dp_absolute_diffs)

dp_absolute_diffs <- dp_absolute_diffs %>%
  gather(key="data", value="absolute_difference")

################################################################################

absolute_diffs <- bind_rows(absolute_diffs, dp_absolute_diffs)

absolute_diffs <- absolute_diffs %>%
  mutate(data = factor(data, 
                       levels=c("k-nts_3_",
                                "k-nts_5_",
                                "k-nts_7_",
                                "k-nts_10_",
                                "k-nts_15_",
                                "DP_20_",
                                "DP_10_",
                                "DP_4.6_",
                                "DP_1_",
                                "DP_0.1_"),
                       labels=c("knTS+ (k = 3)",
                                "knTS+ (k = 5)",
                                "knTS+ (k = 7)",
                                "knTS+ (k = 10)",
                                "knTS+ (k = 15)",
                                "DP (\u03b5 = 20)",
                                "DP (\u03b5 = 10)",
                                "DP (\u03b5 = 4.6)",
                                "DP (\u03b5 = 1)",
                                "DP (\u03b5 = 0.1)")))

diff_stats <- absolute_diffs %>%
  group_by(data) %>%
  summarize(means=mean(absolute_difference),
            sds=sd(absolute_difference))

# plot the point plot
ggplot(diff_stats, aes(x=data, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="k-nTS+ Protected Data Set",
       y="Absolute Difference in Coefficients")

absolute_diffs %>%
  ggplot(aes(x=data, y=absolute_difference)) +
  geom_boxplot() +
  labs(x="Protected Data Set",
       y="Absolute Difference in Model Weights")

absolute_diffs %>%
  filter(absolute_difference <= 1.0) %>%
  ggplot(aes(x=data, y=absolute_difference)) +
  geom_boxplot() +
  labs(x="Protected Data Set",
       y="Absolute Difference in Model Weights")




