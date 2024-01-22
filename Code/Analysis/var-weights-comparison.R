## Compare Original VAR weights to degraded VAR weights using the M3 Monthly
## Micro.

# Author: Cameron Bale

library(tidyverse)
library(ggpubr)

# import the VAR model weight files

all_var_param_files <- list.files("../../Outputs/VAR Weights/")

og_var_param_files <- grep("AN_", all_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("DP_", og_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("k-nts", og_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("rate_", og_var_param_files, value=TRUE, invert=TRUE)

absolute_diffs <- list()

for (k in c("3-0.5", "3-1", "3-1.5")){

  knts_string <- paste0("k-nts-plus-bounded_", k, "_")
  
  knts_var_param_files <- grep(knts_string, all_var_param_files, value=TRUE)
  knts_var_param_files <- grep("rate_", knts_var_param_files, value=TRUE, invert=TRUE)
  
  knts_diffs <- c()

  for (i in seq_along(knts_var_param_files)){
    
    split_file <- strsplit(knts_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
  
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", knts_var_param_files[i]))
  
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
  
    knts_diffs <- append(knts_diffs, unname(as_vector(abs(protected_weights - original_weights))))
  
  }
  
  absolute_diffs[[knts_string]] <- knts_diffs
  
}

absolute_diffs <- do.call(cbind, absolute_diffs)

absolute_diffs <- as_tibble(absolute_diffs)

absolute_diffs <- absolute_diffs %>%
  gather(key="data", value="absolute_difference")

################################################################################

# repeat the above with differentially private data

dp_absolute_diffs <- list()

for (epsilon in c(0.1, 1, 4.6, 10, 20)){
  
  dp_string <- paste0("DP_", epsilon, "_")
  
  dp_var_param_files <- grep(dp_string, all_var_param_files, value=TRUE)
  dp_var_param_files <- grep("rate_", dp_var_param_files, value=TRUE, invert=TRUE)
  
  dp_diffs <- c()
  
  for (i in seq_along(dp_var_param_files)){
    
    split_file <- strsplit(dp_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", dp_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
    
    dp_diffs <- append(dp_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  dp_absolute_diffs[[dp_string]] <- dp_diffs
  
}

dp_absolute_diffs <- do.call(cbind, dp_absolute_diffs)

dp_absolute_diffs <- as_tibble(dp_absolute_diffs)

absolute_diffs <- dp_absolute_diffs %>%
  gather(key="data", value="absolute_difference") %>%
  bind_rows(absolute_diffs)

################################################################################

# repeat the above with additive noise protected data

an_absolute_diffs <- list()

for (s in c(0.25, 0.5, 1.0, 1.5, 2.0)){
  
  an_string <- paste0("AN_", s, "_")
  
  an_var_param_files <- grep(an_string, all_var_param_files, value=TRUE)
  an_var_param_files <- grep("rate_", an_var_param_files, value=TRUE, invert=TRUE)
  
  an_diffs <- c()
  
  for (i in seq_along(an_var_param_files)){
    
    split_file <- strsplit(an_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", an_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
    
    an_diffs <- append(an_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  an_absolute_diffs[[an_string]] <- an_diffs
  
}

an_absolute_diffs <- do.call(cbind, an_absolute_diffs)

an_absolute_diffs <- as_tibble(an_absolute_diffs)

absolute_diffs <- an_absolute_diffs %>%
  gather(key="data", value="absolute_difference") %>%
  bind_rows(absolute_diffs) %>%
  # mutate(Method = ifelse(grepl("k-nts", data), "k-nTS+",
  #                 ifelse(grepl("DP_", data), "DP",
  #                 ifelse(grepl("AN_", data), "AN", "None")))) %>%
  separate(data, c("Method", "Parameter"), sep="_")

################################################################################

diff_stats <- absolute_diffs %>%
  group_by(Method, Parameter) %>%
  summarize(means=mean(absolute_difference),
            sds=sd(absolute_difference), .groups='drop')

# plot the point plot
an_plot <- diff_stats %>%
  filter(Method == "AN") %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="",
       y="Absolute Difference in Coefficients") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

# plot the point plot
dp_plot <- diff_stats %>%
  filter(Method == "DP") %>%
  mutate(Parameter = factor(Parameter, levels=c('20', '10', '4.6', '1', '0.1'))) %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="",
       y="") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

# plot the point plot
knts_plot <- diff_stats %>%
  filter(Method == "k-nts-plus-bounded") %>%
  mutate(Parameter = factor(Parameter, levels=c('3-0.5', '3-1', '3-1.5'), labels=c("3, 0.5", "3, 1.0", "3, 1.5"))) %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="",
       y="") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

################################################################################
################################################################################

og_var_param_files <- grep("AN_", all_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("DP_", og_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("k-nts", og_var_param_files, value=TRUE, invert=TRUE)
og_var_param_files <- grep("rate_", og_var_param_files, value=TRUE)

absolute_diffs <- list()

for (k in c(3, 5, 7, 10, 15)){
  
  knts_string <- paste0("k-nts-plus_", k, "_")
  
  knts_var_param_files <- grep(knts_string, all_var_param_files, value=TRUE)
  knts_var_param_files <- grep("rate_", knts_var_param_files, value=TRUE)
  
  knts_diffs <- c()
  
  for (i in seq_along(knts_var_param_files)){
    
    split_file <- strsplit(knts_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", knts_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
    
    knts_diffs <- append(knts_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  absolute_diffs[[knts_string]] <- knts_diffs
  
}

absolute_diffs <- do.call(cbind, absolute_diffs)

absolute_diffs <- as_tibble(absolute_diffs)

absolute_diffs <- absolute_diffs %>%
  gather(key="data", value="absolute_difference")

################################################################################

# repeat the above with differentially private data

dp_absolute_diffs <- list()

for (epsilon in c(0.1, 1, 4.6, 10, 20)){
  
  dp_string <- paste0("DP_", epsilon, "_")
  
  dp_var_param_files <- grep(dp_string, all_var_param_files, value=TRUE)
  dp_var_param_files <- grep("rate_", dp_var_param_files, value=TRUE)
  
  dp_diffs <- c()
  
  for (i in seq_along(dp_var_param_files)){
    
    split_file <- strsplit(dp_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", dp_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
    
    dp_diffs <- append(dp_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  dp_absolute_diffs[[dp_string]] <- dp_diffs
  
}

dp_absolute_diffs <- do.call(cbind, dp_absolute_diffs)

dp_absolute_diffs <- as_tibble(dp_absolute_diffs)

absolute_diffs <- dp_absolute_diffs %>%
  gather(key="data", value="absolute_difference") %>%
  bind_rows(absolute_diffs)

################################################################################

# repeat the above with additive noise protected data

an_absolute_diffs <- list()

for (s in c(0.25, 0.5, 1.0, 1.5, 2.0)){
  
  an_string <- paste0("AN_", s, "_")
  
  an_var_param_files <- grep(an_string, all_var_param_files, value=TRUE)
  an_var_param_files <- grep("rate_", an_var_param_files, value=TRUE)
  
  an_diffs <- c()
  
  for (i in seq_along(an_var_param_files)){
    
    split_file <- strsplit(an_var_param_files[i], split="_")[[1]]
    
    file_suffix <- paste(split_file[3:length(split_file)], collapse="_")
    
    protected_weights <- read.csv(paste0("../../Outputs/VAR Weights/", an_var_param_files[i]))
    
    original_weights <- read.csv(paste0("../../Outputs/VAR Weights/", grep(file_suffix, og_var_param_files, value=TRUE)))
    
    an_diffs <- append(an_diffs, unname(as_vector(abs(protected_weights - original_weights))))
    
  }
  
  an_absolute_diffs[[an_string]] <- an_diffs
  
}

an_absolute_diffs <- do.call(cbind, an_absolute_diffs)

an_absolute_diffs <- as_tibble(an_absolute_diffs)

absolute_diffs <- an_absolute_diffs %>%
  gather(key="data", value="absolute_difference") %>%
  bind_rows(absolute_diffs) %>%
  # mutate(Method = ifelse(grepl("k-nts", data), "k-nTS+",
  #                 ifelse(grepl("DP_", data), "DP",
  #                 ifelse(grepl("AN_", data), "AN", "None")))) %>%
  separate(data, c("Method", "Parameter"), sep="_")

################################################################################

diff_stats <- absolute_diffs %>%
  group_by(Method, Parameter) %>%
  summarize(means=mean(absolute_difference),
            sds=sd(absolute_difference), .groups='drop')

# plot the point plot
an_rate_plot <- diff_stats %>%
  filter(Method == "AN") %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="Additive Noise Parameters",
       y="Absolute Difference in Coefficients") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

# plot the point plot
dp_rate_plot <- diff_stats %>%
  filter(Method == "DP") %>%
  mutate(Parameter = factor(Parameter, levels=c('20', '10', '4.6', '1', '0.1'))) %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="Differential Privacy Parameters",
       y="") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

# plot the point plot
knts_rate_plot <- diff_stats %>%
  filter(Method == "k-nts-plus") %>%
  mutate(Parameter = factor(Parameter, levels=c('3', '5', '7', '10', '15'))) %>%
  ggplot(aes(x=Parameter, y=means)) + 
  geom_point(size=4)+
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(0.05)) +
  labs(x="k-nTS+ Parameters",
       y="") + 
  theme(text = element_text(size=12),
        plot.title = element_text(face= "bold", colour= "black"),
        axis.title.x = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(face="bold", colour = "black")) +
  ylim(-3.5, 3.5)

ggarrange(an_plot, dp_plot, knts_plot, an_rate_plot, dp_rate_plot, knts_rate_plot, 
          labels=c("A", "B", "C", "D", "E", "F"))

