## Code to visualize the forecast error distributions.

# Author: Cameron Bale

# import libraries
library(tidyverse)
library(ggplot2)
library(tidytext)

# path to files with error distributions
ed_file_path <- "../../Outputs/Results/Error_Distributions/"

eds <- read_csv(paste0(ed_file_path, "all_distributions_h1.csv"))

eds <- eds %>% gather(key="name", value="values") %>%
  mutate(name = gsub("Multivariate_LGBM", "LGBM", name),
         name = gsub("k_nts_plus", "knts+", name),
         name = gsub("k_nts", "knts", name),
         name = substring(name, 1, nchar(name)-4)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter"), sep="_") %>%
  mutate(Parameter = if_else(is.na(Parameter), "Original", Parameter))

average_error_ranks <- eds %>%
  group_by(Protection, Model) %>%
  summarize(avg_error = mean(values), .groups="drop") %>%
  arrange(Protection, avg_error) %>%
  group_by(Protection) %>%
  mutate(rank = 1:n())

average_error_ranks %>%
  filter(Protection == "original")

error_variance_ranks <- eds %>%
  group_by(Protection, Model) %>%
  summarize(error_variance = var(values), .groups="drop") %>%
  arrange(Protection, error_variance) %>%
  group_by(Protection) %>%
  mutate(rank = 1:n())

error_variance_ranks %>%
  filter(Protection == "original")

ranks1 <- average_error_ranks %>%
  filter(Protection != "original") %>%
  group_by(Model) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank)

ranks2 <- error_variance_ranks %>%
  filter(Protection != "original") %>%
  group_by(Model) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(avg_rank)

ranks1

ranks2

##############################################

## plot error distributions

limited_eds <- eds %>%
  filter(Protection != "original") %>%
  mutate(error_upper_limit = quantile(values, 0.95)) %>%
  filter(values <= error_upper_limit)

limited_eds %>%
  ggplot(aes(x=Model, y=values)) +
  geom_boxplot() +
  facet_wrap(~Protection) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# calculate the error variance normalized by the smallest variance per protection method parameter

normalized_error_variances <- eds %>%
  filter(Horizon=="h1", Protection %in% c("original", "Top")) %>%
  group_by(Model, Protection, Parameter) %>%
  summarize(error_variance = var(values), .groups="drop") %>%
  # group_by(Protection, Parameter) %>%
  mutate(norm_error_variance = error_variance/min(error_variance))

# ARIMA lowest forecast error variance
normalized_error_variances %>%
  arrange(Parameter, norm_error_variance) %>%
  mutate(Parameter=as.factor(Parameter), Model=reorder_within(Model, norm_error_variance, Parameter)) %>%
  ggplot(aes(x = Model, y = norm_error_variance)) +
  geom_col() +
  facet_wrap(~Parameter, scales='free_x') +
  scale_x_reordered() +
  labs(x="Model",
       y="Normalized Error Variance")



