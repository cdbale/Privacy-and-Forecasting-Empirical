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

k3_error_ranks <- eds %>%
  filter(Protection == "knts+", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(avg_error = mean(values), .groups='drop') %>%
  arrange(avg_error) %>%
  mutate(rank = 1:n())

error_sd_ranks <- eds %>%
  group_by(Protection, Model) %>%
  summarize(error_sd = sd(values), .groups="drop") %>%
  arrange(Protection, error_sd) %>%
  group_by(Protection) %>%
  mutate(rank = 1:n())

k3_sd_ranks <- eds %>%
  filter(Protection == "knts+", Parameter == "3") %>%
  group_by(Model) %>%
  summarize(error_sd = sd(values), .groups='drop') %>%
  arrange(error_sd) %>%
  mutate(rank = 1:n())

k3_error_ranks

k3_variance_ranks
