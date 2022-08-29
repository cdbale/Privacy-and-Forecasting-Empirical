## Code to visualize the forecast error distributions.

# Author: Cameron Bale

# import libraries
library(tidyverse)
library(ggplot2)
library(tidytext)

# path to files with error distributions
ed_file_path <- "../../Outputs/Results/Error_Distributions/"

# # store all file names and import
# fnames <- list.files(path=ed_file_path)
# files <- lapply(fnames, function(x) read.csv(paste0(ed_file_path, x)))
# 
# eds <- do.call(bind_cols, files)
# 
# colnames(eds) <- fnames

eds <- read_csv(paste0(ed_file_path, "all_distributions.csv"))

eds <- eds %>% gather(key="name", value="values") %>%
  mutate(name = gsub("Multivariate_LGBM", "LGBM", name),
         name = substring(name, 1, nchar(name)-4)) %>%
  separate(name, c("Model", "Horizon", "Protection", "Parameter"), sep="_") %>%
  mutate(Parameter = if_else(is.na(Parameter), "Original", Parameter),
         Parameter = factor(Parameter, levels=c('Original', '0.1', '0.2', '0.4'), labels=c('Original', "Top 0.1", "Top 0.2", "Top 0.4")))

eds %>%
  filter(Horizon=="h1", Protection %in% c("original", "Top")) %>%
  ggplot(aes(x=values, fill=Protection)) +
  geom_density() +
  facet_wrap(~Model)

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






