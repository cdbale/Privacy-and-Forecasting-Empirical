## Analyze the features chosen by RReliefF and RFE

library(tidyverse)

relief_directory <- "../../Outputs/RReliefF Rankings/"

relief_files <- list.files(relief_directory)

full_relief <- tibble()

for (f in relief_files){
  temp <- read_csv(paste0(relief_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2])
  
  full_relief <- bind_rows(full_relief, temp)
}

full_relief

avg_evals <- full_relief %>%
  group_by(feature) %>%
  summarize(avg_value = mean(value)) %>%
  arrange(avg_value)

sorted_names <- avg_evals$feature

avg_evals %>%
  mutate(feature=factor(feature, 
                        levels=sorted_names)) %>%
  ggplot(aes(x=feature, y=avg_value)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Weight") +
  scale_x_discrete(labels=c("Curvature",
                            "Max Variance Shift",
                            "Variance",
                            "Max Level Shift",
                            "Spike",
                            "Linearity",
                            "Mean",
                            "Time Level Shift",
                            "Time KL Shift",
                            "Max KL Shift",
                            "Unitroot PP",
                            "Peak",
                            "X ACF",
                            "Time Variance Shift",
                            "Trend",
                            "Trough",
                            "Hurst",
                            "Lumpiness",
                            "Seasonal ACF",
                            "Kurtosis",
                            "Skewness",
                            "Flat Spots",
                            "Second Difference PACF5",
                            "Crossing Points",
                            "Second Difference ACF10",
                            "Seasonal PACF",
                            "First Difference ACF10",
                            "Error ACF10",
                            "First Difference PACF5",
                            "Unitroot KPSS",
                            "Seasonal Strength",
                            "Second Difference ACF",
                            "Stability",
                            "Error ACF",
                            "X PACF5",
                            "Nonlinearity",
                            "First Difference ACF",
                            "X ACF10",
                            "Entropy"))

################################################################################
################################################################################

rfe_directory <- "../../Outputs/RFE Rankings/"

rfe_files <- list.files(rfe_directory)

full_rfe <- tibble()

for (f in rfe_files){
  temp <- read_csv(paste0(rfe_directory, f)) %>%
    mutate(data_set = strsplit(f, "_")[[1]][2])
  
  full_rfe <- bind_rows(full_rfe, temp)
}

avg_rfe_evals <- full_rfe %>%
  group_by(var) %>%
  summarize(avg_rank = mean(rank)) %>%
  arrange(desc(avg_rank))

sorted_rfe_names <- avg_rfe_evals$var

avg_rfe_evals %>%
  mutate(var=factor(var, 
                    levels=sorted_rfe_names)) %>%
  ggplot(aes(x=var, y=avg_rank)) +
  geom_col() +
  coord_flip() +
  labs(x = "Feature",
       y = "Average Rank") +
  scale_x_discrete(labels=c("Curvature",
                            "Max Variance Shift",
                            "Variance",
                            "Max Level Shift",
                            "Spike",
                            "Linearity",
                            "Mean",
                            "Time Level Shift",
                            "Time KL Shift",
                            "Max KL Shift",
                            "Unitroot PP",
                            "Peak",
                            "X ACF",
                            "Time Variance Shift",
                            "Trend",
                            "Trough",
                            "Hurst",
                            "Lumpiness",
                            "Seasonal ACF",
                            "Kurtosis",
                            "Skewness",
                            "Flat Spots",
                            "Second Difference PACF5",
                            "Crossing Points",
                            "Second Difference ACF10",
                            "Seasonal PACF",
                            "First Difference ACF10",
                            "Error ACF10",
                            "First Difference PACF5",
                            "Unitroot KPSS",
                            "Seasonal Strength",
                            "Second Difference ACF",
                            "Stability",
                            "Error ACF",
                            "X PACF5",
                            "Nonlinearity",
                            "First Difference ACF",
                            "X ACF10",
                            "Entropy"))