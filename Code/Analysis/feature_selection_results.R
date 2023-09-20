## Analyze the feature selections from RReliefF and RFE

# Author: Cameron Bale

library(tidyverse)

fp <- "../../Outputs/RFE Rankings/"

fnames <- list.files(fp)

rfe_results <- lapply(fnames, function(x) read_csv(paste0(fp, x)))

temp <- rfe_results[[1]]