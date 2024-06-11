# Identify which series are largest/smallest to identify contributions
# to the percent change in forecast error.

# Author: Cameron Bale

library(tidyverse)

data_folder <- "M3/"
file_path <- paste0("../../Data/Cleaned/", data_folder)
file_names <- grep("_h1_test", list.files(file_path), value=TRUE)

# import file, compute identification for largest 50% and smallest 50% time
# series based on test data

if (file.exists(paste0("../../Data/Magnitudes/", data_folder))){
  print("Folder exists.")
} else {
  dir.create(paste0("../../Data/Magnitudes/", data_folder), recursive=TRUE)
}

for (f in file_names){
  df <- read_csv(paste0(file_path, f)) %>%
    mutate(large_magnitude = `0` > median(`0`),
           snum = 1:n())
  prefix <- strsplit(f, split="_")[[1]][1]
  
  write.csv(df, file=paste0("../../Data/Magnitudes/", data_folder, prefix, "_magnitudes.csv"), row.names=FALSE)
}
