# R script for forecasting using a Lasso-VAR model.

# Author: Cameron Bale

# Based on example here: http://www.wbnicholson.com/BigVAR.html

############################################
######### Step 1: Data Processing ##########
############################################

# command line arguments for executing this script will be:
# 1 - forecast horizon
# 2 - protection method
# 3 - protection method parameter

###################### The first part of this script is for model testing. #######################

library(BigVAR)

h <- 1
protection_method <- "original"
protection_parameter <- "none"

######################## Figure out why this file isn't running - it won't create the simple text file

# Read in the pre-processed data
fname <- paste0("../../Data/VAR_R_Datasets/h_", as.character(h), "_", protection_method, "_", protection_parameter, ".csv")

ts_data <- read.csv(fname)

# We have three lengths of time series, so we will use three lasso-VAR models, one for each length
# First, we need to create the three dataframes containing the series with equal lengths

# store the unique numbers of NA values
num_na <- unique(apply(is.na(ts_data), 1, sum))

# store the series with the first length
ds1 <- ts_data[apply(is.na(ts_data), 1, sum) == num_na[1],]

# select only the columns without NA values
ds1 <- ds1[,apply(is.na(ds1), 2, sum) == 0]

# the BigVAR function expects Txk data, so transpose the data
ds1 <- t(ds1)

############################################
############# Step 2: Modeling #############
############################################

## Perform model optimization for first set of series. Note the gran used to use it later

# "Basic" lasso penalty
# gran is the grid of penalty parameters
# # the first number is the depth, the second controls the number of values
var_mod <-constructModel(Y=ds1, p=12, struct="Basic", gran=c(70, 10), h=h, cv="LOO", verbose=TRUE, IC=TRUE)

results <- cv.BigVAR(var_mod)

plot(results)

## Perform model optimization for second set of series. Note the gran used to use it later

# store the series with the first length
ds2 <- ts_data[apply(is.na(ts_data), 1, sum) == num_na[2],]

# select only the columns without NA values
ds2 <- ds2[,apply(is.na(ds2), 2, sum) == 0]

# the BigVAR function expects Txk data, so transpose the data
ds2 <- t(ds2)

var_mod <-constructModel(Y=ds2, p=12, struct="Basic", gran=c(4000, 10), h=h, cv="LOO", verbose=TRUE, IC=TRUE)

results <- cv.BigVAR(var_mod)

plot(results)

## Perform model optimization for third set of series. Note the gran used to use it later

# store the series with the first length
ds3 <- ts_data[apply(is.na(ts_data), 1, sum) == num_na[3],]

# select only the columns without NA values
ds3 <- ds3[,apply(is.na(ds3), 2, sum) == 0]

# the BigVAR function expects Txk data, so transpose the data
ds3 <- t(ds3)

var_mod <-constructModel(Y=ds3, p=12, struct="Basic", gran=c(6000, 10), h=h, cv="Rolling", verbose=TRUE, IC=TRUE)

results <- cv.BigVAR(var_mod)

plot(results)

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

####################################################################
############# Combine these steps and save Forecasts ###############
####################################################################

# We have three lengths of time series, so we will use three lasso-VAR models, one for each length

# specify the gran vectors for each set of series
# find the file names

# protection_methods <- c("original", "Top", "Bottom", "AN", "DP")
protection_methods <- c("k_nts")

# protection_parameters <- list(c('none'), c(0.10, 0.20, 0.40), c(0.10, 0.20, 0.40), c(0.5, 1, 1.5, 2), c(0.1, 1, 4.6, 10, 20))
protection_parameters <- list(c(5, 10, 15))

gran_vecs <- list(c(70, 10), c(4000, 10), c(6000, 10))

# h_vec <- c(1, 18)
h_vec <- c(1)

sparse_fracs <- list(list(), list(), list())

## note that we have to use LOO cv for the first two groups when h=18 due to lack of data

for (h in h_vec){
  
  # index over protection methods
  for (p in seq_along(protection_methods)){
    # loop over parameter vectors
    for (protection_parameter in protection_parameters[[p]]){
      protection_method <- protection_methods[p]
    
      # Read in the pre-processed data
      fname <- paste0("../../Data/VAR_R_Datasets/h_", as.character(h), "_", protection_method, "_", protection_parameter, ".csv")
    
      ts_data <- read.csv(fname)
    
      # We have three lengths of time series, so we will use three lasso-VAR models, one for each length
      # First, we need to create the three dataframes containing the series with equal lengths
    
      # store the unique numbers of NA values
      num_na <- unique(apply(is.na(ts_data), 1, sum))
    
      ## do this loop for each file for h = 1
    
      Yhat <- list()
    
      for (i in seq_along(num_na)){
      
        print(num_na[i])
      
        # store the series with the current length
        ds <- ts_data[apply(is.na(ts_data), 1, sum) == num_na[i],]
      
        # select only the columns without NA values
        ds <- ds[,apply(is.na(ds), 2, sum) == 0]
      
        # the BigVAR function expects Txk data, so transpose the data
        ds <- t(ds)
        
        if ((h==18) & (i!=3)){
          cv_type <- "LOO"
        } else {
          cv_type <- "Rolling"
        }
      
        # construct sparse VAR model
        var_mod <- constructModel(Y=ds, p=12, struct="Basic", gran=gran_vecs[[i]], h=1, cv=cv_type, verbose=TRUE, IC=TRUE)
      
        # fit model and store results
        results <- cv.BigVAR(var_mod)
        
        sparse_fracs[[i]] <- append(sparse_fracs[[i]], results@sparse_count)
        
        yhat <- list()
        for (f in 1:h){
          # generate forecasts
          fs <- predict(results, n.ahead=f)
          yhat[[f]] <- fs
        }
        
        yhat <- do.call(cbind, yhat)
      
        # store forecasts
        Yhat[[i]] <- yhat
      
      }
      
      # combine forecasts into dataframe
      fcasts <- as.data.frame(do.call(rbind, Yhat))
      
      # save forecasts
      save_file <- paste0("../../Outputs/R_VAR_output/h_", as.character(h), "_", protection_method, "_", protection_parameter, ".csv")
      write.csv(fcasts, file=save_file, row.names=FALSE)
      
      print(paste0("Forecasting for ", protection_method, ": ", protection_parameter, " complete."))
      
    }
  }
}

### need to save the sparsity fraction measures

temp <- sparse_fracs

sparse_fracs <- lapply(sparse_fracs, function(x) do.call(rbind, x))

sparse_fracs <- do.call(cbind, sparse_fracs)














