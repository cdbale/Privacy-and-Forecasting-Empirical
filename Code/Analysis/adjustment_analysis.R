#### Analysis of Forecasts from Private Data ####

# Assess how adjustment direction and the volatility
# of time series relates to whether an adjustment 
# was beneficial when doing swapping.

### import forecasts for k-nts+ (k = 3) swapped for all forecasting models
all_forecasts <- list.files("../../Outputs/Forecasts/")
all_forecasts <- grep("h1_", all_forecasts, value=TRUE)

# filter to only k-nts+ (k = 3) and the original forecasts
forecasts <- all_forecasts[grepl("original", all_forecasts) | grepl("k_nts_plus", all_forecasts)]


# import original forecasts for all forecasting models

# compute (or import) whether the forecast adjustment was beneficial

# compute the relationship between whether an adjustment was beneficial
# and the volatility of the original series and direction of adjustment