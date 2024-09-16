# This script offers an implementation of the REP criterion for selecting 
# between forecasts, as proposed by Petropoulos & Siemsen (2021). The code 
# for the main function (REP) is followed by a short example of how this can 
# be applied. 
# 
# Reference
# Petropoulos F. & Siemsen E. (2021) "Forecast selection and representativeness", 
# Management Science, forthcoming

library(forecast)

REP <- function(y, fitted=NULL, f, lambda = NULL, insample = TRUE,
                delta = 0.5, norm.pm = 1, norm.rep = 1) {
  
  # *** Arguments for the REP() function ***
  # y: the series to be forecast; cannot be NULL; must be a time series object.
  # fitted: the in-sample fitted values; cannot be NULL; must be of the same length as y; cannot be NULL if "insample" is TRUE.
  # f: the out-of-sample point forecasts; cannot be NULL.
  # lambda: the value of lambda for the Box-Cox transformation; if null, it will be estimated.
  # insample: boolean argument for including (or not) the performance gap in the REP calculation [see equations (4) and (5) in Petropoulos & Siemsen (2021)].
  # delta: the discount factor in the calculation of the representativeness.
  # norm.pm: the norm for calculating the performance measure; possible values are 1 (the default value, sum of absolute differences) and 2 (sum of squared differences).
  # norm.rep: the norm for calculating the representativeness; possible values are 1 (the default value, sum of absolute differences) and 2 (sum of squared differences).

  # set frequency and horizon
  frequency <- frequency(y)
  h <- length(f)
  n <- length(y)
  
  # transform the data using BoxCox
  if (is.null(lambda)){
    lambda <- BoxCox.lambda(y, lower=0, upper=1)
  }
  ybc <- BoxCox(y, lambda)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # *** measure performance gap ***
  
  if (insample){
    
    # transform fitted values using BoxCox
    fittedbc <- BoxCox(fitted, lambda)
    
    # scale the actual data and the fitted values
    if (all(ybc[1] == ybc)){ 
      fs <- (fittedbc - mean(fittedbc))
      ys <- rep(0, H)
    } else {
      fs <- (fittedbc - mean(fittedbc)) / sd(ybc)
      ys <- as.vector(scale(ybc))
    }
    
    # calculate performance gap
    if (norm.pm == 1){
      performance_gap <- sum(abs(ys - fs), na.rm=TRUE)
    } else if (norm.pm == 2){
      performance_gap <- sum((ys - fs)^2, na.rm=TRUE)
    }
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # *** measure representativeness gap ***
  
  # set the length of in-sample windows for measuring representativeness
  p <- ceiling(h / frequency) * frequency
  
  # transform forecasts using BoxCox
  fbc = BoxCox(f, lambda)
  
  # initialise representativeness gap
  representativeness_gap <- 0
  
  # for-loop to compare with the data windows in the in-sample 
  # [see equation (3) in Petropoulos & Siemsen (2021)]
  for (i in 1:floor(n/p)){
    
    # get the respective window of data
    x <- as.vector(ybc[(n - i * p + 1):(n - (i - 1) * p)])[1:h]
    
    # scale that window of the data and the forecasts
    if (all(x[1] == x)){
      fs <- (fbc - mean(fbc))
      xs <- rep(0, h)
    } else {
      fs <- (fbc - mean(fbc)) / sd(x)
      xs <- as.vector(scale(x))
    }
    
    # calculate representativeness in that window, and update representativeness_gap
    if (norm.rep == 1){
      representativeness_gap <- representativeness_gap + ((1 - delta)^(i-1)) * sum(abs(xs - fs))
    } else if (norm.rep == 2){
      representativeness_gap <- representativeness_gap + ((1 - delta)^(i-1)) * sum((xs - fs)^2)
    }

  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # *** REP ***
  # [see equations (4) and (5) in Petropoulos & Siemsen (2021)]
  if (insample){
    return(list("REP"=performance_gap + representativeness_gap, "Perf_Gap"=performance_gap, "Rep_Gap"=representativeness_gap))
  } else {
    return(representativeness_gap)
  }

}

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # *** EXAMPLE ***
# 
# # let us assume some (trended and seasonal) data
# y <- AirPassengers
# 
# # we will produce forecasts using Holt's linear trend exponential smoothing
# # which is equivalent to ETS(A,A,N)...
# model1 <- ets(y, model="AAN")
# # ...and we will extract the fitted values and produce some forecasts with h=12
# fitted1 <- model1$fitted
# fcs1 <- forecast(model1, h=12)$mean
# 
# # we will repeat for the Holt-Winters method, or ETS(A,A,A)
# model2 <- ets(y, model="AAA")
# fitted2 <- model1$fitted
# fcs2 <- forecast(model2, h=12)$mean
# 
# # we can now select between the forecasts of these two models using REP...
# REP1 <- REP(y, fitted1, fcs1)
# REP2 <- REP(y, fitted2, fcs2)
# # ...and we can conclude that, in this case, ETS(A,A,A) should be preferred over
# # ETS(A,A,N) as it has a lower REP value.