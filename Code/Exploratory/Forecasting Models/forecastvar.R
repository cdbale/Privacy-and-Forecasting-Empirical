library(vars)
library(forecast)
library(fpp2)
library(ggplot2)
library(tidyverse)
require(gridExtra)

VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]

var1 <- VAR(uschange[,1:2], p=1, type="const")

serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(uschange[,1:2], p=2, type="const")

serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(uschange[,1:2], p=3, type="const")

#serial autocorrelation, dont want low pvalue
serial.test(var3, lags.pt=10, type="PT.asymptotic")


s=seq(.1,10000,length=10)


results=list()
## number of autoregressive parameters
p=3
## for later plotting of coefficients
plot_coef=matrix(nrow=length(s),ncol=2*p+1)

cmeans <- c()
imeans <- c()
cints <- c()
iints <- c()

for (i in 1:length(s)){
    
    results[[i]]=list()
    x=uschange[,1:2]
    dim(x)
    ## add noise to the raw data, going from small to large
    x=x+rnorm(prod(dim(x)),0,s[i])
    summary(x)
    results[[i]][[1]]=s[i]
    ## fit the same model with 3 lags 
    var3 <- VAR(x, p=p, type="const")
    # print(var3$varresult)
    var3
    # print(i)
    ## summary stats of quarterly forecast
    # print(summary((((forecast(var3))$forecast)[[1]])$mean))
    results[[i]][[2]]=summary((((forecast(var3))$forecast)[[1]])$mean)
    ## coefficients from first time series
    # print(var3$varresult[[1]]$coefficients)  
    plot_coef[i,]=results[[i]][[3]]=round(var3$varresult[[1]]$coefficients,3)
    
    # store data means
    data_means <- apply(x, 2, mean)
    cmeans <- c(cmeans, data_means[1])
    imeans <- c(imeans, data_means[2])
    
    
    cints <- c(cints, var3$varresult$Consumption$coefficients['const'])
    iints <- c(iints, var3$varresult$Income$coefficients['const'])
    ## means of each time series vs. the intercept from the model
    # results[[i]][[4]] = list(apply(x, 2, mean), 
    #                          var3$varresult$Consumption$coefficients['const'],
    #                          var3$varresult$Income$coefficients['const'])
    
      }

results

## reclass as time series object
plot_coef=as.ts(plot_coef)


## AR coefficients from 2 time series
fig1<-plot_coef[,1:(2*p)]%>%autoplot()+xlab("Additive Noise Level")+
  ylab("Coefficient Value")+ggtitle("VAR parameters")+scale_x_discrete(labels=s)

## AR coefficients from 2 time series
fig2<-plot_coef[,(2*p+1)]%>%autoplot()+xlab("Additive Noise Level")+
  ylab("Intecept Value")+ggtitle("VAR parameters")+scale_x_discrete(labels=s)

grid.arrange(fig1,fig2,nrow=2)

forecast(var3) %>%
  autoplot() + xlab("Year")

## it looks like (roughly) the intercept follows the mean of the data
noise <- 1:10
tibble(cmeans, imeans, cints, iints, noise) %>%
  rename("Mean Consumption"=cmeans, "Mean Income"=imeans, "Income Intercept"=iints, "Consumption Intercept"=cints) %>%
  gather(key="line", value="value", -noise) %>%
  ggplot(aes(x=noise, y=value, color=line)) +
  geom_line(size=1.5) 



