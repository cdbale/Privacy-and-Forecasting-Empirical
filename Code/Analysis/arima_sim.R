## simulate simple AR(1) processes to
## show how data protection changes time series features

library(forecast)
library(tidyverse)
library(ggplot2)

set.seed(1)

ss1_full <- arima.sim(list(order=c(1, 0, 0), ar=0.8), n=101)

ss1 <- ss1_full[1:(length(ss1_full)-1)]

ss2 <- arima.sim(list(order=c(1, 0, 0), ar=0.8), n=100)

ss3 <- arima.sim(list(order=c(1, 0, 0), ar=0.8), n=100)

ss_swapped <- c()

for (i in seq_along(ss1)){
  j <- sample(c(1, 2), size=1)
  if (j == 1){
    ss_swapped <- c(ss_swapped, ss2[i])
  }
  if (j == 2){
    ss_swapped <- c(ss_swapped, ss3[i])
  }
}

a1 <- arima(ss1, order=c(1,0,0))

a2 <- arima(ss2, order=c(1,0,0))

a3 <- arima(ss3, order=c(1,0,0))

a_swapped <- arima(ss_swapped, order=c(1,0,0))

additive_noise <- function(x){
  x_sd <- sd(x)
  randoms <- rnorm(n=length(x), mean=0, sd=2*x_sd)
  return(x+randoms)
}

noisy_ss <- additive_noise(ss1)

a_noisy <- arima(noisy_ss, order=c(1,0,0))

orig_pred <- predict(a1, n.ahead=1)$pred

noisy_pred <- predict(a_noisy, n.ahead=1)$pred

swapped_pred <- predict(a_swapped, n.ahead=1)$pred

combined <- tibble(Original=ss1, Swapped=ss_swapped, Noised=noisy_ss, time=1:length(ss1))

combined %>%
  gather(key="series", value="value", -time) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line() +
  facet_wrap(~series) +
  labs(y="X",
       x="Time")

# calculate absolute forecast error
abs(noisy_pred - ss1_full[length(ss1_full)])

abs(swapped_pred - ss1_full[length(ss1_full)])

abs(orig_pred - ss1_full[length(ss1_full)])

# model summaries
summary(a_noisy)

summary(a_swapped)

summary(a1)
