# create time series y
yt <- rnorm(100, mean = 0, sd = 50)

# create noisy version of y
s <- seq(10,10000,length=10)
yt_noisy <- yt + rnorm(length(yt), 0, s[10])

# create lagged versions of y and noisy y
yt_1 <- yt[1:length(yt)-1]
yt_1_noisy <- yt_noisy[1:length(yt)-1]

# remove first y valuea
yt <- yt[2:length(yt)]
yt_noisy <- yt_noisy[2:length(yt_noisy)]

# regression for y
model_orig <- lm(yt~yt_1)
# regression summary
summary(model_orig)

# regression for noisy y
model_noisy <- lm(yt_noisy~yt_1_noisy)
# regression summary
summary(model_noisy)

## plot data with estimated intercepts
# plot y
plot(yt, type="l", col='red')
# plot lagged y
lines(yt_1, col='blue')
abline(h=model_orig$coefficients[1])

# plot noisy versions
plot(yt_noisy, type="l", col='red')
lines(yt_1_noisy, col='blue')
abline(h=model_noisy$coefficients[1])

model_orig$coefficients[1]

model_noisy$coefficients[1]