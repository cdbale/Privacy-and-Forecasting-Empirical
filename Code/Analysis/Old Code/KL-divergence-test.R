## Testing KL-divergence method against closed-form KL divergence for Gaussian distributions

# Author: Cameron Bale

library(tidyverse)
library(ggplot2)

###################################################################

kl_divergence <- function(x1, x2){
  
  # density estimates
  y1 <- density(x1, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))$y
  y2 <- density(x2, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))$y
  
  # convert to probabilities
  y1 <- y1/sum(y1)
  y2 <- y2/sum(y2)
  
  # fix division by 0 issue
  y1 <- y1 + .Machine$double.eps
  y2 <- y2 + .Machine$double.eps
  
  # calculate entropy of target distribution
  ent <- -sum(y1*log2(y1))
  
  # KL divergence
  kl <- sum(y1 * log2(y1/y2))
  
  # % divergence
  info <- sum(y1 * log2(y1/y2))/ent * 100
  
  return(list(ent=ent, kl=kl, info=info))
}

gaussian_kl <- function(mu1, mu2, sigma1, sigma2){
  
  # expected from Belov, D.I. and Armstrong, R.D. (2011). Distributions of the Kullback-Leibler divergence with applications
  kl_exp <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2)/(2 * sigma2^2) - 1/2
  # convert to bits
  kl_exp <- kl_exp/log(2)
  
  return(kl_exp)
}

kl_calc_sim <- function(m1, m2, s1, s2, seed){
  
  # seed for reproducability
  set.seed(seed)
  
  # simulated x's
  x1 <- rnorm(1000,m1,s1)
  x2 <- rnorm(1000,m2,s2)
  
  # expected KL divergence
  true_kl <- gaussian_kl(mu1=m1, mu2=m2, sigma1=s1, sigma2=s2)
  
  names(true_kl) <- "true_kl"
  
  res <- kl_divergence(x1=x1, x2=x2)
  
  return(append(res, true_kl))
}

###################################################################

# normal density parameters
sigma1 <- 10
mu1 <- 10
mu2 <- 10

seeds <- 1:100
sigma2s <- sort(runif(100, 1, 20))

results <- lapply(sigma2s, function(y) lapply(seeds, function(x) kl_calc_sim(mu1, mu2, sigma1, y, x)))

results_df <- do.call(bind_rows, lapply(results, function(x) lapply(x, as_tibble)))

results_df %>%
  group_by(true_kl) %>%
  mutate(mean_kl = mean(kl)) %>%
  ungroup() %>%
  distinct(true_kl, mean_kl) %>%
  mutate(n=1:n()) %>%
  ggplot(aes(x=n, y=true_kl, color='red')) +
  geom_line() +
  geom_line(aes(y=mean_kl, color='blue'))

###################################################################





