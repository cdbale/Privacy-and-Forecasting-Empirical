library(FNN)
library(entropy)

######################################################

gaussian_kl <- function(mu1, mu2, sigma1, sigma2){
  
  # closed form from Belov, D.I. and Armstrong, R.D. (2011). Distributions of the Kullback-Leibler divergence with applications
  kl_exp <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2)/(2 * sigma2^2) - 1/2
  # convert to bits
  kl_exp <- kl_exp/log(2)
  
  return(kl_exp)
}

simple_whiten <- function(x, y){
  mu <- mean(c(x, y))
  C <- var(c(x, y))
  new_x <- 1/sqrt(C) * (x-mu)
  new_y <- 1/sqrt(C) * (y-mu)
  return(list(x=new_x, y=new_y))
}

KL.divergence.custom<- function(X, Y, k=10, algorithm=c("kd_tree", "cover_tree", "brute"))
{
  #Kullback-Leibler Distance
  algorithm<- match.arg(algorithm);
  
  if (!is.matrix(X))  X<- matrix(X);
  if (!is.matrix(Y))  Y<- matrix(Y);
  
  n<- nrow(X); p<- ncol(X);
  m<- nrow(Y);
  
  xy_dist <- knnx.dist(Y, X, k=k, algorithm) + .Machine$double.eps
  x_dist <- knn.dist(X, k=k, algorithm) + .Machine$double.eps
  
  log(m/(n-1)) + p*(colMeans(log(xy_dist))- colMeans(log(x_dist)));
  
}

KLD <- function(x1, x2, k){
  x <- simple_whiten(x1, x2)
  x1 <- x[[1]]
  x2 <- x[[2]]
  ds <- KL.divergence.custom(x1, x2, k=k)/log(2)
  return(ds[k])
}

KLcalc <- function(x1,x2){
  
  # density estimates
  y1d <- density(x1, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))
  y2d <- density(x2, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)))
  
  # density estimates
  y1 <- density(x1, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)), bw=min(y1d$bw,y2d$bw))$y
  y2 <- density(x2, from=min(min(x1), min(x2)), to=max(max(x1), max(x2)), bw=min(y1d$bw,y2d$bw))$y
  
  # convert to probabilities
  y1 <- y1/sum(y1)
  y2 <- y2/sum(y2)
  
  # fix division by 0 issue
  y1 <- y1 + .Machine$double.eps
  y2 <- y2 + .Machine$double.eps
  
  kl<-round(KL.Dirichlet(y1, y2, a1=0, a2=0, unit="log2"),3)
  ent<-entropy.Dirichlet(y1, a=0, unit="log2")
  info<-round(kl*100/ent,2)
  
  return(list(kl=kl,ent=ent,info=info))
}

###############################################

m1 <- 10
m2 <- 10
s1 <- 1
s2 <- 2

# set.seed(100)
x1 <- rnorm(10000,m1,s1)
x2 <- rnorm(10000,m2,s2)

KLD(x1, x2, k=3)

KLcalc(x1, x2)

gaussian_kl(m1, m2, s1, s2)

