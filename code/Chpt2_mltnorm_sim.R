# Chapter 2 Normal Mixture Models Simulation
# Alexander Durbin

library(tidyverse)
library(MASS)

# first generate 10 centroids
mu <- c(0,0)
sigma <- diag(1, nrow = 2)
centers <- mvrnorm(10, mu = mu, Sigma = sigma)