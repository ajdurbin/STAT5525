# https://gist.github.com/mm--/8576015

library(MASS)
library(mvtnorm)

rm(list = ls())
set.seed(12345)

## expectation step
## find probability of each cluster for each point
E.step <- function(X, phi, N) {
  
  h <-
    with(phi, do.call(cbind,
                      lapply(1:N, function(i)
                        dmvnorm(X, mu[[i]], sig[[i]]))))
  
  return(h/rowSums(h))  #Normalize
  
}

## maximization step
## given the probability of each cluster for each point
## find the values of mu, sigma, and alpha that maximizelikelihood
M.step <- function(X, h, N) {
  
  covs <- lapply(1:N, function(i) cov.wt(X, h[,i]))
  mu <- lapply(covs, "[[", "center")
  sig <- lapply(covs, "[[", "cov")
  alpha <- colMeans(h)
  
  return(list(mu = mu, sig = sig, alpha = alpha))
  
}

## log.likelihood
## given points and parameters, how well does model fit
## also gives terminating condition 
## stop if don't improve much
## used for AIC model selection (choosing a value of k)
log.like <- function(X, phi, N) {
  
  probs <- 
    with(phi, do.call(cbind,
                      lapply(1:N, function(i)
                        alpha[i] * dmvnorm(X, mu[[i]], sig[[i]]))))
  
  return(sum(log(rowSums(probs))))
  
}

## main program
## X = dataset
## N = number of clusters
## Terminate if log.like @ (t-1) - log.like @ (t) is below a threshold
run.em <- function(X, N, max.iter = 30) {
  
  # get initial estimates of means and covariances for each cluster
  # randomly sample 30 points and build estimates
  # covs is list of lists
  # first list is list of first cluster information
  # etc
  covs <- replicate(N, list(cov.wt(X[sample(nrow(X), 30),])))
  
  # list of cluster means
  mu <- lapply(covs, "[[", "center")
  
  # list of cluster variances
  sig <- lapply(covs, "[[", "cov")
  
  # weights
  alpha <- rep(1 / N, N)
  
  # list of lists of parameters for each cluster
  phi <<- list(mu = mu, sig = sig, alpha = alpha)
  
  # repeat until convergence or maximum iterations reached
  for(i in 1:max.iter) {
    
    oldphi <- phi
    # e step
    h <<- E.step(X, phi, N)
    # m step
    phi <<- M.step(X, h, N)
    
    # check for convergence with log-likelihood
    if((log.like(X, phi, N) - log.like(X, oldphi, N)) < 0.01)
      break
  }
  
  # return cluster information and aic
  return(list(phi = phi, aic = 2 * 3 * N - log.like(X, phi, N)))
  
}

## example run
two.cluster.data <- 
  rbind(rmvnorm(n = 500, mean = c(0, 0), sigma = diag(x = 2) * 5),
        rmvnorm(n = 1000, mean = c(3, 0),
                sigma = matrix(data = c(5, 2, 2, 1), nrow = 2, ncol = 2)))

em <- run.em(X = two.cluster.data, N = 2, max.iter = 100)
km <- kmeans(x = two.cluster.data, centers = 2, iter.max = 30)
