library(MASS)
library(mvtnorm)

## expectation step
## find probability of each cluster for each point
E.step <- function(X, phi, N) {
  h <-
    with(phi, do.call(cbind,
                      lapply(1:N, function(i)
                        dmvnorm(X, mu[[i]], sig[[i]]))))
  h/rowSums(h)  #Normalize
}

## maximization step
## given the probability of each cluster for each point
## find the values of mu, sigma, and alpha that maximize the likelihood
M.step <- function(X, h, N) {
  covs <- lapply(1:N, function(i) cov.wt(X, h[,i]))
  mu <- lapply(covs, "[[", "center")
  sig <- lapply(covs, "[[", "cov")
  alpha <- colMeans(h)
  list(mu = mu, sig = sig, alpha = alpha)
}

## log.likelihood
## given points and parameters, how well does our model fit
## also gives us a terminating condition 
## stop if don't improve much
## also used for AIC for model selection (choosing a value of k)
log.like <- function(X, phi, N) {
  probs <- 
    with(phi, do.call(cbind,
                      lapply(1:N, function(i)
                        alpha[i] * dmvnorm(X, mu[[i]], sig[[i]]))))
  sum(log(rowSums(probs)))
}

## main program
## X = dataset
## N = number of clusters
## Terminate if log.like @ (t-1) - log.like @ (t) is below a threshold
run.em <- function(X, N) {
  covs <- replicate(N, list(cov.wt(X[sample(nrow(X), 30),])))
  mu <- lapply(covs, "[[", "center")
  sig <- lapply(covs, "[[", "cov")
  alpha <- rep(1/N, N)
  phi <<- list(mu = mu, sig = sig, alpha = alpha)
  
  for(i in 1:max.iter) {
    oldphi <- phi
    h <<- E.step(X, phi, N)
    phi <<- M.step(X, h, N)
    if((log.like(X, phi, N) - log.like(X, oldphi, N)) < 0.01)
      break
  }
  return(list(phi = phi, aic = 2*3*N - log.like(X, phi, N)))
}
