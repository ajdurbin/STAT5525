# Chapter 2 Normal Mixture Models Simulation
# Alexander Durbin
# Worry about optimization later

rm(list = ls())

library(tidyverse)
library(MASS)

set.seed(12345)

# generate centroids
mu <- c(1, 0)
sigma <- diag(1, nrow = 2)
center <- mvrnorm(n = 5, mu = mu, Sigma = sigma)
centroids <- tibble(x1 = center[, 1], x2 = center[, 2], class = rep(1, 5))


mu <- c(0, 1)
center <- mvrnorm(n = 5, mu = mu, Sigma = sigma)
tmp <- tibble(x1 = center[, 1], x2 = center[, 2], class = rep(2, 5))

centroids <- bind_rows(centroids, tmp)

# garbage collection
rm(mu, center, tmp)

# generate data

my_data <- tibble(x1 = rep(0, 100), x2 = rep(0, 100), class = rep(NA, 100),
                  subclass = rep(NA, 100))

for (i in 1:nrow(my_data)) {
  
  row_index <- sample(1:10, 1)
  
  new_point <- mvrnorm(n = 1,
                       mu = c(centroids$x1[row_index], centroids$x2[row_index]),
                       Sigma = sigma) 
  
  my_data$x1[i] = new_point[1]
  my_data$x2[i] = new_point[2]
  my_data$class[i] = centroids$class[row_index]
  my_data$subclass[i] = row_index
  
}


# plot data by class
ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(class)))

# plot data by subclass
ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(subclass)))