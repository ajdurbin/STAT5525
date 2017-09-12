# Chapter 2 Normal Mixture Models Simulation
# Alexander Durbin
# Worry about optimization later

rm(list = ls())

library(tidyverse)
library(MASS)

set.seed(131)

# generate centroids
mu <- c(1, 0)
sigma <- diag(1, nrow = 2)
center <- mvrnorm(n = 5, mu = mu, Sigma = sigma)
centroids <- tibble(x1 = center[, 1], x2 = center[, 2], class = rep(0, 5))


mu <- c(0, 1)
center <- mvrnorm(n = 5, mu = mu, Sigma = sigma)
tmp <- tibble(x1 = center[, 1], x2 = center[, 2], class = rep(1, 5))

centroids <- bind_rows(centroids, tmp)

# generate data

my_data <- tibble(x1 = rep(0, 100), x2 = rep(0, 100), class = rep(NA, 100),
                  subclass = rep(NA, 100))

# make this a function with apply statement later
for (i in 1:nrow(my_data)) {
  
  row_index <- sample(1:10, 1)
  
  new_point <- mvrnorm(n = 1,
                       mu = c(centroids$x1[row_index], centroids$x2[row_index]),
                       Sigma = sigma / 5) 
  
  my_data$x1[i] = new_point[1]
  my_data$x2[i] = new_point[2]
  my_data$class[i] = centroids$class[row_index]
  my_data$subclass[i] = row_index
  
}

# linear model using x1, x2 to predict 0,1 response
lmfit <- lm(class ~ x1 + x2, data = my_data)

# create model matrix
tmp <- my_data %>% 
  dplyr::select(x1, x2) %>% 
  mutate(intercept = rep(1, nrow(my_data))) %>% 
  dplyr::select(intercept, x1, x2)

# predicted values
my_data$yhat <- as.matrix(tmp) %*% coefficients(lmfit)

# encoding of classes
my_data <- my_data %>% 
  mutate(pred_class = ifelse(yhat > 0.5, 1, 0))

my_data <- my_data %>% 
  mutate(x1_seq = seq(min(x1), max(x1), length.out = nrow(my_data))) %>% 
  mutate(x2_seq = (0.5 - coefficients(lmfit)[1] - coefficients(lmfit)[2] * x1_seq) / coefficients(lmfit)[3])

# plot data by class
ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(class))) +
  theme(legend.title=element_blank()) +
  ggtitle('TRUTH') +
  geom_line(mapping = aes(x = x1_seq, y = x2_seq))

ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(pred_class))) +
  theme(legend.title=element_blank()) +
  ggtitle('PREDICTED') +
  geom_line(mapping = aes(x = x1_seq, y = x2_seq))

# Get terms for confusion matrix as in HW0

a <- my_data %>% 
  filter(class == 0, pred_class == 0)
a <- nrow(a)

b <- my_data %>% 
  filter(class == 0, pred_class == 1)
b <- nrow(b)

c <- my_data %>% 
  filter(class == 1, pred_class == 0)
c <- nrow(c)

d <- my_data %>% 
  filter(class == 1, pred_class == 1)
d <- nrow(d)

f_pos <- b / (a + b)
f_neg <- c / (c + d)
