# Chapter 2 Normal Mixture Models Simulation
# Alexander Durbin
# Worry about optimization later

rm(list = ls())

library(tidyverse)
library(MASS)
library(mvtnorm)
library(mnormt)

set.seed(1)


# Generate Data -----------------------------------------------------------

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


# Least squares classification and error rates ----------------------------

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
  mutate(pred_class_reg = ifelse(yhat > 0.5, 1, 0))

my_data <- my_data %>% 
  mutate(x1_seq = seq(min(x1), max(x1), length.out = nrow(my_data))) %>% 
  mutate(x2_seq = (0.5 - coefficients(lmfit)[1] - coefficients(lmfit)[2] * x1_seq) / coefficients(lmfit)[3])

# plot data by class
ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(class))) +
  theme(legend.title=element_blank()) +
  ggtitle('TRUTH') +
  geom_line(mapping = aes(x = x1_seq, y = x2_seq)) +
  ylim(-5, 5) +
  xlim(-5, 5)

ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(pred_class_reg))) +
  theme(legend.title=element_blank()) +
  ggtitle('LINEAR REGRESSION PREDICTED') +
  geom_line(mapping = aes(x = x1_seq, y = x2_seq)) +
  ylim(-5, 5) +
  xlim(-5, 5)

# Get terms for confusion matrix as in HW0

a <- my_data %>% 
  filter(class == 0, pred_class_reg == 0) %>% 
  nrow()

b <- my_data %>% 
  filter(class == 0, pred_class_reg == 1) %>% 
  nrow()

c <- my_data %>% 
  filter(class == 1, pred_class_reg == 0) %>% 
  nrow()

d <- my_data %>% 
  filter(class == 1, pred_class_reg == 1) %>% 
  nrow()

f_pos_reg <- b / (a + b)
f_neg_reg <- c / (c + d)


# Optimal separating boundary for sequence data ---------------------------------------------

# generate sequences for calculating class probabilities and plotting
# x1_seq <- seq(min(my_data$x1), max(my_data$x1), by = 0.01)
# x2_seq <- seq(min(my_data$x2), max(my_data$x2), length.out = length(x1_seq))
# seq_mat <- cbind(x1_seq, x2_seq)

# function to compute the normal density given data and centroid
# consider using dmvtnorm instead
# norm_den <- function(vec, cent){
#   
#   diff <- vec - cent
#   diff <- as.matrix(diff)
#   thing <- exp((-5 / 2) * diff %*% t(diff))
#   return(thing)
#   
# }

# class 1 numerator and denominator storage
# c1_num <- matrix(data = 0, ncol = 5, nrow = length(x1_seq))
# c1_den <- matrix(data = 0, ncol = 10, nrow = length(x1_seq))

# loop through the centroids and fill each column of c1_num with the density
# for that particular centroid
# for(i in 1:5){
#   # c1_num[, i] <- apply(seq_mat, 1, function(g) norm_den(g, centroids[i, 1:2]))
#   c1_num[, i] <- apply(
#     seq_mat, 1,
#     function(g) dmvnorm(x = g, mean = as.matrix(centroids[i, 1:2]), sigma = diag(1/5, nrow = 2))
#   )
# }
# 
# # do the same except for all 10 centroids
# for(i in 1:10){
#   # c1_den[, i] <- apply(seq_mat, 1, function(g) norm_den(g, centroids[i, 1:2]))
#   c1_den[, i] <- apply(
#     seq_mat, 1,
#     function(g) dmvnorm(x = g, mean = as.matrix(centroids[i, 1:2]), sigma = diag(1/5, nrow = 2))
#   )
# }
# 
# # now sum across the rows
# c1_num <- apply(c1_num, 1, sum)
# c1_den <- apply(c1_den, 1, sum)
# 
# # make a dataframe and add column of the probabilities derived in class
# # make a new column of 1 - class 1 probs = class 2 probs
# seq_mat <- as.tibble(seq_mat) %>%
#   mutate(class_1_probs = c1_num/c1_den) %>% 
#   mutate(class_2_probs = 1 - class_1_probs)
#   
# # find observations where class probabilites are approximately equal
# seq_mat <- seq_mat %>% 
#   filter(abs(class_1_probs - class_2_probs) < 5e-2)
# 
# # failed plot attempt
# ggplot() +
#   geom_point(data = my_data, mapping = aes(x = x1, y = x2, color = factor(class))) +
#   theme(legend.title=element_blank()) +
#   ggtitle('TRUTH') +
#   geom_curve(data = seq_mat, mapping = aes(x = x1_seq, y = x2_seq, xend = min(my_data$x1), yend = min(my_data$x2)))
#   

# Bayes Optimal Classifier for true data ----------------------------------

c1_num <- matrix(data = 0, ncol = 5, nrow = nrow(my_data))
c1_den <- matrix(data = 0, ncol = 10, nrow = nrow(my_data))

# loop through the centroids and fill each column of c1_num with the density
# for that particular centroid
for(i in 1:5){
  # c1_num[, i] <- apply(seq_mat, 1, function(g) norm_den(g, centroids[i, 1:2]))
  c1_num[, i] <- apply(
    as.matrix(my_data[, 1:2]), 1,
    function(g) dmvnorm(x = g, mean = as.matrix(centroids[i, 1:2]), sigma = diag(1/5, nrow = 2))
  )
}

# do the same except for all 10 centroids
for(i in 1:10){
  # c1_den[, i] <- apply(seq_mat, 1, function(g) norm_den(g, centroids[i, 1:2]))
  c1_den[, i] <- apply(
    as.matrix(my_data[, 1:2]), 1,
    function(g) dmvnorm(x = g, mean = as.matrix(centroids[i, 1:2]), sigma = diag(1/5, nrow = 2))
  )
}

# now sum across the rows
c1_num <- apply(c1_num, 1, sum)
c1_den <- apply(c1_den, 1, sum)

# make a dataframe and add column of the probabilities derived in class
# make a new column of 1 - class 1 probs = class 2 probs
my_data <- my_data %>%
  mutate(class_1_probs = c1_num/c1_den) %>% 
  mutate(class_2_probs = 1 - class_1_probs) %>% 
  mutate(bayes_pred_class = ifelse(class_1_probs > class_2_probs, 0, 1))

# Get terms for confusion matrix as in HW0

a <- my_data %>% 
  filter(class == 0, bayes_pred_class == 0) %>% 
  nrow()

b <- my_data %>% 
  filter(class == 0, bayes_pred_class == 1) %>% 
  nrow()

c <- my_data %>% 
  filter(class == 1, bayes_pred_class == 0) %>% 
  nrow()

d <- my_data %>% 
  filter(class == 1, bayes_pred_class == 1) %>% 
  nrow()

f_pos_bayes <- b / (a + b)
f_neg_bayes <- c / (c + d)

my_data %>% 
  filter((class_1_probs - class_2_probs) < 1e-5)
# 45 things close together

ggplot(data = my_data) +
  geom_point(mapping = aes(x = x1, y = x2, color = factor(bayes_pred_class))) +
  theme(legend.title=element_blank()) +
  ggtitle('BAYES PREDICTED') +
  ylim(-5, 5) +
  xlim(-5, 5)


# Bayes Decision boundary plot --------------------------------------------

# create grid of points
# grid.vector1 = seq(min(my_data$x1), max(my_data$x1), 0.1)
# grid.vector2 = seq(min(my_data$x2), max(my_data$x2), 0.1)
grid.vector1 = seq(-5, 5, by = 0.1)
grid.vector2 = seq(-5, 5, by = 0.1)
grid = expand.grid(grid.vector1, grid.vector2)

#calculate density for each point on grid for each of the multivariates distributions
prob.1 = matrix(0:0, nrow = nrow(grid), ncol = 5) #initialize grid
for (i in 1:nrow(grid)){
  for (j in 1:5){
    prob.1[i, j] = dmnorm(grid[i, ], centroids[j, 1:2], diag(1/5, nrow = 2))  
  }
}
prob.1
prob1.max = apply(prob.1, 1, max)

#second class - as per above
prob.2 = matrix(0:0, nrow = nrow(grid), ncol = 5) #initialize grid
for (i in 1:nrow(grid)){
  for (j in 6:10){
    prob.2[i, j - 5] = dmnorm(grid[i, ], centroids[j, 1:2], diag(1/5, nrow = 2))  
  }
}
prob.2
prob2.max = apply(prob.2, 1, max)


prob.total = cbind(prob1.max, prob2.max)
class = rep(1, times = length(prob1.max))
class[prob1.max < prob2.max] = 2
cbind(prob.total, class)

# plot(grid[, 1], 
#      grid[, 2],
#      pch = ".", 
#      cex = 2,
#      col = ifelse(class == 1, "coral", "cornflowerblue"),
#      xlab = "x1",
#      ylab = "x2",
#      main = "Optimal Bayes Classifier Decision Boundary
#      # xlim = c(-5, 5),
#      # ylim = c(-5, 5)
#     )

plot(grid[, 1], 
     grid[, 2],
     pch = ".", 
     cex = 2,
     col = ifelse(class == 1, "coral", "cornflowerblue"),
     xlab = "x1",
     ylab = "x2",
     xlim = c(-5, 5),
     ylim = c(-5, 5),
     main = "Optimal Bayes Classifier Decision Boundary"
    )


values1 <- my_data %>% 
  filter(class == 0)

values2 <- my_data %>% 
  filter(class == 1)

points(values1, col = "coral")
points(values2, col = "cornflowerblue") 

prob.bayes <- matrix(prob1.max / (prob1.max + prob2.max),
                     length(grid.vector1),
                     length(grid.vector2)
                    )

contour(x = grid.vector1, 
        y = grid.vector2,
        z = prob.bayes, 
        levels = 0.5, 
        labels = "", 
        lwd = 1,
        add = TRUE
        )
