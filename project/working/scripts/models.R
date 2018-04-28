# fit some models to get some results
library(tidyverse)
library(glmnet)
library(caret)
library(rpart)
library(randomForest)

# rm(list = ls())


# usb distribution models -------------------------------------------------


# usb_distribution <- read_csv("usb_distribution.csv")
raw <- usb_distribution[, -c(1, 2, 4)]
trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                family = binomial())  
paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                allowParallel = TRUE)
paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])


# web distribution --------------------------------------------------------


# web_distribution <- read_csv("web_distribution.csv")
raw <- web_distribution[, -c(1,2,8)]
trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                family = binomial())  
paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                allowParallel = TRUE)
paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])

# logon distribution --------------------------------------------------


# logon_distribution <- read_csv("logon_distribution.csv")
raw <- logon_distribution[, -c(1, 2, 4)]
trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                family = binomial())  
paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                allowParallel = TRUE)
paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])
