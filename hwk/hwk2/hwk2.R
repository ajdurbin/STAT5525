## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = FALSE)

## ---- include=FALSE------------------------------------------------------
# for logistic regression and cross validation
library(caret)
# for logistic lasso
library(glmnet)
# for classification tree
library(rpart)

## ------------------------------------------------------------------------

set.seed(130)
# load spam data set
raw <- read.table("spam.data.txt")
# standardize columns
raw[, 1 : 57] <- scale(raw[, 1 : 57])
# factor the response
raw[, 58] <- as.factor(raw[, 58])


## ------------------------------------------------------------------------

trc <- trainControl(method = "cv", number = 10)
carfit <- train(V58 ~ ., data = raw, trControl = trc, method = "rpart", 
                control = list(minsplit = 2, cp = 10, maxcompete = 1000,
                               minbucket = 1))
carfit
carfit$finalModel
carfit$resample

## ---- warning=FALSE------------------------------------------------------

trc <- trainControl(method = "cv", number = 10)
logfit <- train(V58 ~ ., data = raw, trControl = trc, method = "glm",
               family = binomial())  
logfit$resample


## ------------------------------------------------------------------------

1 - logfit$resample[, 1]


## ------------------------------------------------------------------------

1 - logfit$results[, 2]


## ------------------------------------------------------------------------

x <- as.matrix(raw[, 1 : 57])
y <- raw[, 58]

lassofit <- cv.glmnet(x = as.matrix(x), y = y, nfolds = 10, type.measure = "deviance", alpha = 1, family = "binomial")

preds <- predict(lassofit, as.matrix(x), s = "lambda.min", type = "class")

paste0(" Overall error rate: ", 1 - sum(y == preds) / nrow(raw))


