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

# load spam data set
raw <- read.table("spam.data.txt")
# standardize columns
raw[, 1 : 57] <- scale(raw[, 1 : 57])
# factor the response
raw[, 58] <- as.factor(raw[, 58])


## ------------------------------------------------------------------------

# error storage
preders <- rep(0, 10)
# split into training and testing
train_index <- createDataPartition(y = raw[, 58], times = 10, p = 0.9, list = FALSE)

for(i in 1 : 10){
  
  # train/test sets
  raw_train <- raw[train_index[, i], ]
  raw_test <- raw[-train_index[, i], ]
  
  # initial fit
  carfit <- rpart(V58 ~ ., data = raw_train, method = "class",
                  control = list(cp = .Machine$double.eps, minsplit = 2, 
                                 minbucket = 1))
  
  # make predictions
  carpred <- predict(carfit, newdata = raw_test[, 1:57], type="class")
  # confusion matrix and error rate
  
  conf <- table(raw_test[, 58], carpred)
  preders[i] <- (conf[2]  + conf[3]) / (conf[1] + conf[4])
  
}

paste0("The overall classification error is: ", mean(preders))


## ------------------------------------------------------------------------

# error storage
preders <- rep(0, 10)
# split into training and testing
train_index <- createDataPartition(y = raw[, 58], times = 10, p = 0.9, list = FALSE)

for(i in 1 : 10){
  
  # train/test sets
  raw_train <- raw[train_index[, i], ]
  raw_test <- raw[-train_index[, i], ]
  
  # initial fit
  carfit <- rpart(V58 ~ ., data = raw_train, method = "class",
                  control = list(cp = .Machine$double.eps, minsplit = 2, 
                                 minbucket = 1))
  # find best cp value 
  bestcp <- carfit$cptable[which.min(carfit$cptable[,"xerror"]),"CP"]
  
  # prune tree 
  carfit_prune <- prune(carfit, cp = bestcp)
  
  # make predictions
  carpred <- predict(carfit_prune, newdata = raw_test[, 1:57], type="class")
  
  # confusion matrix and error rate
  conf <- table(raw_test[, 58], carpred)
  preders[i] <- (conf[2]  + conf[3]) / (conf[1] + conf[4])
  
}

paste0("The overall classification error is: ", mean(preders))


## ---- warning=FALSE------------------------------------------------------

trc <- trainControl(method = "cv", number = 10)
logfit <- train(V58 ~ ., data = raw, trControl = trc, method = "glm",
               family = binomial())  
logfit$resample


## ------------------------------------------------------------------------

1 - logfit$resample[, 1]


## ------------------------------------------------------------------------

paste0("Overall error rate: ", 1 - logfit$results[, 2])


## ------------------------------------------------------------------------

x <- as.matrix(raw[, 1 : 57])
y <- raw[, 58]

lassofit <- cv.glmnet(x = as.matrix(x), y = y, nfolds = 10, type.measure = "deviance", alpha = 1, family = "binomial")

preds <- predict(lassofit, as.matrix(x), s = "lambda.min", type = "class")

paste0(" Overall error rate: ", 1 - sum(y == preds) / nrow(raw))


