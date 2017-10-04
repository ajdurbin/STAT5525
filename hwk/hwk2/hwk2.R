# alexander durbin
# stat5525 homework 2

library(caret)

# load spam data set
raw <- read.table("spam.data.txt")
# standardize columns
raw[, 1 : 57] <- scale(raw[, 1 : 57])
# factor the response
raw[, 58] <- as.factor(raw[, 58])

# Part 4 - perform logistic regression and report 10 fold cross validation error
trc <- trainControl(method = "cv", number = 10)
model <- train(V58 ~ ., data = raw, trControl = trc, method = "glmnet",
               family = "binomial")
# try using glmnet
library(glmnet)
x <- raw[, 1 : 57]
y <- raw[, 58]
lassofit <- cv.glmnet(x = as.matrix(x), y = y, nfolds = 10, type.measure = "deviance",
                      alpha = 1, family = "binomial")
preds <- predict(lassofit, as.matrix(x), s = "lambda.min", type = "class")
success_rate <- sum(y == preds) / nrow(raw)
error_rate <- 1 - success_rate
