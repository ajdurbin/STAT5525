library(caret)
library(rpart)

raw <- read.table("spam.data.txt")
raw[, 1 : 57] <- scale(raw[, 1 : 57])
raw[, 58] <- as.factor(raw[, 58])

carfit <- rpart(V58 ~ ., data = raw, method = "class",
                control = list(cp = .Machine$double.eps, minsplit = 2, minbucket = 1))
printcp(carfit)
plotcp(carfit)
# see that we have 286 nodes in this by this specification
# i think the cross validation error is xerror or rel error
# research gate says to prune with the cp that gives minimal xval rel error 
# from the plotcp

carpred <- predict(carfit, newdata = raw[, 1: 57], type="class")
conf <- table(raw[, 58], carpred)
# error is then
cler <- (conf[2]  + conf[3]) / (conf[1] + conf[4])



# loop to do cross validation ---------------------------------------------


# now wrap up in a function
# error storage
preders <- rep(0, 10)
# split into training and testing
train_index <- createDataPartition(y = raw[, 58], times = 10, p = 0.8, list = FALSE)

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


