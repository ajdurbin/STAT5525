library(caret)
library(rpart)

raw <- read.table("spam.data.txt")
raw[, 1 : 57] <- scale(raw[, 1 : 57])
raw[, 58] <- as.factor(raw[, 58])

carfit <- rpart(V58 ~ ., data = raw, method = "class",
                control = list(cp = 0.0001, minsplit = 2, minbucket = 1))
printcp(carfit)
plotcp(carfit)
# see that we have 286 nodes in this by this specification
# i think the cross validation error is xerror or rel error
# research gate says to prune with the cp that gives minimal xval rel error 
# from the plotcp

carpred <- predict(carfit, newdata = raw[, 1: 57], type="class")
table(raw[, 58], carpred)
# error is then
5 / nrow(raw)
