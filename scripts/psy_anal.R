options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart")
lapply(._, library, character.only = TRUE)

psych <- read_csv("../raw/psychometric_info.csv")
cur <- read_csv("../raw/LDAP/2011-05.csv")
psych <- psych %>%
    mutate(attrition = ifelse(user_id %in% cur$user_id, 0, 1)) %>% 
    select(-(employee_name))


# logisitic regression ----------------------------------------------------

psych_glm <- psych %>%
    select(-(user_id))

trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = psych_glm, trControl = trc, 
                method = "glm", family = binomial())  

logfit$resample
1 - logfit$resample[, 1]
paste0("Overall error rate: ", 1 - logfit$results[, 2])

# about 15% error rate, so not very good


# kmeans clustering -------------------------------------------------------

psych_kmeans <- psych %>%
    select(-c(user_id, attrition))

best_km_fit <- list("1" <- NULL)
for(i in 2:50){
    fit <- kmeans(psych_kmeans, centers = i)
    for(j in 1:1000){
        tmp <- kmeans(psych_kmeans, centers = i)
        if(tmp$tot.withinss < fit$tot.withinss){
            fit <- tmp
        }
    }
    tmp <- as.character(i)
    best_km_fit[[tmp]] <- fit
}

# collect tot.withinss
sto <- rep(0, 50)
for(i in 2:length(best_km_fit)){
    tmp <- best_km_fit[[i]]
    sto[i] <- tmp$tot.withinss
}

ggplot() +
    geom_jitter(mapping = aes(x = 2:50, y = sto[2:length(sto)])) +
    ggtitle("Scree Plot For k-Means Clustering") +
    xlab("Number Of Clusters") +
    ylab("Total Within Clusters Sum Of Squares") +
    ylim(min(sto), max(sto))

# appears to be say 20 clusters here


# hierarchical clustering -------------------------------------------------

psych_hier <- psych %>%
    select(-c(user_id, attrition))

hcs <- hclust(dist(psych_hier), method = "single")
hcc <- hclust(dist(psych_hier), method = "complete")
hca <- hclust(dist(psych_hier), method = "average")
plot(hcs, main = "Single-Linkage Dendogram")
plot(hcc, main = "Complete-Linkage Dendogram")
plot(hca, main = "Average-Linkage Dendogram")

# not helpful at all


# random forest  ----------------------------------------------------------

psych_rf <- psych %>%
    select(-(user_id))

trc <- trainControl(method = "cv", number = 10)
forfit <- train(factor(attrition) ~ ., data = psych_rf, 
                trControl = trc, method = "rf")
forfit$resample
1 - forfit$resample[, 1]
mean(1 - forfit$resample[, 1])

# 16%, performs worse than logistic regression


# cart tree ---------------------------------------------------------------

psych_cart <- psych %>% 
    select(-(user_id))

trc <- trainControl(method = "cv", number = 10)
forfit <- train(factor(attrition) ~ ., data = psych_rf, 
                trControl = trc, method = "rpart")
forfit$resample
1 - forfit$resample[, 1]
mean(1 - forfit$resample[, 1])

# 17%, still worse than logistic regression

