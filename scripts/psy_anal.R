options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret")
lapply(._, library, character.only = TRUE)

psych <- read_csv("../raw/psychometric_info.csv")
cur <- read_csv("../raw/LDAP/2011-05.csv")
psych <- psych %>%
    mutate(attrition = ifelse(user_id %in% cur$user_id, 0, 1)) %>% 
    select(-(employee_name))


# logisitic regression ----------------------------------------------------

tmp <- psych %>%
    select(-(user_id))
trc <- trainControl(method = "cv", number = 10)
logfit <- train(factor(attrition) ~ ., data = tmp, trControl = trc, 
                method = "glm", family = binomial())  
logfit$resample
1 - logfit$resample[, 1]
paste0("Overall error rate: ", 1 - logfit$results[, 2])

# about 15% error rate, so not very good