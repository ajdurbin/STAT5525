# supervisors
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels", "parallel")
lapply(._, library, character.only = TRUE)
cur <- read_csv("../raw/LDAP/2011-05.csv")
sup2 <- unique(cur$supervisor)
sup2 <- cur %>%
    filter(employee_name %in% sup2) %>% 
    select(employee_name, user_id, email)
sup1 <- unique(first$supervisor)
sup1 <- first %>%
    filter(employee_name %in% sup1) %>% 
    select(employee_name, user_id, email)
all.equal(sup1, sup2)
