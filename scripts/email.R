options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart", "tm", "wordcloud", "animation")
lapply(._, library, character.only = TRUE)
email <- read_csv("../raw/email_info.csv")
usr <- sample(unique(email$user), 1)
usr_data <- email %>% 
    filter(user == usr)
