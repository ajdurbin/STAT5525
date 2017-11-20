rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv")
usb <- read_csv("../raw/device_info.csv")
# usb_users <- unique(file$user) 
# usr <- sample(usb_users, 1)
# usr_data <- file %>%
#     filter(user == usr)
# extract the file extensions
# usr_data$extensions <- stringr::str_extract(usr_data$filename, "[^.]*$")
file$extensions <- stringr::str_extract(file$filename, "[^.]*$")
barplot(table(file$extensions))
# few .exe files
exe_users <- file %>%
    filter(extensions == "exe")
length(unique(exe_users$pc))
# 345 usb exe machines, but are they the it people?
