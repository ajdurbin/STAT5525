rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels", "parallel")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv")
device <- read_csv("../raw/device_info.csv")
psych <- read_csv("../raw/psychometric_info.csv")
cur <- read_csv("../raw/LDAP/2011-05.csv")
logon <- read_csv("../raw/logon_info.csv")

## test local before remote run
# email <- read_csv("../raw/email_info.csv")
# f <- function(x, pos){
#     return(x[, c(2, 3, 4, 5)])
# }
# http <- read_csv_chunked("../raw/http_info,csv", DataFrameCallback$new(f), 
#                          chunk_size = 100000)

users <- unique(logon$user)

# combo <- function(usr, cur = cur, file = file, device = device, psych = psych,
#                   logon = logon, email = email, http = http) {
combo <- function(usr, cur = cur, file = file, device = device, psych = psych,
                  logon = logon) {
    
    # file downloads
        
    usr_file <- file %>%
        filter(user == usr) 
    if (nrow(usr_file) != 0) {
        usr_file <- usr_file %>% 
        mutate(date = lubridate::mdy_hms(date), time = hms::as.hms(date), 
               day = lubridate::as_date(date), hour = lubridate::hour(date)) %>% 
            mutate(extensions = stringr::str_extract(filename, "[^.]*$")) 
    }
        
    
    
    # usb connects
    usr_device <- device %>%
        filter(user == usr)
    if (nrow(usr_device) != 0){
        usr_device <- usr_device %>% 
        mutate(date = lubridate::mdy_hms(date), time = hms::as.hms(date), 
               day = lubridate::as_date(date), hour = lubridate::hour(date))
    }
        
    
    # logon
    usr_logon <- logon %>%
        filter(user == usr) %>% 
        mutate(date = lubridate::mdy_hms(date), time = hms::as.hms(date), 
               day = lubridate::as_date(date), hour = lubridate::hour(date))
    
    # psychometrics
    usr_psych <- psych %>%
        filter(user_id == usr)
    
    # current employees
    usr_cur <- cur %>%
        filter(user_id == usr)
    
    # # email
    # usr_email <- email %>%
    #     filter(user == usr) %>% 
    #     mutate(date = lubridate::mdy_hms(date), time = hms::as.hms(date), 
    #            day = lubridate::as_date(date), hour = lubridate::hour(date))
    # 
    # # web
    # usr_http <- http %>%
    #     filter(user == usr) %>% 
    #     mutate(date = lubridate::mdy_hms(date), time = hms::as.hms(date), 
    #            day = lubridate::as_date(date), hour = lubridate::hour(date))
    
    row <- data.frame(user = usr)
    return(row)
    
}

## testing
# pckg <- lapply(users, function(g) combo(usr = g, cur = cur, file = file,
#                                         device = device, psych = psych,
#                                         logon = logon))
# test <- combo(usr = "LAP0338", cur = cur, file = file,
#               device = device, psych = psych,
#               logon = logon) 

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores, type = "PSOCK")
clusterExport(cl = cl, ls())
pckg <- lapply(users, function(g) combo(usr = g, cur = cur, file = file,
                                        device = device, psych = psych,
                                        logon = logon))