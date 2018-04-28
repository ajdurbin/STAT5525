# usb after hours check script
options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "hms", "parallel", "stringr", "lubridate")
lapply(._, library, character.only = TRUE)

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv') %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(user = str_sub(user, start = 6)) %>% 
    mutate(before_5 = 
               ifelse(activity == "Connect" & hour < 5, TRUE, FALSE)) %>% 
    mutate(after_11 = 
               ifelse(activity == "Connect" & hour > 22, TRUE, FALSE)) %>% 
    mutate(after_hour_connect = before_5 | after_11)
current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")

# just output df row with attrition, after hour connects, total usb connects
usb_function <- function(usr) {
    
    tmp <- device %>%
        filter(user == usr)
    row <- data.frame(user = usr,
                      attrition = ifelse(usr %in% current$user_id, FALSE, TRUE),
                      after_hour_connect = 
                          nrow(tmp[tmp$after_hour_connect == TRUE, ]),
                      total_usb_connects = 
                          nrow(tmp[tmp$activity == "Connect", ]))
    return(row)
    
}

# unpack df
list_to_df <- function(my_list) {
    
    pckg <- my_list[[1]]
    for (i in 2:length(my_list)) {
        tmp <- my_list[[i]]
        pckg <- rbind(pckg, tmp)
    }
    return(pckg)
}

usb_users <- unique(device$user)
result <- lapply(usb_users, function(g) usb_function(usr = g))
result <- list_to_df(result)
# pick out the fired people
fired <- result %>% 
    filter(attrition == TRUE)
# see those with little usb traffic who connect at night
