# keylogger, virus, malware filtering
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "stringr", "parallel")
lapply(._, library, character.only = TRUE)

current <- read_csv("../raw/LDAP/2011-05.csv")
original <- read_csv("../raw/LDAP/2009-12.csv")
file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(extension = str_extract(filename, "[^.]*$")) %>% 
    mutate(keylogger = str_detect(content, "keylog")) %>% 
    mutate(virus = str_detect(content, "virus")) %>% 
    mutate(password = str_detect(content, "password")) %>% 
    mutate(hack = str_detect(content, "hack")) %>% 
    mutate(steal = str_detect(content, "steal")) %>% 
    mutate(illegal = str_detect(content, "illegal")) %>% 
    mutate(before_5 = 
               ifelse(hour < 5, TRUE, FALSE)) %>% 
    mutate(after_11 = 
               ifelse(hour > 22, TRUE, FALSE)) %>% 
    mutate(after_hour_dowload = before_5 | after_11) %>% 
    mutate(malware = str_detect(content, "malware"))
device <- read_csv("../raw/device_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(before_5 = 
               ifelse(activity == "Connect" & hour < 5, TRUE, FALSE)) %>% 
    mutate(after_11 = 
               ifelse(activity == "Connect" & hour > 22, TRUE, FALSE)) %>% 
    mutate(after_hour_connect = before_5 | after_11)
# email <- read_csv("../data/email_small.csv") %>% 
#     mutate(date = lubridate::mdy_hms(date)) %>% 
#     mutate(day = lubridate::as_date(date)) %>% 
#     mutate(time = hms::as.hms(date)) %>% 
#     mutate(hour = lubridate::hour(time)) %>% 
#     mutate(keylogger = str_detect(content, "keylog")) %>% 
#     mutate(virus = str_detect(content, "virus")) %>% 
#     mutate(password = str_detect(content, "password")) %>% 
#     mutate(hack = str_detect(content, "hack")) %>% 
#     mutate(interview = str_detect(content, "interview")) %>% 
#     mutate(fired = str_detect(content, "fired")) %>% 
#     mutate(steal = str_detect(content, "steal")) %>% 
#     mutate(malware = str_detect(content, "malware"))
# http <- read_csv("../data/http_small.csv") %>% 
#     mutate(date = lubridate::mdy_hms(date)) %>% 
#     mutate(day = lubridate::as_date(date)) %>% 
#     mutate(time = hms::as.hms(date)) %>% 
#     mutate(hour = lubridate::hour(time)) %>% 
#     mutate(keylogger = str_detect(content, "keylog")) %>% 
#     mutate(virus = str_detect(content, "virus")) %>% 
#     mutate(password = str_detect(content, "password")) %>% 
#     mutate(hack = str_detect(content, "hack")) %>% 
#     mutate(steal = str_detect(content, "steal")) %>% 
#     mutate(malware = str_detect(content, "malware"))

mal <- file %>%
    filter(malware == TRUE)
virus <- file %>%
    filter(virus == TRUE)
key <- file %>%
    filter(keylogger == TRUE)
pass <- file %>%
    filter(password == TRUE)
illegal <- file %>%
    filter(illegal == TRUE)
# has two true things, rest are contained in the mal and key

# keyloggers <- unique(key$user)

# process usb information
usb_function <- function(usr, device, file) {
    
    usr_orig <- original %>% 
        filter(user_id == usr)
    usr_device <- device %>%
        filter(user == usr)
    usr_file <- file %>% 
        filter(user == usr)
    ahd <- usr_file %>%
        filter(after_hour_dowload == TRUE)
    row <- data.frame(user = usr,
                      role = usr_orig$role,
                      attrition = ifelse(usr %in% current$user_id, FALSE, TRUE),
                      after_hour_connect = 
                          nrow(usr_device[usr_device$after_hour_connect == TRUE, ]),
                      total_usb_connects = 
                          nrow(usr_device[usr_device$activity == "Connect", ]),
                      after_hour_downloads =
                          nrow(ahd),
                      total_usb_downloads =
                          nrow(usr_file))
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
result <- lapply(usb_users, 
                 function(g) usb_function(usr = g, device = device, file = file))
result <- list_to_df(result)
ah <- result %>% 
    filter(after_hour_downloads > 0) %>% 
    arrange(role)

