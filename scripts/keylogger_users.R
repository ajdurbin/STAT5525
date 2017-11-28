# keylogging users
# goal is to look at their traffic and find them somehow spreading this
# to other machines
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
logon <- read_csv("../raw/logon_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time))
http <- read_csv("../data/http_small.csv") %>%
    mutate(date = lubridate::mdy_hms(date)) %>%
    mutate(day = lubridate::as_date(date)) %>%
    mutate(time = hms::as.hms(date)) %>%
    mutate(hour = lubridate::hour(time))
email <- read_csv("../data/email_small.csv") %>%
    mutate(date = lubridate::mdy_hms(date)) %>%
    mutate(day = lubridate::as_date(date)) %>%
    mutate(time = hms::as.hms(date)) %>%
    mutate(hour = lubridate::hour(time))

key <- file %>%
    filter(keylogger == TRUE)
bad_users <- unique(key$user)

# user traffic
usr <- "JLM0364"
# look at emails sent to this user since they go mia with email, web 
# traffic before this happens
unique(usr_email$from)
work_email <- "Jacqueline.Latifah.Miles@dtaa.com"
personal_email <- "Jacqueline_L_Miles@sbcglobal.net"
received_emails <- email %>%
    mutate(work = str_detect(to, work_email)) %>% 
    mutate(personal = str_detect(to, personal_email)) %>% 
    filter(work == TRUE | personal == TRUE)
first <- "jacqueline"
last <- "miles"
included_emails <- email %>%
    mutate(first = str_detect(content, first)) %>% 
    mutate(last = str_detect(content, last)) %>% 
    filter(first == TRUE)
when <- key %>% 
    filter(user == usr)
usr_file <- file %>%
    filter(user == usr)
usr_device <- device %>%
    filter(user == usr)
usr_web <- http %>% 
    filter(user == usr)
usr_email <- email %>% 
    filter(user == usr)
# they have no web traffic for like 3 months basically before this
# only has 2 total usb connections
# they downloaded file and then offloaded it on someone elses machine
pc_after_malware <- "PC-8486"
logon_after <- logon %>% 
    filter(day >= when$day) %>% 
    filter(pc == pc_after_malware)
usb_after <- usr_device %>% 
    filter(day >= when$day)
# who uses this pc
