# combine userid, datetime, http traffic, device, attrition, logon/logoff
# then filter web traffic

rm(list = ls())
library(tidyverse)

# load data sets for parsing
logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")

current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")
current <- current %>%
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

emp <- list.files(path = "~/data_analytics/DataSets1_9182017/Employees_info/",
                  pattern = "*.csv", full.names = TRUE)
emp <- do.call(rbind, lapply(emp, read_csv)) %>% 
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

bad_sites <- c("facebook", "myspace", "twitter", "instagram", "pinterest",
               "tumblr", "netflix", "hulu", "youtube", "monster", "indeed",
               "linkedin", "glassdoor", "careerbuilder")

usr <- unique(logon$user)[sample(1:length(logon), 1)]

combo_filter <- function(user, log = logon, dev = device, web = http,
                         cur = current, total = emp, sites = bad_sites){
  
  usr_log <- log %>% 
    filter(user == usr) %>% 
    select(date, pc, activity)
  usr_dev <- dev %>%
    filter(user == usr) %>% 
    select(date, pc, usb)
  # note some people do not use usb
  usr_web <- web %>% 
    filter(user == usr) %>% 
    select(date, pc, website)
  # now adjust the na in activitiy, website, usb
  combo <- plyr::rbind.fill(usr_log, usr_dev, usr_web) %>% 
    mutate(activity = ifelse(is.na(activity), "using", activity)) %>% 
    mutate(website = ifelse(is.na(website), "none", website)) %>% 
    mutate(usb = ifelse(is.na(usb), "none", usb))
  
  # 1 means attrition
  attrition <- ifelse(usr %in% cur$user, 0, 1)
  
  # count bad sites visits
  cnt <- 0
  for(site in sites){
    tmp <- sum(stringr::str_detect(combo$website, site))
    cnt <- cnt + tmp
  }
  
  # find user roles
  roles <- total %>%
    filter(user == usr) %>% 
    select(Role) %>% 
    unique()
  
  # pckg for return
  pckg <- list("user" = usr, "user_data" = combo, "attrition" = attrition,
               "bad site visits" = cnt, "roles" = roles)
  
  return(pckg)
  
}

test <- combo_filter(user = usr)
