options(stringsAsFactors = FALSE)

rm(list = ls())
library(tidyverse)
library(parallel)

# load data sets for parsing
logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")

# current employee from most recent employee data set
current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")
current <- current %>%
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

# all employee data sites combined into single dataframe
emp <- list.files(path = "~/data_analytics/DataSets1_9182017/Employees_info/",
                  pattern = "*.csv", full.names = TRUE)
emp <- do.call(rbind, lapply(emp, read_csv)) %>% 
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

# balance between social media, streaming, shopping, job searching
bad_sites <- c("facebook", "myspace", "twitter", "instagram", "pinterest",
               "tumblr", "netflix", "hulu", "youtube", "monster", "indeed",
               "linkedin", "glassdoor", "careerbuilder", "amazon",
               "craigslist", "flickr", "reddit", "ebay", "espn")

# if cannot run on entire data set
# usr <- unique(logon$user)[sample(1:length(logon), 1)]

# function to return list object of 
# user
# data frame of (date, pc, logon/logoff, usb, web)
# attrition or not
# number of bad site visits
# roles in company
combo_filter <- function(usr, log = logon, dev = device, web = http,
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
    mutate(usb = ifelse(is.na(usb), "none", usb)) %>% 
    as.data.frame()
  
  # 1 means attrition
  attrition <- ifelse(usr %in% cur$user, 0, 1)
  
  # count bad sites visits
  cnt <- 0
  for(site in sites){
    tmp <- sum(stringr::str_detect(combo$website, site))
    cnt <- cnt + tmp
  }
  
  # standardize based on total web traffic
  cnt <- cnt / nrow(combo[combo$wesite != "none"])
  
  # find user roles
  roles <- total %>%
    filter(user == usr) %>% 
    select(Role) %>% 
    unique()
  
  # get vector of different roles
  roles <- as.character(roles$Role)
  
  # pckg for return
  pckg <- list("user" = usr, "user_data" = combo, "attrition" = attrition,
               "bad site visits" = cnt, "roles" = roles)
  
  return(pckg)
  
}

# run for single user
# system.time(
#   test <- combo_filter(user = usr)
# )

# create parallel cluster for later
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
clusterExport(cl = cl, ls())

# run for all users in parallel
system.time(
  big_data <- parSapply(cl = cl, unique(logon$user), function(g) combo_filter(usr = g))
)

# first col is users
# second col is user_data
# third col is attrition flag
# fourth col is bad site count
# fifth col is roles in company
# to better visualize the structure
# user data will say List, 5
# meaning that there are 5 columns in this List
# similar for roles
big_data <- t(big_data)

stopCluster(cl)
