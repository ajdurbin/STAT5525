options(stringsAsFactors = FALSE)

rm(list = ls())
library(tidyverse)
library(parallel)


# load data and parse -----------------------------------------------------


# load data sets for parsing
logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")

# test on single pc
pc <- unique(logon$pc)[sample(1:length(logon), 1)]

# function declarations ---------------------------------------------------

pc_extract <- function(cmp, log = logon, dev = device, web = http){
  
  # logon info for this pc
  cmp_log <- log %>% 
    filter(pc == cmp) %>% 
    select(user, activity, date)
  
  # web info for this pc
  cmp_web <- web %>% 
    filter(pc == cmp) %>% 
    select(user, website, date)
  
  # device info for this pc
  cmp_dev <- dev %>%
    filter(pc == cmp) %>% 
    select(user, usb, date)
  
  # all info on this pc
  combo <- plyr::rbind.fill(cmp_log, cmp_dev, cmp_web) %>% 
    mutate(activity = ifelse(is.na(activity), "using", activity)) %>% 
    mutate(website = ifelse(is.na(website), "none", website)) %>% 
    mutate(usb = ifelse(is.na(usb), "none", usb)) %>% 
    as.data.frame()
  
  # number of unique users
  num_usr <- cmp_log %>% 
    select(user) %>% 
    unique() %>% 
    nrow()
  
  # number of logons
  num_on <- cmp_log %>% 
    filter(activity == "Logon") %>% 
    nrow()
  
  # number of logoffs
  num_off <- cmp_log %>% 
    filter(activity == "Logoff") %>% 
    nrow()
  
  # number usb connections
  num_usb <- cmp_dev %>% 
    filter(usb == "Connect") %>% 
    nrow()
  
  # web traffic
  num_web <- cmp_web %>% 
    filter(website != "none") %>% 
    nrow()
    
  
  pckg <- data.frame(pc = cmp, total_users = num_usr, total_logon = num_on,
                     total_logoff = num_off, total_usb = num_usb,
                     total_web = num_web)
  return(pckg)
  
}


# run ---------------------------------------------------------------------


# run for single pc
pc_data <- pc_extract(cmp = pc)

# currently having return issues, it thinks each pc is own variable name
# create parallel cluster for later
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
clusterExport(cl = cl, ls())

# run for all users in parallel
system.time(
  pc_data <- parSapply(cl = cl, unique(logon$pc), 
                                  function(g) pc_extract(cmp = g))
)

# formatting
pc_data <- t(pc_data)
pc_data <- as.data.frame(pc_data, row.names = FALSE)
for(i in 1:ncol(pc_data)){
  pc_data[, i] <- as.vector(unlist(pc_data[, i]))
}

stopCluster(cl)