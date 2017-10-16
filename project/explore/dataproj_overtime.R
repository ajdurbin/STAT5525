library(stringr)
library(dplyr)
setwd("C:/Users/dougl/Documents/FALL 17/DATA ANALYTICS/DATAPROJ")
logon <- read.csv("logon_info.csv")
logon <- logon[,2:5]

# Take out time from date-time
Hours <- format(as.POSIXct(strptime(logon$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
logon$hour <- Hours

# Take out date from date-time
Day <- format(as.POSIXct(strptime(logon$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
logon$date <- Day

# Change useris to consistent format
# with employee log
logon$user_id <- str_sub(logon$user,-7,-1)

# Use only logoffs from the weekdays
logon_week <- logon[!logon$day %in% c("Saturday","Sunday"),]
logon_week <- logon[logon$activity == "Logoff",]

# If logout was after 5:30
# take difference in times (in seconds NOT minutes)
logon_week$mins <- as.integer(ifelse(as.POSIXct(logon_week$hour, format = '%H:%M:%S')<as.POSIXct('17:30:00', format = '%H:%M:%S'),
                     0,
                     as.POSIXct(logon_week$hour, format = '%H:%M:%S')-as.POSIXct('17:30:00', format = '%H:%M:%S')))

# Change to mins
logon_week$mins <- logon_week$mins / 60

# Take only latest logoff
# sum time and days
users_logoff <- logon_week %>%
  group_by(user_id,date1) %>%
  dplyr::filter(mins == max(mins)) %>%
  group_by(user_id) %>%
  summarise(totalmins = sum(mins), totaldays = n()) %>%
  data.frame()

# Add rate stat
users_logoff$minsperday <- users_logoff$totalmins / users_logoff$totaldays
employee_log <- join(employee_log, users_logoff, by = "user_id")

# ___ Logons ____

# Use only logons from the weekdays
logon_week <- logon[!logon$day %in% c("Saturday","Sunday"),]
logon_week <- logon[logon$activity == "Logon",]

# If logout was before 8:30
# take difference in times (in seconds NOT minutes)
logon_week$minsbefore <- as.integer(ifelse(as.POSIXct(logon_week$hour, format = '%H:%M:%S')>as.POSIXct('9:30:00', format = '%H:%M:%S'),
                                     0,
                                     as.POSIXct('9:30:00', format = '%H:%M:%S')-as.POSIXct(logon_week$hour, format = '%H:%M:%S')))

# Change to mins
logon_week$minsbefore <- logon_week$mins / 60

# Take only earliest login
# sum time and days
users_logon <- logon_week %>%
  group_by(user_id,date) %>%
  dplyr::filter(minsbefore == max(minsbefore)) %>%
  group_by(user_id) %>%
  summarise(totalmins = sum(minsbefore), totaldays = n()) %>%
  data.frame()

users_logon$minsperday <- users_logon$totalmins / users_logon$totaldays

# Employee Log
employee_log <- read.csv('C:/Users/dougl/Documents/FALL 17/DATA ANALYTICS/DATAPROJ/Employees_info/2009-12.csv')
employee_log <- join(employee_log, users_logon, by = "user_id")
