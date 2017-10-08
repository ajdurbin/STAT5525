library(stringr)
library(dplyr)
logon <- readr::read_csv("/home/alex/data_analytics/DataSets1_9182017/logon_info.csv")
logon <- logon[,2:5]

# Take out time from date-time
Hours <- format(as.POSIXct(strptime(logon$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
logon$hour <- Hours

# Take out date from date-time
Day <- format(as.POSIXct(strptime(logon$date,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
logon$day <- Day

# Change useris to consistent format
# with employee log
logon$user_id <- str_sub(logon$user,-7,-1)

# Use only logoffs from the weekdays
# filter out weekends
logon_week <- logon[!logon$day %in% c("Saturday","Sunday"),]
# dates and times where they logoff
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
users_sum <- logon_week %>%
  group_by(user_id,date) %>%
  dplyr::filter(mins == max(mins)) %>%
  group_by(user_id) %>%
  summarise(totalmins = sum(mins), totaldays = n()) %>%
  data.frame()

# Add rate stat
users_sum$minsperday <- users_sum$totalmins / users_sum$totaldays

# Employee Log
employee_log <- read.csv('/home/alex/data_analytics/DataSets1_9182017/Employees_info/2009-12.csv')
employee_log <- plyr::join(employee_log, users_sum, by = "user_id")
