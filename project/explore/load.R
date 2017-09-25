rm(list = ls())
library(tidyverse)


# logon information -------------------------------------------------------


logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')


# device information ------------------------------------------------------


device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(deivce)
head(device)
# contains user, pc, and connect/disconnect, and date of access


# http information --------------------------------------------------------


http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
head(http)
# contains log id, date, user, pc, and website


# employee information ----------------------------------------------------


may_11 <- read_csv('~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv')
colnames(may_11)
# gives user_id and role at company, can see what employees still work there
