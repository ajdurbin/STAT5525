rm(list = ls())
library(tidyverse)


# logon information -------------------------------------------------------


logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')
# maybe consider employees who go long periods logged off have meetings
# and look at their position to see if its one that would require being away
# from computer frequently
# look at logon logoff information near lunchtime, see if i can classify
# this and find people who eat together based on similar logon logoff times,
# say like 1 minute apart



# device information ------------------------------------------------------


device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(deivce)
head(device)
# contains user, pc, and connect/disconnect, and date of access


# http information --------------------------------------------------------


http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
head(http)
# contains log id, date, user, pc, and website url
# web traffic is just the domains, so no need to strip out
# good idea to make a vector of known bad sites and then add new column
# bad sites could be grouped by topic later, but for now
# just have social networking, news, entertainment
# will quickly find a list of most common ones
# PREDICT GENDER BASED ON TRAFFIC BUT DONT HAVE GENDER INFORMATION YET
# WOULD NEED TO SOMEHOW GET GENDER INTO DATASET TOO


# employee information ----------------------------------------------------


may_11 <- read_csv('~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv')
colnames(may_11)
# gives user_id and role at company, can see what employees still work there

dec_09 <- read_csv('~/data_analytics/DataSets1_9182017/Employees_info/2009-12.csv')
colnames(dec_09)
# colnames appear consistent at least, name, id, email, role, domain
# so we should be able to see attrition, now we want to predict it
# we can do so using web traffic, working from home, and usb activity?

format(object.size(logon), units = "auto")
