options(stringsAsFactors = FALSE)
rm(list = ls())
library(tidyverse)


# load data ---------------------------------------------------------------

logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')
logon$date <- lubridate::mdy_hms(logon$date)
logon$time <- hms::as.hms(logon$date)
logon$day <- lubridate::as_date(logon$date)

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"
device$date <- lubridate::mdy_hms(device$date)
device$time <- hms::as.hms(device$date)
device$day <- lubridate::as_date(device$date)

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")
http$date <- lubridate::mdy_hms(http$date)
http$time <- hms::as.hms(http$date)
http$day <- lubridate::as_date(http$date)

# current employee from most recent employee data set
current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")
current <- current %>%
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

# all employee data sites combined into single dataframe
emp <- list.files(path = "~/data_analytics/DataSets1_9182017/Employees_info/",
                  pattern = "*.csv", full.names = TRUE)
emp <- do.call(rbind, lapply(emp, read_csv)) %>% 
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))
emp$Role <- stringr::str_replace_all(emp$Role, " ", "")


# device ------------------------------------------------------------------
# device_info.csv (~66k records)
# * Fields: id, date, user, pc, activity (connect/disconnect)
# * Some users use a portable zip drive
# * Some connect(s) may be missing disconnect(s), since machine may be turned 
#   off without a proper disconnect. 

head(device)
length(unique(device$pc))
# 228 unique machines in here

# check that connects/disconnects are equal, should be able to track this
# usb connections
con <- device %>%
  group_by(pc) %>% 
  filter(usb == "Connect") %>% 
  summarise(n = n()) %>% 
  arrange(pc, -n)
head(con, n = 10)
tail(con, n = 10)
summary(con$n)
hist(con$n)
# usb disconnections
dis <- device %>%
  group_by(pc) %>% 
  filter(usb == "Connect") %>% 
  summarise(n = n()) %>% 
  arrange(pc, -n)
head(dis, n = 10)
tail(dis, n = 10)
summary(dis$n)
hist(dis$n)

# compare connections and disconnections
head(con, n = 10)
head(dis, n = 10)

all.equal(con, dis)
# returns true, so that each machine has equal connects/disconnects
# question remaing is if they are consecutive connects/disconnects
# how to check this?
# filter by pc, date, user and just look maybe?
tmp <- device %>% 
  group_by(date) %>% 
  arrange(pc, user)
head(tmp, n = 20)
tail(tmp, n = 20)

# look at counts of pcs
tmp <- device %>%
  group_by(pc) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
# distribution of n
summary(tmp$n)
hist(tmp$n)


# employee ----------------------------------------------------------------

head(emp)
unique(emp$Role) # 15 unique roles
length(unique(emp$employee_name)) # 1000 unique employees

# get counts of employee roles
tmp <- unique(emp) %>% 
  group_by(Role) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

sum(tmp$n) # adds to 1000, so no people with changed roles

head(current)
tmp <- current %>%
  group_by(Role) %>% 
  summarise(n = n()) %>% 
  arrange(-n)
sum(tmp$n) # 935 current employees
