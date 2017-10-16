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

head(device)

# look at counts of pcs and entries
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
