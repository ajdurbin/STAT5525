options(stringsAsFactors = FALSE)
rm(list = ls())
library(tidyverse)


# load data ---------------------------------------------------------------

logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')
logon$date <- lubridate::mdy_hms(logon$date)

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"
device$date <- lubridate::mdy_hms(logon$date)

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")
http$date <- lubridate::mdy_hms(logon$date)

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
