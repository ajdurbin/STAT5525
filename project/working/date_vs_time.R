# date versus time plots of indicator variables for logon, website, usb

library(tidyverse)
library(lubridate)
library(hms)
library(scales)

big_data <- read_csv("big_data.csv")
# choose random user
usr <- sample(unique(big_data$user), 1)
# get user data
usr_data <- big_data %>% 
  filter(user == usr)
# make indicator variables for each of the objectives
usr_data <- usr_data %>% 
  mutate(indicator_logon = ifelse(activity == "Logon", 1, 0)) %>% 
  mutate(indicator_usb = ifelse(usb == "Connect", 1, 0)) %>% 
  mutate(indicator_web = ifelse(website != "none", 1, 0))
# check classes of objects and convert types
class(usr_data$date)
class(usr_data$day)
class(usr_data$time)
# try making plot of just times when there is activity
ggplot(data = usr_data) +
  geom_point(mapping = aes(x = day, y = time), alpha = 0.1) +
  scale_x_date(date_breaks = "1 months")
# filter by what i want, like logon, usb, web ect
# and just use those times for this
low <- hms(seconds = 0, minutes = 0, hours = 12)
high <- hms(seconds = 0, minutes = 59, hours = 23)

on <- usr_data %>% 
  filter(indicator_logon == 1)
ggplot(data = on) +
  geom_point(mapping = aes(x = day, y = time), alpha = 0.5)
