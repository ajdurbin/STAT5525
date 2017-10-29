# average usb connection time
# again do not need to worry about different computers because of the fact
# that no user uses multiple machines for usb

library(tidyverse)
big_data <- read_csv("big_data.csv", na = "")
big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

usb_data <- big_data %>% 
  filter(usb != "none")

usr <- "ACME/RES0962"
usr_usb_data <- usb_data %>% 
  filter(user == usr)

# get user usb data
# usr_usb_data <- usr_usb_data[1:4, ]
# split into connects and disconnects
con <- usr_usb_data %>% 
  filter(usb == "Connect")
dis <- usr_usb_data %>% 
  filter(usb == "Disconnect")
# difference between connects and disconnects
tst <- difftime(dis$time, con$time, units = "mins")
# convert to numeric for averaging
tst <- as.numeric(tst, units = "mins")
# compute average
mean(tst)

# just make function to compute these differences inside and replace the
# lonely connects/disconnects with the average

avg_usb_time <- function(combo){
  
  # filter out no usb activity and lonely connections
  combo <- combo %>% 
    filter(usb != "none", usb_mis_dis != TRUE)
  
  if(nrow(combo) == 0){
    return("")
  }
  
  # split into connect/disconnect
  con <- combo %>% 
    filter(usb == "Connect")
  dis <- combo %>% 
    filter(usb == "Disconnect")
  # time differences and conversion
  dif <- difftime(dis$time, con$time, units = "hours")
  dif <- as.numeric(dif)
  # return average
  return(mean(dif))
  
  
}

usr_usb_data$avg_usb_hrs <- avg_usb_time(usr_usb_data)

# do similar thing for logon/logoff time


# generalize script to return data frame of results -----------------------

rm(list = ls())
library(tidyverse)
library(lubridate)
library(hms)

big_data <- read_csv("big_data.csv")

big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

# usb users
tmp <- big_data %>% 
  filter(usb != "none")
usb_users <- unique(tmp$user)


for(usr in usb_users){
  
  # filter out no usb activity and lonely connections
  combo <- big_data %>% 
    filter(usb != "none", usb_mis_dis != TRUE, user == usr) %>% 
    arrange(date)
  
  if(nrow(combo) == 0){
    next
  }
  
  # split into connect/disconnect
  con <- combo %>% 
    filter(usb == "Connect")
  dis <- combo %>% 
    filter(usb == "Disconnect")
  # # time differences and conversion
  # dif <- difftime(dis$time, con$time, units = "hours")
  # dif <- as.numeric(dif)
  
  # using interval, duration, period from lubridate
  time_interval <- con$date %--% dis$date
  dif <- as.duration(time_interval)
  dif <- as.numeric(dif)
  dif <- dif/60
  # return objects
  avg <- mean(dif)
  qck1 <- sum(dif < 1)
  qck5 <- sum(dif < 5)
  mx <- max(dif)
  mn <- min(dif)
  md <- median(dif)
  role <- unique(combo$role)
  num <- length(dif)
  pc <- unique(combo$pc)
  primary_pc <- unique(combo$primary_pc)
  # row
  row <- data.frame(
    user = usr,
    primary_pc = primary_pc,
    usb_pc = pc,
    pc_count = unique(combo$pc_count),
    role = role,
    attrition = unique(combo$attrition),
    usb_connects = num,
    quick_connects_lt_1_min = qck1,
    quick_connects_lt_5_min = qck5,
    average_usb_min = avg,
    median_usb_min = md,
    max_usb_min = mx,
    min_usb_min = mn
  )
  
  if(!exists("usb_distribution")){
    usb_distribution <- row
  } else{
    usb_distribution <- rbind(usb_distribution, row)
  }
  
}

write_csv(usb_distribution, "usb_distribution.csv")


# something going on where some have negative average usb connection time
# need to check calculations
# choose bad user and then manually do for loop above
# usr <- "ACME/KAD0059"
# need to do it using interval, duration, period object in lubridate

# think it's fixed, now try
# sum(usb_distribution$average_usb_time>0)
# checks out and works