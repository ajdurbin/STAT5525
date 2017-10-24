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
