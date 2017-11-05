# flag quick logon/logoffs

library(tidyverse)
big_data <- read_csv("big_data.csv", na = "")

# usb users
tmp <- big_data %>% 
  filter(usb != "none")
usb_users <- unique(tmp$user)

for(usr in usb_users){
  # get all usb user traffic
  tmp <- big_data %>% 
    filter(user == usr) %>% 
    arrange(date, pc)
}
