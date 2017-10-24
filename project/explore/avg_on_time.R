# average user logon time
# do need to worry about different pc

library(tidyverse)
big_data <- read_csv("big_data.csv", na = "")
big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

logon_data <- big_data %>% 
  filter(activity != "using")

usr <- "ACME/RES0962"
usr_logon <- logon_data %>% 
  filter(user == usr)

pcs <- unique(usr_logon$pc)