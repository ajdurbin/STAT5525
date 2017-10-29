# average user logon time
# do need to worry about different pc

library(tidyverse)
big_data <- read_csv("big_data.csv", na = "")
big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

# logon_data <- big_data %>% 
#   filter(activity != "using")

usr <- "ACME/RES0962"
usr_data <- big_data %>% 
  filter(user == usr)

# usr_pcs <- unique(usr_logon$pc)
# sto <- rep(0, length(usr_pcs))
# 
# for(i in 1:length(usr_pcs)){
#   
#   usr_pc <- usr_pcs[i]
#   pc_data <- usr_logon %>% 
#     filter(pc == usr_pc)
#   con <- pc_data %>% 
#     filter(activity == "Logon")
#   dis <- pc_data %>% 
#     filter(activity == "Logoff")
#   dif <- difftime(dis$time, con$time, units = "hours")
#   sto[i] <- mean(dif)
#   
# }
# 
# sto <- mean(sto)

# now wrapped in a function, should not need to check these
# temporary df since each user has used at least one pc
avg_logon <- function(combo){
  
  
  combo <- combo %>% 
    filter(activity != "using", logoff_mis != TRUE)
  usr_pcs <- unique(combo$pc)
  sto <- rep(0, length(usr_pcs))
  
  for(i in 1:length(usr_pcs)){
    
    usr_pc <- usr_pcs[i]
    pc_data <- combo %>% 
      filter(pc == usr_pc)
    con <- combo %>% 
      filter(activity == "Logon")
    dis <- combo %>% 
      filter(activity == "Logoff")
    dif <- difftime(dis$time, con$time, units = "hours")
    sto[i] <- mean(dif)
    
  }
  
  return(mean(sto))
  
}

# now test
usr_data$avg_on_hrs <- avg_logon(usr_data)


# generalize script to return dataframe of results ------------------------


rm(list = ls())
library(tidyverse)
library(lubridate)
library(hms)

big_data <- read_csv("big_data.csv", na = "")
big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

users <- unique(big_data$user)


for(usr in users){
  
  
  combo <- big_data %>% 
    filter(activity != "using", logoff_mis != TRUE, user == usr) %>% 
    arrange(date)
  usr_pcs <- unique(combo$pc)
  
  connects <- 0
  
  for(i in 1:length(usr_pcs)){
    
    usr_pc <- usr_pcs[i]
    pc_data <- combo %>% 
      filter(pc == usr_pc)
    con <- combo %>% 
      filter(activity == "Logon")
    dis <- combo %>% 
      filter(activity == "Logoff")
    
    if(nrow(con) == 0){
      next
    }
    
    connects <- connects + nrow(con)
    
    # using interval, duration, period from lubridate
    time_interval <- con$date %--% dis$date
    dif <- as.duration(time_interval)
    dif <- as.numeric(dif)
    dif <- dif/60
    
    # collect all times
    if(!exists("all_times")){
      all_times <- dif
    } else{
      all_times <- c(all_times, dif)
    }
    
  }
  
  row <- data.frame(
    user = usr,
    primary_pc = unique(combo$primary_pc),
    pc_count = length(usr_pcs),
    role = unique(combo$role),
    attrition = unique(combo$attrition),
    total_connects = connects,
    quick_connects_lt_1_min = sum(all_times < 1),
    quick_connects_lt_5_min = sum(all_times < 5),
    avg_on_min = mean(all_times),
    median_on_min = median(all_times),
    max_on_min = max(all_times),
    min_on_min = min(all_times)
  )
  
  if(!exists("logon_distribution")){
    logon_distribution <- row
  } else{
    logon_distribution <- rbind(logon_distribution, row)
  }
  
}

write_csv(logon_distribution, "logon_distribution.csv")
