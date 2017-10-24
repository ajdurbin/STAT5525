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
