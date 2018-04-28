# avg_usb_time.R revised

# rm(list = ls())
# options(stringsAsFactors = FALSE)
# library(tidyverse)
# library(lubridate)
# library(hms)
# library(stringr)

# big_data <- read_csv("../data/big_data.csv")

big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

# usb users
tmp <- big_data %>% 
  filter(usb != "none", usb_mis_dis != TRUE)
usb_users <- unique(tmp$user)
# usr <- sample(usb_users, 1)
# usr <- "ACME/RES0962"


for(usr in usb_users){
  
  ## CHUNK COMMENT
  ## may be redudnant since already filtering out usb people
  # filter out no usb activity and lonely connections
  combo <- big_data %>% 
    filter(usb != "none", usb_mis_dis != TRUE, user == usr) %>% 
    arrange(date)
  
  # to check if they have any usb traffic at all or move to next usr
  if(nrow(combo) == 0){
    next
  }
  ## END CHUNK COMMENT
  
  # filter to all traffic
  combo <- big_data %>% 
    filter(usb_mis_dis != TRUE, user == usr) %>% 
    arrange(date)
  
  primary_pc <- unique(combo$primary_pc)
  
  if(nrow(combo) == 0){
    next
  }
  
  # split into connect/disconnect
  con <- combo %>% 
    filter(usb == "Connect")
  dis <- combo %>% 
    filter(usb == "Disconnect")
  
  # BEGIN bad connections
  bad_connects <- 0
  bad_pc <- ""
  for(i in 1:nrow(con)){
    connect <- (con$date[i])
    disconnect <- (dis$date[i])
    tmp <- combo %>% 
      filter(between(date, connect, disconnect))
    
    ## CHUNK COMMENT
    ## redundant because they should hae at least two rows
    ## since im grabbing the usb connect and disconnect
    # if they have nothing in these dates at all
    if(nrow(tmp) == 0){
      next
    }
    ## END CHUNK COMMENT
    
    
    ## CHUNK COMMENT
    ## now checking if nothing in between these times 
    # check if nothing in between and move on
    if( (nrow(tmp)-2) == 0){
      next
    }
    # for those dates with only a single connect
    if( (nrow(tmp)-1) == 0){
      next
    }
    ## END CHUNK COMMENT
    
    # filter out primary pc stuff now
    tmp <- tmp %>% 
      filter(pc != primary_pc)
    
    # check for rows again
    if(nrow(tmp) == 0){
      next
    }
    
    # filter to logons now
    tmp <- tmp %>% 
      filter(activity == "Logon")
    
    # check for rows again
    if(nrow(tmp) == 0){
      next
    }
    
    bad_connects <- bad_connects + nrow(tmp)
    pcs <- paste0(tmp$pc, collapse = " ")
    bad_pc <- str_c(bad_pc, pcs, sep = " ")
    
  }
  
  # END bad connections
  
  # for counting after hour connections
  con$hour <- as.numeric(hour(con$time))
  
  # using interval, duration, period from lubridate
  time_interval <- con$date %--% dis$date
  dif <- as.duration(time_interval)
  dif <- as.numeric(dif)
  dif <- dif/60
  role <- unique(combo$role)
  
  # row
  row <- data.frame(
    user = usr,
    primary_pc = primary_pc,
    pc_count = unique(combo$pc_count),
    role = role,
    attrition = unique(combo$attrition),
    bad_connects = bad_connects,
    bad_connects_pcs = bad_pc,
    after_hour_connects = sum(con$hour > 21 | con$hour < 5),
    total_usb_connects = length(dif),
    quick_connects_lt_1_min = sum(dif < 1),
    quick_connects_lt_5_min = sum(dif < 5),
    average_usb_min = mean(dif),
    median_usb_min = median(dif),
    max_usb_min = max(dif),
    min_usb_min = min(dif)
  )
  
  if(!exists("usb_distribution")){
    usb_distribution <- row
  } else{
    usb_distribution <- rbind(usb_distribution, row)
  }
  
}

# write_csv(usb_distribution, "../data/usb_distribution.csv")

rm(tmp, usb_users, usr, combo, pc, primary_pc, con, dis, bad_connects,
   bad_pc, i, connect, disconnect, pcs, time_interval, dif, avg,
   qck1, qck5, mx, mn, md, role, num, row, cnt)
