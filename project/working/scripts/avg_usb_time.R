# average usb connection time
# again do not need to worry about different computers because of the fact
# that no user uses multiple machines for usb

# library(tidyverse)
# big_data <- read_csv("big_data.csv", na = "")
# big_data <- big_data %>% 
#   mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
#   mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))
# 
# usb_data <- big_data %>% 
#   filter(usb != "none")
# 
# usr <- "ACME/RES0962"
# usr_usb_data <- usb_data %>% 
#   filter(user == usr)
# 
# # get user usb data
# # usr_usb_data <- usr_usb_data[1:4, ]
# # split into connects and disconnects
# con <- usr_usb_data %>% 
#   filter(usb == "Connect")
# dis <- usr_usb_data %>% 
#   filter(usb == "Disconnect")
# # difference between connects and disconnects
# tst <- difftime(dis$time, con$time, units = "mins")
# # convert to numeric for averaging
# tst <- as.numeric(tst, units = "mins")
# # compute average
# mean(tst)
# 
# # just make function to compute these differences inside and replace the
# # lonely connects/disconnects with the average
# 
# avg_usb_time <- function(combo){
#   
#   # filter out no usb activity and lonely connections
#   combo <- combo %>% 
#     filter(usb != "none", usb_mis_dis != TRUE)
#   
#   if(nrow(combo) == 0){
#     return("")
#   }
#   
#   # split into connect/disconnect
#   con <- combo %>% 
#     filter(usb == "Connect")
#   dis <- combo %>% 
#     filter(usb == "Disconnect")
#   # time differences and conversion
#   dif <- difftime(dis$time, con$time, units = "hours")
#   dif <- as.numeric(dif)
#   # return average
#   return(mean(dif))
#   
#   
# }
# 
# usr_usb_data$avg_usb_hrs <- avg_usb_time(usr_usb_data)

# do similar thing for logon/logoff time


# generalize script to return data frame of results -----------------------

# rm(list = ls())
# library(tidyverse)
# library(lubridate)
# library(hms)
# 
# big_data <- read_csv("big_data.csv")
# 
# big_data <- big_data %>% 
#   mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
#   mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

# usb users
# tmp <- big_data %>% 
#   filter(usb != "none")
# usb_users <- unique(tmp$user)
# usr <- sample(usb_users, 1)
# 
# 
# for(usr in usb_users){
#   
#   # filter out no usb activity and lonely connections
#   combo <- big_data %>% 
#     filter(usb != "none", usb_mis_dis != TRUE, user == usr) %>% 
#     arrange(date)
#   
#   if(nrow(combo) == 0){
#     next
#   }
#   
#   # split into connect/disconnect
#   con <- combo %>% 
#     filter(usb == "Connect")
#   dis <- combo %>% 
#     filter(usb == "Disconnect")
#   # # time differences and conversion
#   # dif <- difftime(dis$time, con$time, units = "hours")
#   # dif <- as.numeric(dif)
#   
#   # count how many after hours connections there are, say between 9pm,5am
#   con$hour <- as.numeric(hour(con$time))
#   cnt <- sum(con$hour > 21 | con$hour < 5)
#   
#   # using interval, duration, period from lubridate
#   time_interval <- con$date %--% dis$date
#   dif <- as.duration(time_interval)
#   dif <- as.numeric(dif)
#   dif <- dif/60
#   # return objects
#   avg <- mean(dif)
#   qck1 <- sum(dif < 1)
#   qck5 <- sum(dif < 5)
#   mx <- max(dif)
#   mn <- min(dif)
#   md <- median(dif)
#   role <- unique(combo$role)
#   num <- length(dif)
#   pc <- unique(combo$pc)
#   primary_pc <- unique(combo$primary_pc)
#   # row
#   row <- data.frame(
#     user = usr,
#     primary_pc = primary_pc,
#     usb_pc = pc,
#     pc_count = unique(combo$pc_count),
#     role = role,
#     attrition = unique(combo$attrition),
#     after_hour_connects = cnt,
#     total_usb_connects = num,
#     quick_connects_lt_1_min = qck1,
#     quick_connects_lt_5_min = qck5,
#     average_usb_min = avg,
#     median_usb_min = md,
#     max_usb_min = mx,
#     min_usb_min = mn
#   )
#   
#   if(!exists("usb_distribution")){
#     usb_distribution <- row
#   } else{
#     usb_distribution <- rbind(usb_distribution, row)
#   }
#   
# }
# 
# write_csv(usb_distribution, "usb_distribution.csv")


# something going on where some have negative average usb connection time
# need to check calculations
# choose bad user and then manually do for loop above
# usr <- "ACME/KAD0059"
# need to do it using interval, duration, period object in lubridate

# think it's fixed, now try
# sum(usb_distribution$average_usb_time>0)
# checks out and works

# ggplot(data = usb_distribution) +
#   geom_jitter(mapping = aes(y = total_usb_connects, x = after_hour_connects,
#                             color = factor(attrition)))
# 
# 
# ggplot(data = usb_distribution) +
#   geom_jitter(mapping = aes(x = quick_connects_lt_1_min, y = after_hour_connects,
#                             color = factor(attrition)))
# 
# 
# ggplot(data = usb_distribution) +
#   geom_jitter(mapping = aes(x = pc_count, y = after_hour_connects,
#                             color = factor(attrition))) +
#   xlim(0,125)

# need metric for connecting to other pc while usb in
# get users whole traffic, take segments between usb connects/disconnects
# and sum the other pc they logging into
# that may be deciding factor



# get counts of pc connections in between user connections ----------------

# this user is guilty of this
# usr <- "ACME/AJC0399"
# usr_dat <- big_data %>% 
#   filter(user == usr, usb_mis_dis != TRUE) %>% 
#   arrange(date)
# 
# con <- usr_dat %>% 
#   filter(usb == "Connect")
# dis <- usr_dat %>% 
#   filter(usb == "Disconnect")
# primary_pc <- unique(usr_dat$primary_pc)
# bad_connects <- 0
# for(i in 1:nrow(con)){
#   connect <- con$date[i]
#   disconnect <- dis$date[i]
#   tmp <- usr_dat %>% 
#     filter(between(date, connect, disconnect))
#   # check if nothing in between and move on
#   if( (nrow(tmp)-2) == 0){
#     next
#   }
#   
#   # filter out primary pc stuff now
#   tmp <- tmp %>% 
#     filter(pc != primary_pc)
#   # check for rows again
#   if(nrow(tmp) == 0){
#     next
#   }
#   
#   # filter to logons now
#   tmp <- tmp %>% 
#     filter(activity == "Logon")
#   if(nrow(tmp) == 0){
#     next
#   }
#   
#   bad_connects <- bad_connects + nrow(tmp)
#   
#   
# }


# added bad connections to usb_dist ---------------------------------------


# rm(list = ls())
# options(stringsAsFactors = FALSE)
# library(tidyverse)
# library(lubridate)
# library(hms)
# library(stringr)

big_data <- read_csv("../data/big_data.csv")

big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

# usb users
tmp <- big_data %>% 
  filter(usb != "none")
usb_users <- unique(tmp$user)
usr <- sample(usb_users, 1)


for(usr in usb_users){
  
  # filter out no usb activity and lonely connections
  combo <- big_data %>% 
    filter(usb != "none", usb_mis_dis != TRUE, user == usr) %>% 
    arrange(date)
  pc <- unique(combo$pc)
  
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
  # # time differences and conversion
  # dif <- difftime(dis$time, con$time, units = "hours")
  # dif <- as.numeric(dif)
  
  
  # BEGIN bad connections
  bad_connects <- 0
  bad_pc <- ""
  for(i in 1:nrow(con)){
    connect <- con$date[i]
    disconnect <- dis$date[i]
    tmp <- combo %>% 
      filter(between(date, connect, disconnect))
    # check if nothing in between and move on
    if( (nrow(tmp)-2) == 0){
      next
    }
    
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
    if(nrow(tmp) == 0){
      next
    }
    
    pcs <- paste(tmp$pc, collapse = " ")
    bad_connects <- bad_connects + nrow(tmp)
    bad_pc <- str_c(bad_pc, pcs, sep = " ")
    
  }
  
  # END bad connections
  
  # count how many after hours connections there are, say between 9pm,5am
  con$hour <- as.numeric(hour(con$time))
  cnt <- sum(con$hour > 21 | con$hour < 5)
  
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
  # pc <- unique(combo$pc)
  # row
  row <- data.frame(
    user = usr,
    primary_pc = primary_pc,
    usb_pc = pc,
    pc_count = unique(combo$pc_count),
    role = role,
    attrition = unique(combo$attrition),
    bad_connects = bad_connects,
    bad_connects_pc = bad_pc,
    after_hour_connects = cnt,
    total_usb_connects = num,
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

write_csv(usb_distribution, "../data/usb_distribution.csv")

rm(tmp, usb_users, usr, combo, pc, primary_pc, con, dis, bad_connects, 
   bad_pc, i, connect, disconnect, pcs, time_interval, dif, avg,
   qck1, qck5, mx, mn, md, role, num, row, cnt)

# ggplot(data = usb_distribution) +
#   geom_jitter(mapping = aes(
#     y = bad_connects, x = after_hour_connects, color = factor(attrition)
#   ))
# 
# # do some logisitic lasso
# library(glmnet)
# library(caret)
# 
# # remove unneccessary columns
# raw <- usb_distribution[, -(1:3)]
# raw <- raw[, -2]
# 
# trc <- trainControl(method = "cv", number = 10)
# logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
#                 family = binomial())  
# paste0("Overall error rate: ", 1 - logfit$results[, 2])



# examine attrition users connections -------------------------------------


# usb_distribution <- read_csv("usb_distribution.csv")
# fired <- usb_distribution %>% 
#   filter(attrition == 1)
