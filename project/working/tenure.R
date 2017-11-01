# tenure
# loop through employee data sets and find employee start and end dates
rm(list = ls())
library(tidyverse)
library(lubridate)
library(hms)
big_data <- read_csv("big_data.csv")
unique_users <- unique(big_data$user)
# usr <- sample(unique_users, 1)
# 
# usr_dat <- big_data %>% 
#   filter(user == usr) %>% 
#   arrange(date)
# tenure <- head(usr_dat$day, 1) %--% tail(usr_dat$day, 1)
# tenure <- as.duration(tenure)
# tenure <- as.numeric(tenure, "days")

for(usr in unique_users){
  usr_dat <- big_data %>% 
    filter(user == usr) %>% 
    arrange(date)
  tenure <- head(usr_dat$day, 1) %--% tail(usr_dat$day, 1)
  tenure <- as.duration(tenure)
  tenure <- as.numeric(tenure, "days")
  
  row <- data.frame(user = usr,
                    attrition = unique(usr_dat$attrition),
                    role = unique(usr_dat$role),
                    tenure_days = tenure,
                    start_date = head(usr_dat$day, 1),
                    end_date = tail(usr_dat$day, 1))
  
  if(!exists("tenure_distribution")){
    tenure_distribution <- row
  } else{
    tenure_distribution <- rbind(tenure_distribution, row)
  }
}

write_csv(tenure_distribution, "tenure_distribution.csv")