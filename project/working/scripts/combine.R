options(stringsAsFactors = FALSE)

rm(list = ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(hms)


# load and parse data -----------------------------------------------------


logon <- read_csv('~/data_analytics/DataSets1_9182017/logon_info.csv')
logon$date <- lubridate::mdy_hms(logon$date)
logon$time <- hms::as.hms(logon$date)
logon$day <- lubridate::as_date(logon$date)
logon <- logon %>% 
  arrange(date)

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"
device$date <- lubridate::mdy_hms(device$date)
device$time <- hms::as.hms(device$date)
device$day <- lubridate::as_date(device$date)
device <- device %>% 
  arrange(date)

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")
http$date <- lubridate::mdy_hms(http$date)
http$time <- hms::as.hms(http$date)
http$day <- lubridate::as_date(http$date)
http <- http %>% 
  arrange(date)

# current employee from most recent employee data set
current <- read_csv("~/data_analytics/DataSets1_9182017/Employees_info/2011-May.csv")
current <- current %>%
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))

# all employee data sites combined into single dataframe
emp <- list.files(path = "~/data_analytics/DataSets1_9182017/Employees_info/",
                  pattern = "*.csv", full.names = TRUE)
emp <- do.call(rbind, lapply(emp, read_csv)) %>% 
  mutate(user = stringr::str_c(stringr::str_sub(Domain, 1, 4), user_id, sep = "/"))
emp$Role <- stringr::str_replace_all(emp$Role, " ", "")

# balance between social media, streaming, shopping, job searching
bad_sites <- c("facebook", "myspace", "twitter", "instagram", "pinterest",
               "tumblr", "netflix", "hulu", "youtube", "monster", "indeed",
               "linkedin", "glassdoor", "careerbuilder", "amazon",
               "craigslist", "flickr", "reddit", "ebay", "espn",
               "eharmony", "match", "tinder")


# function declarations ---------------------------------------------------


# return user summary
combo_filter <- function(usr, log = logon, dev = device, web = http,
                         cur = current, total = emp, sites = bad_sites){
  
  usr_log <- log %>% 
    filter(user == usr) %>% 
    select(user, date, pc, activity, day, time)
  usr_dev <- dev %>%
    filter(user == usr) %>% 
    select(user, date, pc, usb, day, time)
  # note some people do not use usb
  usr_web <- web %>% 
    filter(user == usr) %>% 
    select(user, date, pc, website, day, time)
  # now adjust the na in activitiy, website, usb
  combo <- plyr::rbind.fill(usr_log, usr_dev, usr_web) %>% 
    mutate(activity = ifelse(is.na(activity), "using", activity)) %>% 
    mutate(website = ifelse(is.na(website), "none", website)) %>% 
    mutate(usb = ifelse(is.na(usb), "none", usb)) %>% 
    mutate(usb_mis_dis = "", logoff_mis = "") %>% 
    # convert date/time to string to preserve type throughout
    mutate(date = as.character(date), time = as.character(time),
           day = as.character(day)) %>% 
    as.data.frame()
  combo <- combo %>%
    arrange(date)
  
  # now fill in missing usb disconnects
  combo <- insert_usb(combo = combo)
  combo <- combo %>%
    arrange(date)
  
  # # now fill in missing logoffs/screen locks
  combo <- insert_logoff(combo = combo)
  combo <- combo %>%
    arrange(date)
  
  # premature return for fixing consecutive connects/logons
  # return(combo)
  
  name_info <- total %>% 
    filter(user == usr) %>% 
    select(employee_name) %>% 
    # select(employee_name, Email) %>% 
    unique()
  combo$name <- name_info$employee_name
  
  # 1 means attrition
  attrition <- ifelse(usr %in% cur$user, 0, 1)
  combo$attrition <- attrition
  
  # count bad sites visits
  cnt <- 0
  for(site in sites){
    tmp <- sum(stringr::str_detect(combo$website, site))
    cnt <- cnt + tmp
  }
  
  # standardize based on total web traffic
  # cnt <- cnt / nrow(combo[combo$website != "none"])
  combo$bad_sites <- cnt
  
  # find user roles
  roles <- total %>%
    filter(user == usr) %>% 
    select(Role) %>% 
    unique()
  
  # collapse into char string separated by spaces
  roles <- paste0(as.character(roles$Role), collapse = " ")
  combo$role <- roles
    
  # get miscellaneous pc information from total
  unq_pc <- pc_prs(combo = combo)
  
  # pckg for return
  # pckg <- list("user" = usr, "user_data" = combo, "attrition" = attrition,
  #              "bad site visits" = cnt, "roles" = roles)
  # pckg <- data.frame(name = name_info$employee_name, email = name_info$Email,
  #                    user = usr, attrition = attrition, bad_sites = cnt, 
  #                    roles = roles, primary_pc = unq_pc$prm_pc, 
  #                    pc_count = unq_pc$unq_pc, num_on = unq_pc$on, 
  #                    num_off = unq_pc$off, prm_num_on = unq_pc$prm_on, 
  #                    prm_num_off = unq_pc$prm_off, usb_num = unq_pc$usb_con)
               
  combo$primary_pc = unq_pc$prm_pc
  combo$pc_count = unq_pc$unq_pc
  
  combo <- combo %>%
    arrange(date)
  
  return(combo)
  
}

# return user pc information - used in combo_filter
pc_prs <- function(combo){
  
  # total machines theyve logged on to
  unq_pc <- combo %>%
    select(pc) %>% 
    unique() %>% 
    nrow()
  
  # total logons
  on <- combo %>%
    select(activity) %>% 
    filter(activity == "Logon") %>% 
    nrow()
  
  # total logoffs
  off <- combo %>%
    select(activity) %>% 
    filter(activity == "Logoff") %>% 
    nrow()
  
  # get number of usb connects
  usb_con <- combo %>%
    select(usb) %>% 
    filter(usb == "Connect") %>% 
    nrow()
  
  # get primary machine
  prm_pc <- combo %>% 
    group_by(pc) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    filter(N == max(N)) %>% 
    select(pc) %>% 
    unique() %>% 
    as.character()
  
  # primary pc logons
  prm_pc_on <- combo %>%
    filter(pc == prm_pc) %>% 
    filter(activity == "Logon") %>% 
    nrow()
  
  # primary pc logoffs
  prm_pc_off <- combo %>%
    filter(pc == prm_pc) %>% 
    filter(activity == "Logoff") %>% 
    nrow()
  
  pckg <- data.frame(unq_pc = unq_pc, on = on, off = off, usb_con = usb_con,
                     prm_pc = prm_pc, prm_on = prm_pc_on, prm_off = prm_pc_off)
  
  return(pckg)
  
}

# fills in missing usb disconnects - used in combo_filter
# need not take into account usr with multiple pcs usb usage
# since there are 228 unique users and 228 unique pcs
insert_usb <- function(combo){
  
  # split up
  # usb connect/disconnects
  no_match <- combo %>%
    filter(usb != "none")
  # no usb connects/disconnects
  yes_match <- combo %>%
    filter(usb == "none")
  
  # if user has no usb traffic at all
  if(nrow(no_match) == 0){
    return(combo)
  }
  
  # tmp storage
  new_usb <- tibble(date = "", 
                    user = "",
                    pc = "", 
                    activity = "",
                    day = "",
                    time = "",
                    usb = "",
                    website = "",
                    usb_mis_dis = "",
                    logoff_mis = "")
  
  # loop through and append matching rows to new_usb
  for(i in 1:(nrow(no_match)-1)){
    # get next row for checking
    cur <- no_match[i, ]
    nxt <- no_match[i+1, ]
    # check if they same, then create row and add to df
    if(cur$usb == nxt$usb){
      # flag original entry
      no_match[i, ]$usb_mis_dis <- TRUE
      new_row <- cur
      # flag new entry
      new_row$usb <- 'Disconnect'
      new_row$usb_mis_dis <- TRUE
      new_usb <- rbind(new_usb, new_row)
    }
  }
  
  # check last entry to make sure not lonely connect
  if(tail(no_match$usb, n = 1) == "Connect"){
    
    new_row <- tail(no_match, n = 1)
    # flag new entry
    new_row$usb <- 'Disconnect'
    new_row$usb_mis_dis <- TRUE
    new_usb <- rbind(new_usb, new_row)
    # flag original entry
    no_match[nrow(no_match), ]$usb_mis_dis <- TRUE
    
  }
  
  # remove dummy row if any were added to new_usb and combine with no_match
  if(nrow(new_usb) != 1){
    new_usb <- new_usb[2:nrow(new_usb), ]
    new_match <- rbind(no_match, new_usb)
  } else{
    new_match <- no_match
  }
  
  # recombine new_match with yes_match
  if(nrow(yes_match) != 0){
    pckg <- rbind(yes_match, new_match)
  } else{
    pckg <- new_match
  }
  
  pckg <- pckg %>% 
    arrange(date)
  
  return(pckg)
  
}

# fills in missing logoffs - used in combo_filter
insert_logoff <- function(combo){
  
  # unique pc
  all_pc <- unique(combo$pc)
  
  # total storage 
  sto <- tibble(date = "", 
                user = "",
                pc = "", 
                activity = "",
                day = "",
                time = "",
                usb = "",
                website = "",
                usb_mis_dis = "",
                logoff_mis = "")
  
  
  for(the_pc in all_pc){
    
    # tmp storage
    tmp <- tibble(date = "", 
                  user = "",
                  pc = "", 
                  activity = "",
                  day = "",
                  time = "",
                  usb = "",
                  website = "",
                  usb_mis_dis = "",
                  logoff_mis = "")
    
    # get entries for that pc
    # split into logon/logoff and using
    no_match  <- combo %>%
      filter(pc == the_pc, activity != "using")
    yes_match <- combo %>%
      filter(pc == the_pc, activity == "using")
    
    # check if there's only one observation and continue to next pc
    if(nrow(no_match) == 1){
      
      no_match[1, ]$logoff_mis <- TRUE
      new_row <- no_match[1, ]
      # flag new entry
      new_row$activity <- 'Logoff'
      new_row$logoff_mis <- TRUE
      tmp <- rbind(tmp, new_row)
      
      tmp <- tmp[2:nrow(tmp), ]
      new_match <- rbind(no_match, tmp)
      
      # recombine new_match with yes_match
      if(nrow(yes_match) != 0){
        pckg <- rbind(yes_match, new_match)
      } else{
        pckg <- new_match
      }
      
      sto <- rbind(sto, pckg) 
      next
      
    }
    
    # loop through the rows and find consecutive logon
    # create new row and add to sto
    for(i in 1:(nrow(no_match)-1)){
      # get next row for checking
      cur <- no_match[i, ]
      nxt <- no_match[i+1, ]
      # check if they same, then create row and add to df
      if(cur$activity == nxt$activity){
        # flag original entry
        no_match[i, ]$logoff_mis <- TRUE
        new_row <- cur
        # flag new entry
        new_row$activity <- 'Logoff'
        new_row$logoff_mis <- TRUE
        tmp <- rbind(tmp, new_row)
      }
    }
    
    # double check that last row is not a logon
    # if so then create new row and add to sto
    if(tail(no_match$activity, n = 1) == "Logon"){
      
      new_row <- tail(no_match, n = 1)
      # flag new entry
      new_row$activity <- 'Logoff'
      new_row$logoff_dis <- TRUE
      tmp <- rbind(tmp, new_row)
      # flag original entry
      no_match[nrow(no_match), ]$logoff_mis <- TRUE
      
    }
    
    # remove dummy row if any were added to new_usb and combine with no_match
    if(nrow(tmp) != 1){
      tmp <- tmp[2:nrow(tmp), ]
      new_match <- rbind(no_match, tmp)
    } else{
      new_match <- no_match
    }
    
    # recombine new_match with yes_match
    if(nrow(yes_match) != 0){
      pckg <- rbind(yes_match, new_match)
    } else{
      pckg <- new_match
    }
    
    sto <- rbind(sto, pckg)
    
  }
  
  # remove dummy row
  sto <- sto[2:nrow(sto), ]
  sto <- sto %>%
    arrange(date)
  
  return(sto)
  
}

# run ---------------------------------------------------------------------


# run for single user
# usr <- "ACME/KLS0717"
# system.time(
#   test <- combo_filter(usr = usr)
# )

# slow for loop to get all users
# for(usr in unique(logon$user)){
#   
#   tmp <- combo_filter(usr = usr)
#   
#   if(exists("big_data")){
#     big_data <- rbind(big_data, tmp)
#   } else{
#     big_data <- tmp
#   }
#   
# }
# 

# try not in parallel for debuuging
# big_data <- sapply(unique(logon$user), function(g) combo_filter(usr = g))
# big_data <- t(big_data)
# big_data <- as.data.frame(big_data, row.names = FALSE)
# big_data <- parsapply(big_data, unlist)
# big_data <- as.data.frame(big_data)

# create parallel cluster for later
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
clusterExport(cl = cl, ls())

# run for all users in parallel
system.time(
  big_data <- parSapply(cl = cl, unique(logon$user),
                                  function(g) combo_filter(usr = g))
)

# formatting
big_data <- t(big_data)
big_data <- as.data.frame(big_data, row.names = FALSE)
big_data <- parSapply(cl, big_data, unlist)
big_data <- as.data.frame(big_data)
big_data <- big_data %>% 
  arrange(date)

stopCluster(cl)

write_csv(big_data, path = paste0(getwd(), "/big_data.csv"))
