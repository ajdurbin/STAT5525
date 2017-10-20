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

device <- read_csv('~/data_analytics/DataSets1_9182017/device_info.csv')
colnames(device)[5] <- "usb"
device$date <- lubridate::mdy_hms(device$date)
device$time <- hms::as.hms(device$date)
device$day <- lubridate::as_date(device$date)

http <- read_csv('~/data_analytics/DataSets1_9182017/http_info.csv', col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")
http$date <- lubridate::mdy_hms(http$date)
http$time <- hms::as.hms(http$date)
http$day <- lubridate::as_date(http$date)

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
    as.data.frame()
  
  # now fill in missing usb disconnects
  # combo <- insert_usb(combo = combo)
  
  # now fill in missing logoffs/screen locks
  # combo <- insert_logoff(combo = combo)
  
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
  cnt <- cnt / nrow(combo[combo$wesite != "none"])
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
insert_usb <- function(combo){
  
  # split up
  # usb connect/disconnects
  no_match <- combo %>%
    filter(usb != "none")
  # no usb connects/disconnects
  yes_match <- combo %>%
    filter(usb == "none")
  
  # tmp storage
  new_usb <- tibble(date = as_datetime(now()), 
                    user = "",
                    pc = "", 
                    activity = "",
                    day = as.Date(date),
                    time = as.hms(date),
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
      new_row <- cur
      new_row$usb <- 'Disconnect'
      new_row$usb_mis_dis <- TRUE
      new_usb <- rbind(new_usb, new_row)
    }
  }
  
  # remove dummy row
  new_usb <- new_usb[2:nrow(new_usb), ]
  # add new matching rows
  new_match <- rbind(no_match, new_usb)
  # recombine total
  pckg <- rbind(yes_match, new_match)
  return(pckg)
  
}

# run ---------------------------------------------------------------------


# run for single user
# usr <- "ACME/KLS0717"
# system.time(
#   test <- combo_filter(usr = usr)
# )

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
for(i in 1:ncol(big_data)){
  big_data[, i] <- as.vector(unlist(big_data[, i]))
}

stopCluster(cl)
