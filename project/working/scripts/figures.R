# get some frequency figures
# these are same plots used in gifs
# just highlight section and keep running until find one you like
rm(list = ls())
options(stringsAsFactors = FALSE)
._ <- c("dplyr", "plyr", "readr", "ggplot2", "stringr", "lubridate", "hms",
        "caret", "glmnet", "rpart", "randomForest", "parallel",
        "gridExtra")
lapply(._, library, character.only = TRUE)

big_data <- read_csv("../data/big_data.csv")

# web ----
# load data and grab user
usr <- sample(unique(big_data$user), 1)

# get user data
usr_data <- big_data %>% 
    filter(user == usr)
# make indicator variables for each of the objectives
usr_data <- usr_data %>% 
    mutate(indicator_on = ifelse(activity == "Logon", 1, 0)) %>% 
    mutate(indicator_off = ifelse(activity == "Logoff", 1, 0)) %>% 
    mutate(indicator_con = ifelse(usb == "Connect", 1, 0)) %>% 
    mutate(indicator_dis = ifelse(usb == "Disconnect", 1, 0)) %>% 
    mutate(indicator_web = ifelse(website != "none", 1, 0)) %>% 
    mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), FALSE, TRUE)) %>% 
    mutate(logoff_mis = ifelse(is.na(logoff_mis), FALSE, TRUE))

# filter to indicator traffic for plots
on <- usr_data %>% 
    filter(indicator_on == 1)
off <- usr_data %>% 
    filter(indicator_off == 1)
con <- usr_data %>% 
    filter(indicator_con == 1)
dis <- usr_data %>% 
    filter(indicator_dis == 1)
web <- usr_data %>% 
    filter(indicator_web == 1)

# web
ggplot() +
    geom_point(mapping = aes(x = day, y = time), data = web, alpha = 0.5, color = "blue") +
    scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0(usr, " Web Traffic"))
    

# logon ----
# load data and grab user
usr <- sample(unique(big_data$user), 1)

# get user data
usr_data <- big_data %>% 
    filter(user == usr)
# make indicator variables for each of the objectives
usr_data <- usr_data %>% 
    mutate(indicator_on = ifelse(activity == "Logon", 1, 0)) %>% 
    mutate(indicator_off = ifelse(activity == "Logoff", 1, 0)) %>% 
    mutate(indicator_con = ifelse(usb == "Connect", 1, 0)) %>% 
    mutate(indicator_dis = ifelse(usb == "Disconnect", 1, 0)) %>% 
    mutate(indicator_web = ifelse(website != "none", 1, 0)) %>% 
    mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), FALSE, TRUE)) %>% 
    mutate(logoff_mis = ifelse(is.na(logoff_mis), FALSE, TRUE))

# filter to indicator traffic for plots
on <- usr_data %>% 
    filter(indicator_on == 1)
off <- usr_data %>% 
    filter(indicator_off == 1)
con <- usr_data %>% 
    filter(indicator_con == 1)
dis <- usr_data %>% 
    filter(indicator_dis == 1)
web <- usr_data %>% 
    filter(indicator_web == 1)

# logon
ggplot() +
      geom_point(mapping = aes(x = day, y = time), data = on, alpha = 0.5, color = "blue") +
      geom_point(mapping = aes(x = day, y = time), data = off, alpha = 0.5, color = "red") +
      scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste0(usr, " Logon/Logoffs"))


# usb ----
# load data and grab user
usr <- sample(unique(big_data$user), 1)

# get user data
usr_data <- big_data %>% 
    filter(user == usr)
# make indicator variables for each of the objectives
usr_data <- usr_data %>% 
    mutate(indicator_on = ifelse(activity == "Logon", 1, 0)) %>% 
    mutate(indicator_off = ifelse(activity == "Logoff", 1, 0)) %>% 
    mutate(indicator_con = ifelse(usb == "Connect", 1, 0)) %>% 
    mutate(indicator_dis = ifelse(usb == "Disconnect", 1, 0)) %>% 
    mutate(indicator_web = ifelse(website != "none", 1, 0)) %>% 
    mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), FALSE, TRUE)) %>% 
    mutate(logoff_mis = ifelse(is.na(logoff_mis), FALSE, TRUE))

# filter to indicator traffic for plots
on <- usr_data %>% 
    filter(indicator_on == 1)
off <- usr_data %>% 
    filter(indicator_off == 1)
con <- usr_data %>% 
    filter(indicator_con == 1)
dis <- usr_data %>% 
    filter(indicator_dis == 1)
web <- usr_data %>% 
    filter(indicator_web == 1)

# usb
ggplot() +
    geom_point(mapping = aes(x = day, y = time), data = con, alpha = 0.5, color = "blue") +
    geom_point(mapping = aes(x = day, y = time), data = dis, alpha = 0.5, color = "red") +
    scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0(usr, " USB Connects/Disconnects"))
