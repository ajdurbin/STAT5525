# date versus time plots of indicator variables for logon, website, usb
rm(list = ls())

library(tidyverse)
library(lubridate)
library(hms)
library(animation)


# test for random user ----------------------------------------------------


# big_data <- read_csv("big_data.csv")
# # choose random user
# usr <- sample(unique(big_data$user), 1)
# usr_name <- usr
# stringr::str_sub(usr_name, 1, 5) <- ""
# 
# # get user data
# usr_data <- big_data %>% 
#   filter(user == usr)
# # make indicator variables for each of the objectives
# usr_data <- usr_data %>% 
#   mutate(indicator_on = ifelse(activity == "Logon", 1, 0)) %>% 
#   mutate(indicator_off = ifelse(activity == "Logoff", 1, 0)) %>% 
#   mutate(indicator_con = ifelse(usb == "Connect", 1, 0)) %>% 
#   mutate(indicator_dis = ifelse(usb == "Disconnect", 1, 0)) %>% 
#   mutate(indicator_web = ifelse(website != "none", 1, 0)) %>% 
#   mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), FALSE, TRUE)) %>% 
#   mutate(logoff_mis = ifelse(is.na(logoff_mis), FALSE, TRUE))
# 
# # # check classes of objects and convert types
# # class(usr_data$date)
# # class(usr_data$day)
# # class(usr_data$time)
# # # try making plot of just times when there is activity
# # ggplot(data = usr_data) +
# #   geom_point(mapping = aes(x = day, y = time), alpha = 0.1) +
# #   scale_x_date(date_breaks = "1 months")
# # # filter by what i want, like logon, usb, web ect
# # # and just use those times for this
# # low <- hms(seconds = 0, minutes = 0, hours = 12)
# # high <- hms(seconds = 0, minutes = 59, hours = 23)
# 
# 
# # filter to indicator traffic for plots
# on <- usr_data %>% 
#   filter(indicator_on == 1)
# off <- usr_data %>% 
#   filter(indicator_off == 1)
# con <- usr_data %>% 
#   filter(indicator_con == 1)
# dis <- usr_data %>% 
#   filter(indicator_dis == 1)
# web <- usr_data %>% 
#   filter(indicator_web == 1)
# 
# # logon/logoffs
# ggplot() +
#   geom_point(mapping = aes(x = day, y = time), data = on, alpha = 0.5, color = "blue") +
#   geom_point(mapping = aes(x = day, y = time), data = off, alpha = 0.5, color = "red") +
#   facet_wrap(~logoff_mis, nrow = 2) +
#   scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle(paste0(usr, " Logon/Logoffs")) +
#   ggsave(paste0("LogonLogoff_", usr_name, ".png"), dpi = 1000, device = "png")
# 
# # connect/disconnects
# if(nrow(con) != 0){
#   
# ggplot() +
#     geom_point(mapping = aes(x = day, y = time), data = con, alpha = 0.5, color = "blue") +
#     geom_point(mapping = aes(x = day, y = time), data = dis, alpha = 0.5, color = "red") +
#     scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
#     facet_wrap(~usb_mis_dis, nrow = 2) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     ggtitle(paste0(usr, " USB Connects/Disconnects")) +
#     ggsave(paste0("ConnectDisconnect_", usr_name, ".png"), dpi = 1000, device = "png")
#   
# }
# 
# # web
# ggplot() +
#   geom_point(mapping = aes(x = day, y = time), data = web, alpha = 0.5, color = "blue") +
#   scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle(paste0(usr, " Web Traffic")) +
#   ggsave(paste0("Web_", usr_name, ".png"), dpi = 1000, device = "png")



# test for movie ----------------------------------------------------------


saveGIF({
  for(usr in unique(big_data$user)){
    
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
    
    # # logon/logoffs
    # p <- ggplot() +
    #   geom_point(mapping = aes(x = day, y = time), data = on, alpha = 0.5, color = "blue") +
    #   geom_point(mapping = aes(x = day, y = time), data = off, alpha = 0.5, color = "red") +
    #   facet_wrap(~logoff_mis, nrow = 2) +
    #   scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   ggtitle(paste0(usr, " Logon/Logoffs"))
    # print(p)
    
    # connect/disconnects
    # if(nrow(con) != 0){
    # 
    #   p <- ggplot() +
    #     geom_point(mapping = aes(x = day, y = time), data = con, alpha = 0.5, color = "blue") +
    #     geom_point(mapping = aes(x = day, y = time), data = dis, alpha = 0.5, color = "red") +
    #     scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #     facet_wrap(~usb_mis_dis, nrow = 2) +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     ggtitle(paste0(usr, " USB Connects/Disconnects"))
    #   print(p)
    # 
    # }

    # web
    p <- ggplot() +
      geom_point(mapping = aes(x = day, y = time), data = web, alpha = 0.5, color = "blue") +
      scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste0(usr, " Web Traffic"))
    print(p)
    
  }
}, movie.name = "web.gif", interval = 1, nmax = 1000)


# movie for each role -----------------------------------------------------

# find nice way to make a for loop to do this for me easily
# since much computation is involved and 15 different roles

big_data <- read_csv("big_data.csv")
names(big_data)
unique(big_data$role)

saveGIF({
  for(usr in unique(big_data$user)){
    
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
    
    # # logon/logoffs
    # p <- ggplot() +
    #   geom_point(mapping = aes(x = day, y = time), data = on, alpha = 0.5, color = "blue") +
    #   geom_point(mapping = aes(x = day, y = time), data = off, alpha = 0.5, color = "red") +
    #   facet_wrap(~logoff_mis, nrow = 2) +
    #   scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   ggtitle(paste0(usr, " Logon/Logoffs"))
    # print(p)
    
    # connect/disconnects
    # if(nrow(con) != 0){
    # 
    #   p <- ggplot() +
    #     geom_point(mapping = aes(x = day, y = time), data = con, alpha = 0.5, color = "blue") +
    #     geom_point(mapping = aes(x = day, y = time), data = dis, alpha = 0.5, color = "red") +
    #     scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #     facet_wrap(~usb_mis_dis, nrow = 2) +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     ggtitle(paste0(usr, " USB Connects/Disconnects"))
    #   print(p)
    # 
    # }

    # web
    p <- ggplot() +
      geom_point(mapping = aes(x = day, y = time), data = web, alpha = 0.5, color = "blue") +
      scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste0(usr, " Web Traffic"))
    print(p)
    
  }
}, movie.name = "web.gif", interval = 1, nmax = 1000)


# gifs without faceting on the missing traffic ----------------------------

rm(list = ls())
big_data <- read_csv("big_data.csv")
names(big_data)
unique(big_data$role)

saveGIF({
  for(usr in unique(big_data$user)){
    
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
    
    # # logon/logoffs
    # p <- ggplot() +
    #   geom_point(mapping = aes(x = day, y = time), data = on, alpha = 0.5, color = "blue") +
    #   geom_point(mapping = aes(x = day, y = time), data = off, alpha = 0.5, color = "red") +
    #   scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   ggtitle(paste0(usr, " Logon/Logoffs"))
    # print(p)

    # connect/disconnects
    # if(nrow(con) != 0){
    # 
    #   p <- ggplot() +
    #     geom_point(mapping = aes(x = day, y = time), data = con, alpha = 0.5, color = "blue") +
    #     geom_point(mapping = aes(x = day, y = time), data = dis, alpha = 0.5, color = "red") +
    #     scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     ggtitle(paste0(usr, " USB Connects/Disconnects"))
    #   print(p)
    # 
    # }
    
    # web
    p <- ggplot() +
      geom_point(mapping = aes(x = day, y = time), data = web, alpha = 0.5, color = "blue") +
      scale_x_date(date_breaks='2 weeks', date_minor_breaks = "1 weeks") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste0(usr, " Web Traffic"))
    print(p)
    
  }
}, movie.name = "web_no_facet.gif", interval = 1, nmax = 1000)
