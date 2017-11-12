options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "plyr", "lubridate", "hms", "animation", "readr",
        "ggplot2")
lapply(._, library, character.only = TRUE)
which <- "email"

combo <- read_csv("../data/timestamps.csv")
combo <- combo %>%
    mutate(activity = ifelse(is.na(activity), "", activity)) %>%
    mutate(ind_logon = ifelse(activity == "Logon", 1, 0)) %>%
    mutate(ind_logoff = ifelse(activity == "Logoff", 1, 0)) %>%
    mutate(usb = ifelse(is.na(usb), "", usb)) %>%
    mutate(ind_con = ifelse(usb == "Connect", 1, 0)) %>%
    mutate(ind_dis = ifelse(usb == "Disconnect", 1, 0)) %>%
    mutate(download = ifelse(is.na(download), "", download)) %>%
    mutate(ind_down = ifelse(download == 1, 1, 0)) %>%
    mutate(email = ifelse(is.na(email), "", email)) %>%
    mutate(ind_email = ifelse(email == 1, 1, 0)) %>%
    mutate(web = ifelse(is.na(web), "", web)) %>%
    mutate(ind_web = ifelse(web == 1, 1, 0))

saveGIF({

    for(usr in unique(combo$user)){
        
        usr_data <- combo %>%
            filter(user == usr)
        
        if(which == "logon"){
            # logon/logoff
            on <- usr_data %>%
                filter(ind_logon == 1)
            off <- usr_data %>%
                filter(ind_logoff == 1)
            p <- ggplot() +
                   geom_point(mapping = aes(x = day, y = time, color = "Logon"), 
                             data = on, alpha = 0.5) +
                   geom_point(mapping = aes(x = day, y = time, color = "Logoff"), 
                              data = off, alpha = 0.5) +
                   scale_x_date(date_breaks = "2 weeks", 
                       date_minor_breaks = "1 weeks") +
                   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                   ggtitle(paste0(usr, " Logon/Logoffs")) +
                   scale_color_manual("Activity", values = c("red", "blue"))
            print(p)
        }

        if(which == "usb"){
            # connect/disconnect
            con <- usr_data %>%
                filter(ind_con == 1)
            dis <- usr_data %>%
                filter(ind_dis == 1)
            if(nrow(con) != 0){
                p <- ggplot() +
                    geom_point(mapping = aes(x = day, y = time,
                                            color = "Connect"),
                             data = con, alpha = 0.5) +
                    geom_point(mapping = aes(x = day, y = time,
                                           color = "Disconnect"),
                            data = dis, alpha = 0.5) +
                    scale_x_date(date_breaks = "2 weeks", 
                       date_minor_breaks = "1 weeks") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    ggtitle(paste0(usr, " Connect/Disconnects")) +
                    scale_color_manual("Activity", values = c("red", "blue"))
                print(p)
            }
        }
        
         if(which == "download"){
            # connect/disconnect
            down <- usr_data %>%
              filter(ind_down == 1)
            if(nrow(down) != 0){
                p <- ggplot() +
                    geom_point(mapping = aes(x = day, y = time,
                                            color = "Download"),
                            data = down, alpha = 0.5) +
                   scale_x_date(date_breaks = "2 weeks", 
                                date_minor_breaks = "1 weeks") +
                   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                   ggtitle(paste0(usr, " Downloads")) +
                   scale_color_manual("Activity", values = c("red"))
                print(p)
          }
        }

       if(which == "web"){
           # web 
           web <- usr_data %>%
            filter(ind_web == 1)
            p <- ggplot() +
                 geom_point(mapping = aes(x = day, y = time,
                                    color = "Web Visit"),
                   data = web, alpha = 0.5) +
               scale_x_date(date_breaks = "2 weeks", 
                   date_minor_breaks = "1 weeks") +
               theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
               ggtitle(paste0(usr, " Website Visits")) +
               scale_color_manual("Activity", values = c("red"))
            print(p)
        }

        if(which == "email"){
            # email
            email <- usr_data %>%
                filter(ind_email == 1)
            p <- ggplot() +
                geom_point(mapping = aes(x = day, y = time,
                                       color = "Email Sent"), 
                        data = email, alpha = 0.5) +
                scale_x_date(date_breaks = "2 weeks", 
                    date_minor_breaks = "1 weeks") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ggtitle(paste0(usr, " Email Traffic")) +
                scale_color_manual("Activity", values = c("red"))
            print(p)
        }

    }

}, movie.name = paste0(which, ".gif"), interval = 1.2, max = 10000)
