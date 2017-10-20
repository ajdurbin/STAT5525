# logoff insert
insert_logoff <- function(combo){
  
  # split up
  # logon/logoff
  no_match <- combo %>%
    filter(activity != "using")
  # no logon/logoff
  yes_match <- combo %>%
    filter(activity == "using")
  
  # tmp storage
  new_off <- tibble(date = as_datetime(now()), 
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
    if(cur$activity == nxt$activity){
      new_row <- cur
      new_row$activity <- 'Logoff'
      new_row$logoff_mis <- TRUE
      new_off <- rbind(new_off, new_row)
    }
  }
  
  # remove dummy row
  new_off <- new_off[2:nrow(new_off), ]
  # add new matching rows
  new_match <- rbind(no_match, new_off)
  # recombine total
  pckg <- rbind(yes_match, new_match)
  return(pckg)
  
}

# test this out
usr <- "ACME/KLS0717"
test <- combo_filter(usr = usr)
nrow(test)
nrow(test %>% filter(activity == "using"))
nrow(test %>% filter(activity == "Logon"))
nrow(test %>% filter(activity == "Logoff"))
tmp <- insert_logoff(combo = test)
tmp <- tmp %>% 
  arrange(date, pc)
nrow(tmp)
nrow(tmp %>% filter(activity == "using"))
nrow(tmp %>% filter(activity == "Logon"))
nrow(tmp %>% filter(activity == "Logoff"))
# adds more rows than necessary
