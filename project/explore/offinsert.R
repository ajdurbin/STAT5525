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


# build up manually ----


usr <- "ACME/KLS0717" # test user
test <- combo_filter(usr = usr) # get their combined data
test <- test[c(1:7,27:28), ]

insert_logoff <- function(combo){
  
  # unique pc
  all_pc <- unique(test$pc) # 52 different pc
  
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
    no_match  <- test %>%
      filter(pc == the_pc, activity != "using")
    yes_match <- test %>%
      filter(pc == the_pc, activity == "using")
    
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
        new_row$usb <- 'Logoff'
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
  
  return(pckg)
  
}

test <- insert_logoff(test)
