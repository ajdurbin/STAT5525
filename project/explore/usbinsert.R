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
  
  # check last entry to make sure not lonely connect
  if(tail(no_match$usb, n = 1) == "Connect"){
    
    new_row <- tail(no_match, n = 1)
    new_row$usb <- 'Disconnect'
    new_row$usb_mis_dis <- TRUE
    new_usb <- rbind(new_usb, new_row)
    
  }
  
  # remove dummy row
  new_usb <- new_usb[2:nrow(new_usb), ]
  # add new matching rows
  new_match <- rbind(no_match, new_usb)
  # recombine total
  pckg <- rbind(yes_match, new_match)
  return(pckg)
  
}

usr <- "ACME/KLS0717"
test <- combo_filter(usr = usr)
# get usb traffic to check consecutive connects
usb <- test %>%
  filter(usb != "none")

# filter to make smaller and better visualize the differences
usb <- usb[c(1,2,3,5,6,7),]
usb <- insert_usb(usb)

# usb users for more robust testing of only one pc
bd <- read_csv("big_data.csv")
usb_users <- bd %>% 
  filter(usb != "none")
usb_users <- unique(usb_users$user)
# loop through usb users and count how many pc they use
usb_users_pc_count <- rep(0,length(usb_users))
for(i in 1:length(usb_users)){
  
  usr <- usb_users[i]
  
  tmp <- device %>% 
    filter(user == usr) %>% 
    select(pc) %>% 
    unique() %>% 
    nrow()
  
 usb_users_pc_count[i] = tmp 
  
}
max(usb_users_pc_count)

# # scratch for function ----------------------------------------------------
# 
# 
# # usb disconnect insert script
# # sample user "ACME/KLS0717"
# usr <- "ACME/KLS0717"
# # usr <- unique(logon$user)[sample(1:length(unique(logon$user)), 1)]
# test <- combo_filter(usr = usr)
# # see how many rows i add in end
# nrow(test)
# # head(test)
# no_match <- test %>%
#   filter(usb != "none")
# yes_match <- test %>%
#   filter(usb == "none")
# # unique(no_match$pc)
# # get subset for trying loop on
# no_match <- no_match[1:46, ]
# # find number of consecutive connects
# my_log <- no_match$usb == lag(no_match$usb)
# # first value is na, set it to false, first value doesnt matter since
# # we checking if the second value is equal to the first
# my_log[1] <- FALSE
# # my_log <- na.omit(my_log)
# sum(my_log)
# # so should have two extra rows to no_match
# nrow(no_match)
# 
# # now to get all rows that are not consecutive connects
# no_con <- no_match[!my_log,]
# nrow(no_con)
# # how to get average difference without loop?
# 
# new_usb <- tibble(date = as_datetime(now()), 
#                       pc = "", 
#                       activity = "",
#                       day = as.Date(date),
#                       time = as.hms(date),
#                       usb = "",
#                       website = "",
#                       mis_dis = NA)
# 
# # add column to flag if the matching disconnect was missing
# # no_match$mis_dis <- NA
# 
# # no_match <- as.tibble(no_match)
# 
# for(i in 1:(nrow(no_match)-1)){
#   # get next row for checking
#   cur <- no_match[i, ]
#   nxt <- no_match[i+1, ]
#   # check if they same, then create row and add to df
#   if(cur$usb == nxt$usb){
#     new_row <- cur
#     new_row$usb <- 'Disconnect'
#     new_row$mis_dis <- TRUE
#     new_usb <- rbind(new_usb, new_row)
#   }
# }
# 
# # remove dummy row
# new_usb <- new_usb[2:nrow(new_usb), ]
# nrow(new_usb) # now should match the sum(my_log)
# # update no_match and arrange by date
# no_match <- rbind(no_match, new_usb)
# no_match <- no_match %>% 
#   arrange(date)
# pcgk <- rbind(yes_match, no_match)
# nrow(test)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # check unique usb users and pcs ------------------------------------------
# 
# 
# # try to find users using usb with more than one machine
# length(unique(device$user))
# sto <- rep(NA, length(unique(logon$user)))
# for(i in 1:length(unique(logon$user))){
#   usr <- unique(logon$user)[i]
#   test <- combo_filter(usr = usr)
#   no_match <- test %>%
#     filter(usb != "none")
#   if(length(unique(no_match$pc)) > 0){
#     sto[i] <- usr
#   }
# }
# length(sto[is.na(sto)])
# sto <- sto[!is.na(sto)]
# # gives 228, which is the unique users in here
# # now double check that none of the users use more than one machine with usb
# sto2 <- rep(NA, length(sto))
# for(i in 1:length(sto)){
#   usr <- sto[i]
#   test <- combo_filter(usr = usr)
#   no_match <- test %>%
#     filter(usb != "none")
#   if(length(unique(no_match$pc)) > 1){
#     sto2[i] <- usr
#   }
# }
# length(sto2[!is.na(sto2)])
# # so no users have used usb devices with more than one pc
# 
# 
# 
# 
# 
# 
# # going to have to do this without loops after it works
# # for this user it's a 354*886 for loop
# # does this have to be a double for loop though?
# # for(the_day in unique(test$day)){
# #   for(comp in unique(test$pc)){
# #     
# #     no_match <- test %>%
# #       filter(day == the_day, pc == comp, usb != "none")
# #     
# #   }
# # }
# 
# # use lag to find indices that do not have corresponding disconnect
# # head(lag(no_match$usb))
# # head(no_match$usb)
# # indices <- no_match$usb != lag(no_match$usb)
# # indices <- !indices
# # head(no_match[indices, ])
# # tail(no_match[indices, ])
