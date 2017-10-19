# usb disconnect insert script
# sample user "ACME/KLS0717"
usr <- "ACME/KLS0717"
# usr <- unique(logon$user)[sample(1:length(unique(logon$user)), 1)]
test <- combo_filter(usr = usr)
head(test)
tmp <- test %>%
  filter(usb != "none")
unique(tmp$pc)
# get subset for trying loop on
tmp <- tmp[1:46, ]
# find number of consecutive connects
my_log <- tmp$usb == lag(tmp$usb)
my_log <- na.omit(my_log)
sum(my_log)
# so should have two extra rows to tmp
nrow(tmp)

new_usb <- tibble(date = as_datetime(now()), 
                  pc = "", 
                  activity = "",
                  day = as.Date(date),
                  time = hms::as.hms(date),
                  usb = "",
                  website = "")

# add column to flag if the matching disconnect was missing
# tmp$mis_dis <- NA

tmp <- as.tibble(tmp)

for(i in 1:(nrow(tmp)-1)){
  
  # get next row for checking
  cur <- tmp[i, ]
  nxt <- tmp[i+1, ]
  
  # check if they same, then create row and add to df
  if(cur$usb == nxt$usb){
    new_row <- cur
    new_row$usb <- 'Disconnect'
    # cur$mis_dis <- TRUE
    new_usb <- rbind(new_usb, rbind(cur, new_row))
  } else{
    
   new_usb <- rbind(new_usb, cur)
    
  }
  
}

new_usb <- new_usb[2:nrow(new_usb), ]
nrow(new_usb)





















# check unique usb users and pcs ------------------------------------------


# try to find users using usb with more than one machine
length(unique(device$user))
sto <- rep(NA, length(unique(logon$user)))
for(i in 1:length(unique(logon$user))){
  usr <- unique(logon$user)[i]
  test <- combo_filter(usr = usr)
  tmp <- test %>%
    filter(usb != "none")
  if(length(unique(tmp$pc)) > 0){
    sto[i] <- usr
  }
}
length(sto[is.na(sto)])
sto <- sto[!is.na(sto)]
# gives 228, which is the unique users in here
# now double check that none of the users use more than one machine with usb
sto2 <- rep(NA, length(sto))
for(i in 1:length(sto)){
  usr <- sto[i]
  test <- combo_filter(usr = usr)
  tmp <- test %>%
    filter(usb != "none")
  if(length(unique(tmp$pc)) > 1){
    sto2[i] <- usr
  }
}
length(sto2[!is.na(sto2)])
# so no users have used usb devices with more than one pc






# going to have to do this without loops after it works
# for this user it's a 354*886 for loop
# does this have to be a double for loop though?
# for(the_day in unique(test$day)){
#   for(comp in unique(test$pc)){
#     
#     tmp <- test %>%
#       filter(day == the_day, pc == comp, usb != "none")
#     
#   }
# }

# use lag to find indices that do not have corresponding disconnect
# head(lag(tmp$usb))
# head(tmp$usb)
# indices <- tmp$usb != lag(tmp$usb)
# indices <- !indices
# head(tmp[indices, ])
# tail(tmp[indices, ])
