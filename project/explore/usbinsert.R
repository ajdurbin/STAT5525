# usb disconnect insert script
# usr <- unique(logon$user)[sample(1:length(unique(logon$user)), 1)]
# test <- combo_filter(usr = usr)
# head(test)
# tmp <- test %>%
#   filter(usb != "none")
# unique(tmp$pc)

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
