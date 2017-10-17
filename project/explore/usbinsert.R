# usb disconnect insert script
usr <- unique(logon$user)[sample(1:length(logon), 1)]
test <- combo_filter(usr = usr)
head(test)
tmp <- test %>%
  filter(usb != "none")

# going to have to do this without loops after it works
# for this user it's a 354*886 for loop
# does this have to be a double for loop though?
for(the_day in unique(test$day)){
  for(comp in unique(test$pc)){
    
    tmp <- test %>%
      filter(day == the_day, pc == comp, usb != "none")
    
  }
}

lag(tmp$usb)
tmp
