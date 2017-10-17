# usb disconnect insert script
usr <- unique(logon$user)[sample(1:length(logon), 1)]
test <- combo_filter(usr = usr)
head(test)
tmp <- test %>%
  filter(usb != "none")
