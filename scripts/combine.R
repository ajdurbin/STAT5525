rm(list = ls())
options(stringsAsFactors = FALSE)

libs <- c("dplyr", "stringr", "data.table", "readr", "lubridate", "hms",
        "plyr")
lapply(libs, library, character.only = TRUE)

# load and parse data -----

data_path <- "/groups/DataLightHouse/DataSets2_10012017/"

logon <- read_csv(paste0(data_path, "logon_info.csv"))
logon <- logon %>%
    select(user, date, pc, activity) %>%
    mutate(date = mdy_hms(date), time = as.hms(date), day = as_date(date))

device <- read_csv(paste0(data_path, "device_info.csv"))
device <- device %>%
    select(user, date, pc, activity) %>%
    mutate(date = mdy_hms(date), time = as.hms(date), day = as_date(date)) %>%
    mutate(usb = activity) %>%
    select(-(activity))

file <- read_csv(paste0(data_path, "file_info.csv"))
file <- file %>%
    select(user, date, pc) %>%
    mutate(date = mdy_hms(date), time = as.hms(date), day = as_date(date))%>%
    mutate(download = 1)

email <- read_csv(paste0(data_path, "email_info.csv"))
email <- email %>%
    select(user, date, pc, attachments, size) %>%
    mutate(date = mdy_hms(date), time = as.hms(date), day = as_date(date)) %>%
    mutate(email = 1)

combo <- rbind.fill(logon, device, file, email)
write_csv(combo, "combo.csv")

