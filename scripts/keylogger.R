# keylogger, virus, malware filtering
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "stringr", "parallel")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(extension = str_extract(filename, "[^.]*$")) %>% 
    mutate(keylogger = str_detect(content, "key")) %>% 
    mutate(virus = str_detect(content, "virus")) %>% 
    mutate(malware = str_detect(content, "malware"))

mal <- file %>%
    filter(malware == TRUE)
virus <- file %>%
    filter(virus == TRUE)
