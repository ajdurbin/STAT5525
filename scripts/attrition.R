options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms")
lapply(._, library, character.only = TRUE)

raw <- read_csv("../data/timestamps.csv")
cur <- read_csv("../raw/LDAP/2011-05.csv")
raw <- raw %>%
    mutate(attrition = ifelse(user %in% cur$user_id, 0, 1))
write_csv(raw, "../data/big_data.csv")
