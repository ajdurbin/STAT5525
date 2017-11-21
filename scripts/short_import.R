# short import
rm(list = ls())
._ <- c("data.table", "readr")
lapply(._, library, character.only = TRUE)
# read in first ~100000 rows as characters and modify as needed
http <- fread(input = "/groups/DataLightHouse/DataSets2_10012017/http_info.csv",
              sep = ",", nrows = 100000, header = TRUE, na.strings = "",
              stringsAsFactors = FALSE, drop = c("id", "content"),
              colClasses = "character")
email <- fread(input = "/groups/DataLightHouse/DataSets2_10012017/email_info.csv",
              sep = ",", nrows = 100000, header = TRUE, na.strings = "",
              stringsAsFactors = FALSE, drop = c("id", "content", "size"),
              colClasses = "character")
# write_csv(http, 
#           "/groups/DataLightHouse/1003/alex/data_analytics2/data/http_small.csv")
# write_csv(http, 
#           "/groups/DataLightHouse/1003/alex/data_analytics2/data/email_small.csv")