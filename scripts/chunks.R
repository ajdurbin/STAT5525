library(readr)
library(dplyr)
library(lubridate)
library(hms)
library(plyr)

path <- "/groups/DataLightHouse/DataSets2_10012017/"
#chunksize <- 89642
#transact_file <- paste0(path, "http_info.csv")
#con <- file(description = transact_file, open = "r")
#data <- read.table(con, nrow = chunksize, header = F, fill = T, sep = ",")
#data <- data[(-1), ]
#my_names <- c("id", "date", "user", "pc", "url", "content")
#colnames(data) <- my_names
#
#combo <- read_csv("combo.csv")
#data <- data %>%
#    select(date, user, pc) %>%
#    mutate(website = 1, date = mdy_hms(date), day = as_date(date),
#        time = as.hms(date))
#
#combo <- rbind.fill(combo, data)
#i <- 1
#
#while(i < 3172){
#
#    data <- read.table(con, nrow = chunksize, col.names = my_names, fill = T,
#        sep = ",")
#
#    paste0("\nProcessing row ", i, " of 3172\n")
#
#    colnames(data) <- my_names
#    data <- data %>%
#        select(date, user, pc) %>%
#        mutate(website = 1, date = mdy_hms(date), day = as_date(date),
#            time = as.hms(date))
#    combo <- rbind.fill(combo, data)
#    i <- i + 1
#
#}
#
#close(con)
#write_csv(combo, "no_content.csv")

f <- function(x, pos){
    return(x[, c(2, 3, 4)])
}

http <- read_csv_chunked(paste0(path, "http_info.csv"), DataFrameCallback$new(f), 
    chunk_size = 100000)

http <- http %>%
   mutate(date = mdy_hms(date), time = as.hms(date), day = as_date(date)) %>%
   mutate(web = 1)

combo <- read_csv("combo.csv")
timestamps <- plyr::rbind.fill(combo, http)
write_csv(timestamps, "timestamps.csv")

