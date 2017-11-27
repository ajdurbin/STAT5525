library(readr)
path <- "/groups/DataLightHouse/DataSets2_10012017/"
f <- function(x, pos){
    return(x)
}
http <- read_csv_chunked(paste0(path, "http_info.csv"), DataFrameCallback$new(f), 
    chunk_size = 100000)
http <- http[100000, ]
write_csv(http, "../data/http_small.csv")

