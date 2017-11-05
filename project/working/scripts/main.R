# main

rm(list = ls())
options(stringsAsFactors = FALSE)

libs <- c("tidyverse", "stringr", "lubridate", "hms", "caret", "glmnet")
lapply(libs, library, character.only = TRUE)

tenure_distribution <- read_csv("../data/tenure_distribution.csv")
fired <- tenure_distribution %>% 
  filter(attrition == 1) %>% 
  arrange(end_date)

all_data <- read_csv("../data/big_data.csv")

start_date <- unique(fired$start_date)
end_dates <- unique(fired$end_date)
# test
# end_dates <- end_dates[1:3]

for(end_date in end_dates){
  
  end_date <- as_date(end_date)
  
  # create directory
  dir.create(paste0("../outputs/", end_date))
  
  # filter dates
  big_data <- all_data %>% 
    filter(between(day, start_date, end_date))
  
  # source files
  source("avg_on_time.R")
  source("avg_usb_time.R")
  source("web_filter.R")

  # source output
  rmarkdown::render(input = "template.Rmd",
                    output_file = paste0("../outputs/", end_date,
                    "/fit.html"))

  # write outputs
  write_csv(logon_distribution,
            path = paste0("../outputs/", end_date,
                          "/logon_distribution.csv"))
  write_csv(usb_distribution,
            path = paste0("../outputs/", end_date,
                          "/usb_distribution.csv"))
  write_csv(web_distribution,
            path = paste0("../outputs/", end_date,
                          "/web_distribution.csv"))

  # garbage collection
  rm(logon_distribution, usb_distribution, web_distribution, raw,
     logfit, trc)
  
}
