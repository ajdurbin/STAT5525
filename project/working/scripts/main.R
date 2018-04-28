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
  
  # # create directory
  # dir.create(paste0("../outputs/", end_date))
  # 
  # # filter dates
  # big_data <- all_data %>% 
  #   filter(between(day, start_date, end_date))
  # 
  # # source files
  # source("avg_on_time.R")
  # # source("avg_usb_time.R")
  # source("revised_avg_usb_time.R")
  # source("web_filter.R")
  
  # functions to remove NA, NaN, Inf, -INF
  is.nan.data.frame <- function(x){
      do.call(cbind, lapply(x, is.nan))
  }
  is.inf.data.frame <- function(x){
      do.call(cbind, lapply(x, is.infinite))
  }
  logon_distribution <- read_csv(paste0("../outputs/", end_date, 
                                        "/logon_distribution.csv"))
  logon_distribution[is.na(logon_distribution)] <- 0
  logon_distribution[is.nan.data.frame(logon_distribution)] <- 0
  logon_distribution[is.inf.data.frame(logon_distribution)] <- 0
  
  usb_distribution <- read_csv(paste0("../outputs/", end_date, 
                                        "/usb_distribution.csv")) %>% 
      mutate(bad_connects_pcs = 
                 ifelse(is.na(bad_connects_pcs), "", bad_connects_pcs))
  usb_distribution[is.na(usb_distribution)] <- 0
  usb_distribution[is.nan.data.frame(usb_distribution)] <- 0
  usb_distribution[is.inf.data.frame(usb_distribution)] <- 0
  
  web_distribution <- read_csv(paste0("../outputs/", end_date, 
                                        "/web_distribution.csv"))
  web_distribution[is.na(web_distribution)] <- 0
  web_distribution[is.nan.data.frame(web_distribution)] <- 0
  web_distribution[is.inf.data.frame(web_distribution)] <- 0
  
  # source output
  rmarkdown::render(input = "template.Rmd",
                    output_file = paste0("../outputs/", end_date,
                    "/fits.html"))

  # # write outputs
  # write_csv(logon_distribution,
  #           path = paste0("../outputs/", end_date,
  #                         "/logon_distribution.csv"))
  # write_csv(usb_distribution,
  #           path = paste0("../outputs/", end_date,
  #                         "/usb_distribution.csv"))
  # write_csv(web_distribution,
  #           path = paste0("../outputs/", end_date,
  #                         "/web_distribution.csv"))

  # garbage collection
  rm(logon_distribution, usb_distribution, web_distribution, raw,
     logfit, trc)
  
}
