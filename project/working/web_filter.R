# web traffic analysis
# find users with lots of traffic that is like
# thiswebsite.com, this-website.net, this.website.pu, etc

rm(list = ls())
options(stringsAsFactors = FALSE)
library(tidyverse)
library(stringr)
library(lubridate)
library(hms)

http <- read_csv("/home/alex/data_analytics/DataSets1_9182017/http_info.csv",
                 col_names = FALSE)
colnames(http) <- c("id", "date", "user", "pc", "website")
http$date <- lubridate::mdy_hms(http$date)
http$time <- hms::as.hms(http$date)
http$day <- lubridate::as_date(http$date)
http <- http %>% 
  arrange(date)


# some exploratory analysis -----------------------------------------------


# pick random user
usr <- sample(unique(http$user), 1)
usr_web <- http %>% 
  filter(user == usr)
# usr <- "ACME/MVW0630"
# candidate user, has several isntances of doubt.something.com, doubt-this.net

# filter out the http://
str_sub(usr_web$website, start = 1, end = 7) <- ""
# get tld from each website
# usr_web$tld <- sub(".*\\.", "", usr_web$website)
# like this version better
usr_web$tld <- str_extract(string = usr_web$website, pattern = "[^.]*$")
# domain, everything before last period
usr_web$domain <- str_replace(string = usr_web$website, 
                              pattern = "\\.[^\\.]*$",
                              replacement = "")
# split domains at remaining delimiters, ".", "-"
# returns lsit object
# need to recombine into a vector
word_list <- str_split(string = usr_web$domain, pattern = "[-|.]")
word_vec <- unlist(word_list)
word_vec <- unique(word_vec)
word_df <- data.frame(word = word_vec, count = 0)
for(i in 1:nrow(word_df)){
  
  wrd <- word_df[i, 1]
  word_df[i, 2] <- sum(str_detect(string = usr_web$domain,
                                  pattern = wrd))
  
}
word_df <- word_df %>% 
  arrange(desc(count))

# there is an empty string in here somehow
# emptry string is from people visiting ".com"
# so my split techniques return the empty string
# str_detect may not be best tool for the job
# picking up alot of the partial matches that may not really be what i want
# picks up a lot of, t, at, co, etc that come from fraction of observations



# website filter script ---------------------------------------------------


# get sample statistics from each user like:
# num websites visit
# primary website pc
# num unique websites
# most visit ssites
# how many .com, etc

rm(list = ls())
options(stringsAsFactors = FALSE)
library(tidyverse)
library(stringr)
library(lubridate)
library(hms)

# tld
# am, cn, co, com, eu, fm, gov, li, me, net, org, ph, tv, uk, us

big_data <- read_csv("big_data.csv")

big_data <- big_data %>% 
  mutate(usb_mis_dis = ifelse(is.na(usb_mis_dis), "", usb_mis_dis)) %>% 
  mutate(logoff_mis = ifelse(is.na(logoff_mis), "", logoff_mis))

unique_users <- unique(big_data$user)
# usr <- sample(unique_users, 1)

for(usr in unique_users){
  
  usr_web <- big_data %>% 
    filter(user == usr, website != "none")
  usr_pcs <- unique(usr_web$pc)
  
  # get time differences between web traffic
  # note that this is across days so this may not
  # be a good measure since we calculating difference between someone
  # going to website at like 6 at night and the next one is at
  # like 7 in the morning
  # maybe i just want to flag counts less than 1 min, 5 min again
  # using interval, duration, period from lubridate
  # want something like average run length
  # find how many runs someone goes through websites super fast
  # that would be way to get those vertical bars on graphics
  time_interval <- lag(usr_web$date) %--% usr_web$date
  dif <- as.duration(time_interval)
  dif <- as.numeric(dif)
  dif <- dif/60
  
  # want to get the run lengths for people spamming websites too quickly
  # so need to somehow incorporate the comparison and run lengths at 
  # same time
  # so do in two steps i think
  # get dif < 1 and then do run length and count groups more than 5
  small_dif <- na.omit(dif) < 1
  run_lengths <- rle(small_dif)
  runs <- sum(run_lengths$lengths >= 5)
  
  
  # get most visited website
  # web_table <- data.frame(table(usr_web$website))
  # web_table <- web_table %>% 
  #   arrange(desc(Freq))
  # separate the tld from the domains
  # want to transpose it and cbind it with row later
  # tld <- str_extract(string = web_table$Var1, pattern = "[^.]*$")
  # tld_table <- data.frame(table(tld))
  
  
  # row
  row <- data.frame(
    user = usr,
    primary_pc = unique(usr_web$primary_pc),
    total_pc_count = unique(usr_web$pc_count),
    web_pc_count = length(usr_pcs),
    qck_web_runs = runs,
    qck_web_lt_1_min = sum(na.omit(dif) < 1),
    qck_web_lt_5_min = sum(na.omit(dif) < 5),
    role = unique(usr_web$role),
    attrition = unique(usr_web$attrition),
    num_am = sum(endsWith(usr_web$website, ".am")),
    num_cn = sum(endsWith(usr_web$website, ".cn")),
    num_co = sum(endsWith(usr_web$website, ".co")),
    num_com = sum(endsWith(usr_web$website, ".com")),
    num_eu = sum(endsWith(usr_web$website, ".eu")),
    num_fm = sum(endsWith(usr_web$website, ".fm")),
    num_gov = sum(endsWith(usr_web$website, ".gov")),
    num_li = sum(endsWith(usr_web$website, ".li")),
    num_me = sum(endsWith(usr_web$website, ".me")),
    num_net = sum(endsWith(usr_web$website, ".net")),
    num_org = sum(endsWith(usr_web$website, ".org")),
    num_ph = sum(endsWith(usr_web$website, ".ph")),
    num_tv = sum(endsWith(usr_web$website, ".tv")),
    num_uk = sum(endsWith(usr_web$website, ".uk")),
    num_us = sum(endsWith(usr_web$website, ".us"))
  )
  
  if(!exists("web_distribution")){
    web_distribution <- row
  } else{
    web_distribution <- plyr::rbind.fill(web_distribution, row)
  }

}

write_csv(web_distribution, "web_distribution.csv")
