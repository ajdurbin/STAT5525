# web traffic analysis

library(tidyverse)
library(stringr)

# load combined data from combine.R
big_data <- read_csv("big_data.csv")
# load raw web traffic to make sure filtered correclty
http <- read_csv("/home/alex/data_analytics/DataSets1_9182017/http_info.csv",
                 col_names = FALSE)
nrow(http)
# filter out web traffic
web <- big_data %>% 
  filter(website != "none")
nrow(web)
web <- web$website
sites <- web
# does not appear to be any https traffic
# recall that some websites are like instagr.am 
# need the tld of the domains to just in case
# remove http://
str_sub(sites, 1, 7) <- ""
head(sites)
domains <- str_extract(sites, "[^.]*$")
head(domains)
# look at distribution of the domains sorted
sort(table(domains))
# majority of com, org, net
# interesting, li,cn,fm,ph,me websites - some are url shorteners

# cn is china

# li is either a liechtenstein internet country code or
# a domain hack by url shortern companies like bit.ly

# fm is microstates of miniesia island but anyone can register one for 
# paying the island. mostly used for streaming radio websites

# .ph is phillipines

# .me is internet code for montenegro, but domain hacks
# for english, dutch, also lots of us investment

# .tv domain for tuvalu
# websites often feature video content for 
# specific brands or firms, brand-tailored .tv stations

# .eu is european union

# .am is armenia country coded
# used in domain hacks
# similar to last.fm radio stations
# also insta.gram
# video service Stre.am
# Will.I.Am

# .co 
# columbia domain
# but used in url shorteners for google/amazon/twitter

# now look at the distribution of sites more closely
length(sites)
# 3451665 total instances
length(unique(sites))
# 105428 unique websites
head(table(sites))
# don't get to see that many
# but something extremely interesting happens
# lots of websites that begin with a string, say advertisement
# then going to get lots of variations of things happening
# like advertisement.something, advertisementsomething,advertising-somethign
# ablebit, able-boy, ablebentweak,able-corkagainst etc

web_dist <- as.data.frame(table(sites))
# arrange from high to low
web_dist <- web_dist %>% 
  arrange(desc(Freq))
head(web_dist, n = 600)
ggplot(data = web_dist) + 
  geom_histogram(mapping = aes(x = Freq), bins = 300) +
  xlim(0, 100)
# large percentage of sites only visited once
# hypothesis is that they are the wordsomething word-something type





# get how many unique websites someone visits
# look at top sites for each of the domains
# get top website they visit anyways
# flag traffic that happens after hours
# torrent websites
# lots of the word.something, wordsomething, word-something websites are
# only visited once but all the different variations
# worthwhile to keep track of what these users are doing
# so what to do with these websites only visited once? 
# go through data and make running count of users and how many times
# they visit a particular site? write function to do so

user_list <- unique(big_data$user)
user_list <- sort(user_list)
