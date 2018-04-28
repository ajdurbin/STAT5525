# explore after hours information
logon <- read_csv("../data/logon_distribution.csv") %>% 
  arrange(desc(attrition)) %>% 
  mutate(weekday = weekdays(date))
web <- read_csv("../data/web_distribution.csv") %>% 
  arrange(desc(attrition))
device <- read_csv("../data/usb_distribution.csv") %>% 
  arrange(desc(attrition))
big_data <- read_csv("../data/big_data.csv")
tenure <- read_csv("../data/tenure_distribution.csv")

# interesting usb users
# usr <- "ACME/AFF0760" # wednesdays between midnight and 5
# usr <- "ACME/LKY0181" # M 9-11, W 3-6, T 6-6, T/W 10-1, F-S 10-1
# usr <- "ACME/GML0105" # two satuday, one wednesday all early between 3-4
# Ty 4-5, T 2-3, Th 7-8, T 3-4, 5-6 F, F 130-3, F 6-6 2 websites, Tu 2-2
# usr <- "ACME/CTR0537" # wednesday and monday early mornings, one at 8
# 1-3 wed,11 mon, 12-5 wed, 10 wed
# all have majority after hour usb connections
# now want to look at all of their traffic?
usr_big <- big_data %>% 
  filter(user == usr) %>% 
  mutate(weekday = weekdays(as_date(date)))
usr_web <- web %>% 
  filter(user == usr)
usr_logon <- logon %>% 
  filter(user == usr)

# lots of wednesday between these people, are they working together? 
# get their dates
bad_users <- c("ACME/AFF0760", "ACME/LKY0181", "ACME/GML0105", "ACME/CTR0537")
dates <- big_data %>% 
  filter(user %in% bad_users) %>% 
  filter(usb == "Connect") %>% 
  select(date, user)

last_dates <- tenure %>% 
  filter(user %in% bad_users) %>% 
  select(user, role, end_date, tenure_days)

# doesnt really like they worked together, only one instance of two
# actually working in almost the same time
# only two people did same data at late times
# were the fired same day?
# all fired on different days