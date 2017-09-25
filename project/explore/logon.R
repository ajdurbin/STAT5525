library(tidyverse)

raw <- read_csv('~/da/DataSets1_9182017/logon_info.csv')
colnames(raw)
head(raw)
tail(raw)
length(unique(raw$user)) # 1000 unique users here
length(unique(raw$id)) # all id unique 
length(unique(raw$pc)) # 1100 unique pc, so 1000 user pc and 100 shared


raw$date <- as.POSIXlt(raw$date,format="%m/%d/%Y %H:%M:%S") # convert to correct date time object for plotting
raw <- raw %>% 
  mutate(on = ifelse(activity == 'Logon', 1, 0))

# good first idea is to take data differences and see frequent login logoffs
# across different pcs

raw %>% filter(activity == 'Logon') %>% 
  nrow() # 470877
raw %>% filter(activity == 'Logoff') %>% 
  nrow() # 378702
# about 92000 more logons than logoffs

# more logons than logoffs, recall these can be screen unlocks, screen locks
# are not recorded

# sort by user and break ties by pc and date
raw <- raw %>% 
  arrange(user, date, pc)
# want symmetrical logon logoff times, but already know this is not the case 
# so how to get these differences when non symmetrical logon logoff times?
# arrange by pc, time and make sure no two consecutive logon logoffs
# is this right way to sort? can 
# breaking ties by date and then sorting by pc makes more sense since the times
# should be unique?

# try plotting a specific users logon logoff data with by computer?
my_user <- raw[sample(1:1000, 1), ]$user
my_user_data <- raw %>%
  filter(user == my_user)
# so this gets a specific users information
# now want to plot the time series, but with indicators of logon logoff and overlap

ggplot(data = my_user_data[1:26, ]) +
  geom_line(mapping = aes(x = date, y = on)) +
  geom_point(mapping = aes(x = date, y = on))
# having problems doing simple time series plot, best if i could do it for given
# day, actual date may not matter

a_date <- raw[4, ]$date

# NEW COLUMNS FOR DATE AND TIME, I DONT THINK DATE WILL BE VERY INFORMATIVE
raw <- read_csv('~/da/DataSets1_9182017/logon_info.csv') 

raw$date = as.POSIXlt(raw$date,format = "%m/%d/%Y %H:%M:%S") 
raw <- raw %>%
  mutate(calendar = trunc(date, 'days'))
  # mutate(calendar = as.POSIXlt(date, format = "%m/%d/%Y"))

# look up lubridate package, should be useful to get time intervals