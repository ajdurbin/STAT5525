# graphics for fired employees
library(tidyverse)
rm(list = ls())

logon_distribution <- read_csv("logon_distribution.csv")
names(logon_distribution)
web_distribution <- read_csv("web_distribution.csv")
names(web_distribution)
usb_distribution <- read_csv("usb_distribution.csv")
names(usb_distribution)
tenure_distribution <- read_csv("tenure_distribution.csv")
names(tenure_distribution)

combo <- merge(logon_distribution, web_distribution)
combo <- merge(combo, tenure_distribution)
# does not do properly, adds the new usb users as separate
combo <- merge(combo, usb_distribution)

lf <- logon_distribution %>% 
  filter(attrition == 1)
wf <- web_distribution %>% 
  filter(attrition == 1)
uf <- usb_distribution %>% 
  filter(attrition == 1)

# get how many people were fired by role
# then make simple graphics of this based on the variables i have
logon_distribution %>% 
  group_by(role) %>% 
  summarise(n = n(), total_attrition = sum(attrition),
            pct_attrition = sum(attrition) / n()) %>% 
  arrange(n)

# of council folks
tmp <- logon_distribution %>% 
  filter(role == "OfCouncil")
