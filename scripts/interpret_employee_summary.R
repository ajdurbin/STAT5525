# employee_summary output
options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "chron", "hms", "parallel", "stringr")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"),
           time = hms::as.hms(date), 
           day = as.Date(date), hour = chron::hours(date)) %>% 
    select(-date) 
# device <- read_csv("../raw/device_info.csv") %>% 
#     mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"),
#            time = hms::as.hms(date), 
#            day = as.Date(date), hour = chron::hours(date)) %>% 
#     select(-date) 
# psych <- read_csv("../raw/psychometric_info.csv")
# latest <- read_csv("../raw/LDAP/2011-05.csv")
# original <- read_csv("../raw/LDAP/2009-12.csv")
# logon <- read_csv("../raw/logon_info.csv")
email <- read_csv("../data/email_small.csv") %>% 
    mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"),
           time = hms::as.hms(date), 
           day = as.Date(date), hour = chron::hours(date)) %>% 
    select(-date) 
http <- read_csv("../data/http_small.csv") %>% 
    mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"),
           time = hms::as.hms(date), 
           day = as.Date(date), hour = chron::hours(date)) %>% 
    select(-date) 
pckg <- read_csv("../data/employee_summary.csv")


# download information ----


table(pckg$total_exe)
# 770 do not download exe
# small groups of employees download exe with most doing so 76 times
table(pckg$total_after_hour_downloads)
# 26 employee download after hours
# several employee doing so hundreds of times
table(pckg$total_after_hour_downloads, pckg$attrition)
# 50-50 split between employees with hundreds of atfter hour downloads
# are fired/not-fired
table(pckg$total_download_pc, pckg$attrition)
# again, 50-50 split with attrition and multiple download pcs
# but there is a large jump, where those using less than 300 download
# machines are all still employed, but those greater are not
# useful to find out what these employees are doing
# look at their overall traffic compared to eachother
interest <- pckg %>% 
    filter(total_download_pc > 1)
# all are it admins, so this may not be interesting
# let's look at other employees who are not it admins and download after hour
interest <- pckg %>% 
    filter(total_after_hour_downloads > 0, employee_role != "ITAdmin")
interest_file <- file %>%
    filter(user %in% interest$employee_id)


# prince keyword ----


# look for prince http
prince_http <- str_detect(http$content, "prince")
prince_http <- http[prince_http, ]
prince_http_users <- unique(prince_http$user)
# look for prince files
prince_file <- str_detect(file$content, "prince")
prince_file_counts <- str_count(file$content, "prince")
prince_file <- file[prince_file, ]
prince_file <- prince_file %>%
    mutate(extension = str_extract(filename, "[^.]*$"))
barplot(table(prince_file$extension))
prince_file_users <- unique(prince_file$user)
interest <- pckg %>% 
    filter(employee_id %in% prince_file_users)
table(interest$attrition)
prince_pcs <- unique(prince_file$pc) # 400 something machines, not useful
prince_after_hours <- prince_file$hour > 0 & prince_file$hour < 5
# 71 after hours
prince_after_hours <- prince_file[prince_after_hours, ]
prince_file_users_after_hours <- unique(prince_after_hours$user)
# 5 users doing this after hours

# look for prince emails
prince_email <- str_detect(email$content, "prince")
prince_email_counts <- str_count(email$content, "prince")
prince_email <- email[prince_email, ]
prince_email_users <- unique(prince_email$user)
interest <- pckg %>% 
    filter(employee_id %in% prince_email_users)
table(interest$attrition)
# look at email users who are not usb downloaders
# dont think im doing this correctly
users <- pckg$employee_id
users <- data.frame(user = users) %>% 
    mutate(file = user %in% prince_file_users) %>% 
    mutate(email = user %in% prince_email_users) %>% 
    mutate(http = user %in% prince_http_users)
email_only_users <- users %>% 
    filter(file == FALSE, email == TRUE, http == FALSE) %>% 
    select(user) 
email_only_users <- email_only_users$user
file_only_users <- users %>% 
    filter(file == TRUE, email == FALSE, http == FALSE) %>% 
    select(user)
file_only_users <- file_only_users$user
http_only_users <- users %>% 
    filter(http == TRUE, email == FALSE, file == FALSE) %>% 
    select(user)
http_only_users <- http_only_users$user
# there are some mistakes, like non spam including prince multiple times
# but not the other two keywords from my first guess

# look for ahmose emails
ahmose_email <- str_detect(email$content, "ahmose")
ahmose_email <- email[ahmose_email, ]
ahmose_email_users <- unique(ahmose_email$user)
ahmose_file <- str_detect(file$content, "ahmose")
ahmose_file <- file[ahmose_file, ]
ahmose_file_users <- unique(ahmose_file$user)
ahmose_http <- str_detect(http$content, "ahmose")
ahmose_http <- http[ahmose_http, ]
ahmose_http_users <- unique(ahmose_http$user)

# look for ankh emails
ankh_email <- str_detect(email$content, "ankh")
ankh_email <- email[ankh_email, ]
ankh_email_users <- unique(ankh_email$user)
ankh_file <- str_detect(file$content, "ankh")
ankh_file <- file[ankh_file, ]
ankh_file_users <- unique(ankh_file$user)
ankh_http <- str_detect(http$content, "ankh")
ankh_http <- http[ankh_http, ]
ankh_http_users <- unique(ankh_http$user)

fired <- str_detect(email$content, "fired")
fired <- email[fired, ]
