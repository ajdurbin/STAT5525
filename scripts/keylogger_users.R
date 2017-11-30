# keylogging users
# goal is to look at their traffic and find them somehow spreading this
# to other machines
rm(list = ls())
._ <- c("dplyr", "readr", "lubridate", "hms", "stringr", "parallel")
lapply(._, library, character.only = TRUE)

# current <- read_csv("../raw/LDAP/2011-05.csv")
# original <- read_csv("../raw/LDAP/2009-12.csv")
file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(extension = str_extract(filename, "[^.]*$")) %>% 
    mutate(keylogger = str_detect(content, "keylog")) %>% 
    mutate(virus = str_detect(content, "virus")) %>% 
    mutate(password = str_detect(content, "password")) %>% 
    mutate(hack = str_detect(content, "hack")) %>% 
    mutate(steal = str_detect(content, "steal")) %>% 
    mutate(illegal = str_detect(content, "illegal")) %>% 
    mutate(before_5 = 
               ifelse(hour < 5, TRUE, FALSE)) %>% 
    mutate(after_11 = 
               ifelse(hour > 22, TRUE, FALSE)) %>% 
    mutate(after_hour_dowload = before_5 | after_11) %>% 
    mutate(malware = str_detect(content, "malware"))
device <- read_csv("../raw/device_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(before_5 = 
               ifelse(activity == "Connect" & hour < 5, TRUE, FALSE)) %>% 
    mutate(after_11 = 
               ifelse(activity == "Connect" & hour > 22, TRUE, FALSE)) %>% 
    mutate(after_hour_connect = before_5 | after_11)
logon <- read_csv("../raw/logon_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time))
http <- read_csv("../data/http_small.csv") %>%
    mutate(date = lubridate::mdy_hms(date)) %>%
    mutate(day = lubridate::as_date(date)) %>%
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>%
    mutate(hour = lubridate::hour(time))
email <- read_csv("../data/email_small.csv") %>%
    mutate(date = lubridate::mdy_hms(date)) %>%
    mutate(day = lubridate::as_date(date)) %>%
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>%
    mutate(hour = lubridate::hour(time))
employee_summary <- read_csv("../data/employee_summary.csv")

key <- file %>%
    filter(keylogger == TRUE)
bad_users <- unique(key$user)

# user traffic
usr <- "JLM0364"
# look at emails sent to this user since they go mia with email, web 
# traffic before this happens
unique(usr_email$from)
work_email <- "Jacqueline.Latifah.Miles@dtaa.com"
personal_email <- "Jacqueline_L_Miles@sbcglobal.net"
received_emails <- email %>%
    mutate(work = str_detect(to, work_email)) %>% 
    mutate(personal = str_detect(to, personal_email)) %>% 
    filter(work == TRUE | personal == TRUE)
first <- "jacqueline"
last <- "miles"
included_emails <- email %>%
    mutate(first = str_detect(content, first)) %>% 
    mutate(last = str_detect(content, last)) %>% 
    filter(first == TRUE)
# hard to decipher emails with her name in them
# but they stop near the time of her leaving
# she is not recepient of any of these emails either
# hard to know what they mean
when <- key %>% 
    filter(user == usr)
usr_file <- file %>%
    filter(user == usr)
usr_device <- device %>%
    filter(user == usr)
usr_web <- http %>% 
    filter(user == usr)
usr_email <- email %>% 
    filter(user == usr)
# they have no web traffic for like 3 months basically before this
# only has 2 total usb connections
# they downloaded file and then offloaded it on someone elses machine
pc_after_malware <- "PC-8486"
logon_after <- logon %>% 
    filter(day >= when$day) %>% 
    filter(pc == pc_after_malware)
usb_after <- usr_device %>% 
    filter(day >= when$day)
# who uses this pc
# we see that she only connects to put the keylogger on the machine
# and then logs on to it one more time
# what about her email she got from lockheed martin or whatever?

# new user
# this user did do this first:
# they have evidence of other bad users logging onto this machine
# that malware is installed on a couple times
# need to filter emails to/from these peoples and maybe see some 
# thing interesting, what are job roles/etc
usr <- "CSC0217"
when <- key %>% 
    filter(user == usr)
usr_file <- file %>%
    filter(user == usr)
usr_device <- device %>%
    filter(user == usr)
usr_web <- http %>% 
    filter(user == usr)
usr_email <- email %>% 
    filter(user == usr)
usr_logon <- logon %>%
    filter(user == usr)
usr_info <- original %>% 
    filter(user_id == usr)

unique(usr_email$from)
work_email <- "Cathleen.Samantha.Craig@dtaa.com"
personal_email <- "Cathleen.S.Craig@netzero.com"
received_emails <- email %>%
    mutate(work = str_detect(to, work_email)) %>% 
    mutate(personal = str_detect(to, personal_email)) %>% 
    filter(work == TRUE | personal == TRUE)
first <- "cathleen"
last <- "craig"
included_emails <- email %>%
    mutate(first = str_detect(content, first)) %>% 
    mutate(last = str_detect(content, last)) %>% 
    filter(first == TRUE | last == TRUE)
# first name does not come up in emails
usb_after <- usr_device %>% 
    filter(day >= when$day)
pc_after_malware <- "PC-5866"
logon_after <- logon %>% 
    filter(day >= when$day) %>% 
    filter(pc == pc_after_malware)
# we see at least one other bad user, JTM0223 logon to this machine after
# we check all other users if they get on this machine after installation
# date

# want to look at web traffic between usb connections
usr_dayof_web <- http %>%
    filter(user == usr) %>% 
    filter(day == when$day)
usr_dayof_email <- email %>%
    filter(user == usr) %>% 
    filter(day == when$day)
# want to check previous day too
usr_daybefore_web <- http %>%
    filter(user == usr) %>% 
    filter(day == as_date("2010-06-09"))
# if you look day of, these are repeated keywords in her nasty emails
# to the supervisor, worth searching in general through all user activity
# and then we can get a bigger picture
keywords <- c("after-hours", "weekends", "irreplaceable", "appreciated",
              "graditude")
flags <- rep(FALSE, nrow(usr_email))
for (i in 1:nrow(usr_email)) {
    this_email <- usr_email[i, ]
    for (j in 1:length(keywords)) {
        this_word <- keywords[j]
        if (str_detect(this_email$content, this_word)) {
            flags[i] <- TRUE
            next
        }
    }
}
flagged_emails <- usr_email[flags, ]
# suffer picks up to many false positives
# look at emails from supervisor to get a better picture
supervisor_emails <- email %>% 
    filter(from == "Frances.Alisa.Wiggins@dtaa.com") %>% 
    filter(day >= as_date("2010-06-09") & day <= as_date("2010-06-14"))
# supervisor has couple replies to user with keywords vacation, appreciated
# hard job, talk over lunch

# look at infecte pc traffic
pc_after_malware_users <- logon %>%
    filter(pc == pc_after_malware) %>% 
    select(user) %>% 
    unique()
pc_after_malware_users <- pc_after_malware_users$user
# check these users out for roles, etc
pc_after_malware_users <- employee_summary %>% 
    filter(employee_id %in% pc_after_malware_users)


# second user
usr <- "GTD0219" 
usr_info <- employee_summary %>% 
    filter(employee_id == usr)
sup <- usr_info$supervisor_email
when <- key %>% 
    filter(user == usr)
usr_file <- file %>%
    filter(user == usr)
usr_device <- device %>%
    filter(user == usr)
usr_web <- http %>% 
    filter(user == usr)
usr_email <- email %>% 
    filter(user == usr)
usr_logon <- logon %>%
    filter(user == usr)
usr_dayof_web <- http %>%
    filter(user == usr) %>% 
    filter(day == when$day)
usr_dayof_email <- email %>%
    filter(user == usr) %>% 
    filter(day == when$day)
usr_dayof_logon <- logon %>%
    filter(user == usr) %>% 
    filter(day == when$day)
sup_dayof_email <- email %>%
    filter(from == sup) %>% 
    filter(day == when$day)











# look at some keywords that the users send their supervisors and
# same for supervisors and their constituents
key_words <- email %>% 
    mutate(word = str_detect(content, "holidays | weekends")) %>% 
    filter(word == TRUE)















# stuff for a function to process this information later ----
bad_users_logon <- rep(FALSE, length(bad_users))
bad_users_connects <- rep(0, length(bad_users))
for (i in 1:length(bad_users)) {
    u <- bad_users[i]
    if (u %in% logon_after$user) {
        bad_users_logon[i] <- TRUE
    }
    tmp <- logon_after %>% 
        filter(activity == "Logon") %>% 
        filter(user == u)
    bad_users_connects[i] <- nrow(tmp)
}
bad_users
bad_users_logon
bad_users_connects
# we see that all but one of these bad users logon to this machine
# after the malware/keylogger is installed
# now lets find these peoples roles/just get simple information on them
# write function a little later, keep looking now
# next question is do they email each other? 
# need function to process this information now:
bad_user_function <- function(user, .logon = logon, .file = file, 
                              .email = email, .http = http,
                              .device = device, .current = current,
                              .original = original, .key = key) {
    
    user_file <- .file %>%
        filter(user == user)
    user_device <- .device %>%
        filter(user == user)
    user_web <- .http %>% 
        filter(user == user)
    user_email <- .email %>% 
        filter(user == user)
    user_logon <- .logon %>%
        filter(user == user)
    user_info <- .original %>% 
        filter(user_id == user)
    when <- .key %>% 
        filter(user == user)
    when <- when$day
    
    row <- data.frame(employee_name = user_info$employee_name,
                      user_id = user_info$user_id,
                      email = user_info$email,
                      role = user_info$role,
                      business_unit = user_info$business_unit,
                      functional_unit = user_info$functional_unit,
                      department = user_info$department,
                      team = user_info$team,
                      supervisor = user_info$supervisor,
                      attrition = 
                          ifelse(user %in% current$user_id, FALSE, TRUE),
                      keylogger_download_date = when)
    return(row)
    
}

list_unpack <- function(my_list) {
    
    pckg <- my_list[[1]]
    for (i in 2:length(my_list)) {
        tmp <- my_list[[i]]
        pckg <- rbind(pckg, tmp)
    }
    return(pckg)
    
}

# serial run
result <- lapply(bad_users, function(g) bad_user_function(user = g))
result <- list_unpack(result)

# parallel run, not necessary but cool
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
clusterExport(cl = cl, ls())
result <- parLapply(cl = cl,bad_users, 
                 function(g) bad_user_function(user = g))
stopCluster(cl = cl)
result <- list_unpack(result)

