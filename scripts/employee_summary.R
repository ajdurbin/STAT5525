rm(list = ls())
# ._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "stringr",
#         "parallel")
._ <- c("dplyr", "readr", "lubridate", "hms", "stringr", "parallel")
lapply(._, library, character.only = TRUE)


# load data ---------------------------------------------------------------


file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) 
device <- read_csv("../raw/device_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) 
psych <- read_csv("../raw/psychometric_info.csv")
latest <- read_csv("../raw/LDAP/2011-05.csv")
original <- read_csv("../raw/LDAP/2009-12.csv")
logon <- read_csv("../raw/logon_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) 

# supervisor dictionary
sup <- unique(latest$supervisor)
sup <- latest %>%
    filter(employee_name %in% sup) %>% 
    select(employee_name, user_id, email)

# test local before remote run
#email <- read_csv("../data/email_small.csv")
email <- read_csv("../raw/email_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) 
# f <- function(x, pos){
#     return(x[, c(2, 3, 4, 5)])
# }
# http <- read_csv_chunked("../raw/http_info,csv", DataFrameCallback$new(f), 
#                          chunk_size = 100000)

users <- unique(logon$user)


# function declarations ---------------------------------------------------


# unpack list object into dataframe for email
email_unpack <- function(my_list) {
    
    name <- my_list[[1]][1]
    domain <- my_list[[1]][2]
    if (length(my_list) > 1) {
        for (i in 2:length(my_list)) {
            tmp_name = my_list[[i]][1]
            tmp_domain = my_list[[i]][2]
            name <- c(name, tmp_name)
            domain <- c(domain, tmp_domain)
        } 
    }
    pckg <- data.frame(name = name, domain = domain)
    
}

# split the to emails at ";"
to_split <- function(to){
    
    to <- na.omit(to)
    to <- str_split(to, ";")
    return(to)
}

# unpack the to emails to then split at "@" and then email unpack
to_unpack <- function(to) {
    
    to <- unlist(to)
    return(to)
    
}

# email pckg check
email_check <- function(pckg){
    
    for (i in 1:length(pckg)) {
        if (is.null(pckg[[i]])) {
            pckg[[i]] <- 0
        }
    }
    return(pckg)
    
}

# process user email information
email_process <- function(usr = usr, usr_email = usr_email, original = original) {
    
    # get the user's work email address
    usr_email_address <- original %>% 
        filter(user_id == usr) %>% 
        select(email) %>% 
        as.character()
    # then fitler it out from the other work emails
    other_employee <- original %>% 
        filter(user_id != usr) %>% 
        select(email)
    # split the username and domains
    employee_email_dictionary <- str_split(other_employee$email, "@")
    employee_email_dictionary <- email_unpack(employee_email_dictionary)
    # split username and domains
    usr_from_dictionary <- str_split(usr_email$from, "@")
    usr_from_dictionary <- email_unpack(usr_from_dictionary)
    # not sure if want unique ones above yet, need to be able to count these
    usr_from_dictionary <- unique(usr_from_dictionary)
    # find if they sending emails under someone else's username
    fraud_work_emails <- sum(usr_from_dictionary$name %in% employee_email_dictionary$name)
    # do splits and get information on to emails
    usr_to_dictionary <- to_split(usr_email$to)
    usr_to_dictionary <- to_unpack(usr_to_dictionary)
    usr_to_dictionary <- str_split(usr_to_dictionary, "@")
    usr_to_dictionary <- email_unpack(usr_to_dictionary)
    usr_to_dictionary <- unique(usr_to_dictionary)
    # now do same for cc
    usr_cc_dictionary <- to_split(usr_email$cc)
    usr_cc_dictionary <- to_unpack(usr_cc_dictionary)
    if (length(usr_cc_dictionary) != 0) {
        usr_cc_dictionary <- str_split(usr_cc_dictionary, "@")
        usr_cc_dictionary <- email_unpack(usr_cc_dictionary)
        usr_cc_dictionary <- unique(usr_cc_dictionary)
    }
    # now do same for bcc
    usr_bcc_dictionary <- to_split(usr_email$bcc)
    usr_bcc_dictionary <- to_unpack(usr_bcc_dictionary)
    if (length(usr_bcc_dictionary) != 0) {
        usr_bcc_dictionary <- str_split(usr_bcc_dictionary, "@")
        usr_bcc_dictionary <- email_unpack(usr_bcc_dictionary)
        usr_bcc_dictionary <- unique(usr_bcc_dictionary)
    }
    pckg <- list(fraud_work_emails = fraud_work_emails,
                 total_emails = nrow(usr_email),
                 total_unique_from = nrow(usr_from_dictionary),
                 total_unique_to = nrow(usr_to_dictionary),
                 total_unique_cc = nrow(usr_cc_dictionary),
                 total_unique_bcc = nrow(usr_bcc_dictionary),
                 total_attachments = sum(usr_email$attachments),
                 total_non_work = sum(usr_to_dictionary$domain != "dtaa.com"),
                 total_after_hours = sum(usr_email$hour < 5 & usr_email$hour > 0))
    pckg <- email_check(pckg)
    return(pckg)
    
}

# aggregate user data
# combo <- function(usr, latest = latest, file = file, original = original, device = device, 
#                   psych = psych, logon = logon, email = email, http = http,
#                   sup = sup) {
combo <- function(usr, latest = latest, original = original, file = file, device = device, psych = psych,
                  logon = logon, sup = sup, email = email) {
    
    cat("\n", usr, "\n\n")
    
    # file downloads
    usr_file <- file %>%
        filter(user == usr) 
    if (nrow(usr_file) != 0) {
        usr_file <- usr_file %>% 
            mutate(extension = stringr::str_extract(filename, "[^.]*$")) 
    }
    
    # usb connects
    usr_device <- device %>%
        filter(user == usr)
    if (nrow(usr_device) != 0) {
        
    }

    # logon
    usr_logon <- logon %>%
        filter(user == usr)
    if (nrow(usr_logon) != 0) {
        after_hour_logon <- usr_logon %>%
            filter(activity == "Logon") %>%
            filter(hour > 0 & hour < 5)
    } else {
        after_hour_logon <- data.frame()
    }
    
    # get primary machine
    primary_pc <- usr_logon %>% 
        group_by(pc) %>% 
        mutate(N = n()) %>% 
        ungroup() %>% 
        filter(N == max(N)) %>% 
        select(pc) %>% 
        unique() %>% 
        as.character()

    # supervisor information
    supervisor <- original %>%
        filter(user_id == usr) %>%
        select(supervisor) %>%
        as.character()
    if (is.na(supervisor)) {
        supervisor_info <- data.frame(employee_name = "none",
                                      user_id = "none",
                                      email = "none")
    } else {
        supervisor_info <- sup %>%
            filter(employee_name == supervisor)
    }

    # psychometrics
    usr_psych <- psych %>%
        filter(user_id == usr)

    # beginning employees
    usr_original <- original %>%
        filter(user_id == usr)

    # email
    usr_email <- email %>%
        filter(user == usr)
        
        
    if (nrow(usr_email) != 0) {
        tmp <- email_process(usr = usr, usr_email = usr_email, original = original)
    } else {
        tmp <- list(fraud_work_emails = 0,
                    total_emails = 0,
                    total_unique_from = 0,
                    total_unique_to = 0,
                    total_unique_cc = 0,
                    total_unique_bcc = 0,
                    total_attachments = 0,
                    total_non_work = 0,
                    total_after_hours = 0)
    }
    

    # # web
    # usr_http <- http %>%
    #     filter(user == usr) %>%
    #     mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
    #            day = as.Date(date), hour = chron::hours(date)) %>% 
    #     select(-date)
    
    row <- data.frame(employee_name = usr_original$employee_name,
                      employee_id = usr,
                      employee_email = usr_original$email,
                      employee_role = usr_original$role,
                      # primary pc
                      primary_pc = primary_pc,
                      # supervisor information
                      supervisor_name = supervisor_info$employee_name,
                      supervisor_id = supervisor_info$user_id,
                      supervisor_email = supervisor_info$email,
                      # attrition
                      # false means fired
                      attrition = usr %in% latest$user_id,
                      # file download information
                      total_downloads = nrow(usr_file),
                      total_after_hour_downloads =
                          sum(usr_file$hour < 5 & usr_file$hour > 0),
                      total_download_pc = length(unique(usr_file$pc)),
                      total_doc = nrow(usr_file %>% filter(extension == "doc")),
                      total_exe = nrow(usr_file %>% filter(extension == "exe")),
                      total_jpg = nrow(usr_file %>% filter(extension == "jpg")),
                      total_pdf = nrow(usr_file %>% filter(extension == "pdf")),
                      total_txt = nrow(usr_file %>% filter(extension == "txt")),
                      total_zip = nrow(usr_file %>% filter(extension == "zip")),
                      # usb connection information
                      total_connects =
                          nrow(usr_device %>% filter(activity == "Connect")),
                      total_disconnects =
                          nrow(usr_device %>% filter(activity == "Disconnect")),
                      total_usb_pc = length(unique(usr_device$pc)),
                      total_after_hour_connects =
                          sum(usr_device$hour < 5 & usr_device$hour > 0),
                      # logon information
                      total_pc_logon = length(unique(usr_logon$pc)),
                      total_logon =
                          nrow(usr_logon %>% filter(activity == "Logon")),
                      total_logoff =
                          nrow(usr_logon %>% filter(activity == "Logoff")),
                      total_after_hour_logon = nrow(after_hour_logon),
                      # psychometric information
                      O = usr_psych$O,
                      C = usr_psych$C,
                      E = usr_psych$E,
                      A = usr_psych$A,
                      N = usr_psych$N,
                      # email information
                      fraud_work_emails = tmp[["fraud_work_emails"]],
                      total_emails = tmp[["total_emails"]],
                      total_email_attachments = tmp[["total_attachments"]],
                      total_after_hour_emails = tmp[["total_after_hours"]],
                      total_non_work_emails = tmp[["total_non_work"]],
                      total_unique_from = tmp[["total_unique_from"]],
                      total_unique_to = tmp[["total_unique_to"]],
                      total_unique_cc = tmp[["total_unique_cc"]],
                      total_unique_bcc = tmp[["total_unique_bcc"]])
    
    return(row)
    
}

# unpack combo result into dataframe
combo_unpack <- function(my_list) {
    
    pckg <- my_list[[1]]
    for (i in 2:length(my_list)) {
        tmp <- my_list[[i]] 
        pckg <- plyr::rbind.fill(pckg, tmp)
    }
    
    return(pckg)
    
}


# run ---------------------------------------------------------------------

# # multi-user test
# pckg <- lapply(users, function(g) combo(usr = g, latest = latest, original = original,
#                                         file = file, device = device,
#                                         psych = psych, logon = logon,
#                                         sup = sup, email = email))
# # single user test
# test <- combo(usr = sample(users, 1), latest = latest, original = original, file = file,
#               device = device, psych = psych,
#               logon = logon, sup = sup, email = email)
# # problem user debugging
# test <- combo(usr = "MNR0829", latest = latest, original = original, file = file,
#               device = device, psych = psych,
#               logon = logon, sup = sup, email = email)
# parallel
num_cores <- detectCores()
cl <- makeCluster(num_cores, type = "FORK")
clusterExport(cl = cl, varlist = ls())
pckg <- parLapply(cl = cl, users, 
                  function(g) combo(usr = g, latest = latest, 
                                    original = original, file = file,
                                    device = device, psych = psych,
                                    logon = logon, sup = sup, 
                                    email = email))
stopCluster(cl = cl)
pckg <- combo_unpack(pckg)
write_csv(pckg, "../data/employee_summary.csv")
