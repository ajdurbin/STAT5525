rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "chron", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels", "parallel")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv")
device <- read_csv("../raw/device_info.csv")
psych <- read_csv("../raw/psychometric_info.csv")
cur <- read_csv("../raw/LDAP/2011-05.csv")
first <- read_csv("../raw/LDAP/2009-12.csv")
logon <- read_csv("../raw/logon_info.csv")

# supervisor dictionary
sup <- unique(cur$supervisor)
sup <- cur %>%
    filter(employee_name %in% sup) %>% 
    select(employee_name, user_id, email)

## test local before remote run
# email <- read_csv("../raw/email_info.csv")
# f <- function(x, pos){
#     return(x[, c(2, 3, 4, 5)])
# }
# http <- read_csv_chunked("../raw/http_info,csv", DataFrameCallback$new(f), 
#                          chunk_size = 100000)

users <- unique(logon$user)
# president has no supervisor
# users <- users[users != "MSS001"]

# aggregate summary information
# combo <- function(usr, cur = cur, file = file, first = first, device = device, 
#                   psych = psych, logon = logon, email = email, http = http,
#                   sup = sup) {
combo <- function(usr, cur = cur, first = first, file = file, device = device, psych = psych,
                  logon = logon, sup = sup) {
    
    # file downloads
        
    usr_file <- file %>%
        filter(user == usr) 
    if (nrow(usr_file) != 0) {
        usr_file <- usr_file %>% 
        mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"),
               time = hms::as.hms(date), 
               day = as.Date(date), hour = chron::hours(date)) %>% 
        select(-date)
        mutate(extension = stringr::str_extract(filename, "[^.]*$")) 
    }
        
    
    # usb connects
    usr_device <- device %>%
        filter(user == usr)
    if (nrow(usr_device) != 0){
        usr_device <- usr_device %>%
        mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
               day = as.Date(date), hour = chron::hours(date)) %>% 
        select(-date)
    }

    # logon
    usr_logon <- logon %>%
        filter(user == usr) %>%
        mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
               day = as.Date(date), hour = chron::hours(date)) %>% 
       select(-date) 

    after_hour_logon <- usr_logon %>%
        filter(activity == "Logon") %>%
        filter(hour > 0 & hour < 5)

    # supervisor information
    supervisor <- first %>%
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
    usr_first <- first %>%
        filter(user_id == usr)

    # # email
    # usr_email <- email %>%
    #     filter(user == usr) %>%
    #     mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
    #            day = as.Date(date), hour = chron::hours(date)) %>% 
    #     select(-date)
    # 
    # # web
    # usr_http <- http %>%
    #     filter(user == usr) %>%
    #     mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
    #            day = as.Date(date), hour = chron::hours(date)) %>% 
    #     select(-date)
    
    row <- data.frame(employee_name = usr_first$employee_name,
                      employee_id = usr,
                      employee_email = usr_first$email,
                      employee_role = usr_first$role,
                      # supervisor information
                      supervisor_name = supervisor_info$employee_name,
                      supervisor_id = supervisor_info$user_id,
                      supervisor_email = supervisor_info$email,
                      # attrition
                      attrition = usr %in% cur$user_id,
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
                      N = usr_psych$N)
    
    return(row)
    
}

# # multi-user test
# pckg <- lapply(users, function(g) combo(usr = g, cur = cur, first = first,
#                                         file = file, device = device,
#                                         psych = psych, logon = logon,
#                                         sup = sup))
# # single user test
# test <- combo(usr = sample(users, 1), cur = cur, first = first, file = file,
#               device = device, psych = psych,
#               logon = logon, sup = sup)

num_cores <- detectCores() - 2
cl <- makeCluster(num_cores, type = "FORK")
clusterExport(cl = cl, varlist = ls())
pckg <- parLapply(cl = cl, users, function(g) combo(usr = g, cur = cur,
                                                    first = first, file = file,
                                        device = device, psych = psych,
                                        logon = logon, sup = sup))
stopCluster(cl = cl)

# unpack combo result into dataframe
unpack <- function(my_list) {
    
   pckg <- my_list[[1]]
   for (i in 2:length(my_list)) {
      tmp <- my_list[[i]] 
      pckg <- plyr::rbind.fill(pckg, tmp)
   }
   
   return(pckg)
    
}

pckg <- unpack(pckg)
