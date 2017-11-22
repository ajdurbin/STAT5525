# email processing
rm(list = ls())
options(stringsAsFactors = FALSE)
._ <- c("dplyr", "readr", "plyr", "chron", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels", "parallel",
        "tidytext")
lapply(._, library, character.only = TRUE)
email <- read_csv("../data/email_small.csv")
original <- read_csv("../raw/LDAP/2009-12.csv")
users <- unique(email$user)
usr <- sample(users, 1)

# unpack list object into dataframe for email
email_unpack <- function(my_list) {
    
    name <- my_list[[1]][1]
    domain <- my_list[[1]][2]
    for (i in 2:length(my_list)) {
        tmp_name = my_list[[i]][1]
        tmp_domain = my_list[[i]][2]
        name <- c(name, tmp_name)
        domain <- c(domain, tmp_domain)
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
# get user emails
usr_email <- email %>% 
    filter(user == usr) %>% 
    mutate(date = strptime(date, "%m/%d/%Y %H:%M:%S"), time = hms::as.hms(date),
           day = as.Date(date), hour = chron::hours(date)) %>% 
    select(-date) 
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
fraud_work_emails <- 
    sum(usr_from_dictionary$name %in% employee_email_dictionary$name)
total_unique_from <- nrow(usr_from_dictionary)
total_attachments <- sum(usr_email$attachments)
# do splits and get information on to emails
usr_to_dictionary <- to_split(usr_email$to)
usr_to_dictionary <- to_unpack(usr_to_dictionary)
usr_to_dictionary <- str_split(usr_to_dictionary, "@")
usr_to_dictionary <- email_unpack(usr_to_dictionary)
usr_to_dictionary <- unique(usr_to_dictionary)
# now do same for cc
usr_cc_dictionary <- to_split(usr_email$cc)
usr_cc_dictionary <- to_unpack(usr_cc_dictionary)
usr_cc_dictionary <- str_split(usr_cc_dictionary, "@")
usr_cc_dictionary <- email_unpack(usr_cc_dictionary)
usr_cc_dictionary <- unique(usr_cc_dictionary)
# now do same for bcc
usr_bcc_dictionary <- to_split(usr_email$bcc)
usr_bcc_dictionary <- to_unpack(usr_bcc_dictionary)
if (length(usr_bcc_dictionary) != 0) {
    usr_bcc_dictionary <- str_split(usr_bcc_dictionary, "@")
    usr_bcc_dictionary <- email_unpack(usr_bcc_dictionary)
    usr_bcc_dictionary <- unique(usr_bcc_dictionary)
}
pckg <- list(fraud_work_emails = fraud_work_emails,
             total_emaill = nrow(usr_email),
             total_unique_from = total_unique_from,
             total_unique_to = nrow(usr_to_dictionary),
             total_unique_cc = nrow(usr_cc_dictionary),
             total_unique_bcc = nrow(usr_bcc_dictionary),
             total_attachments = total_attachments,
             total_non_work = sum(usr_to_dictionary$domain != "dtaa.com"),
             total_after_hours = sum(usr_email$hour < 5 & usr_email$hour > 0))



# summaries ---------------------------------------------------------------





# tf_idf ------------------------------------------------------------------


doc_corpus <- Corpus(VectorSource(usr_email$content[1:10]))
control_list <- list(removePunctuation = TRUE, removeNumbers = TRUE,
                     stopwords = TRUE, tolower = TRUE,
                     stemming = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)
tf <- as.matrix(tdm)
idf <- log(ncol(tf) / (1 + rowSums(tf != 0)))
idf <- diag(idf)
tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
tf_idf <- tf_idf / sqrt(rowSums(tf_idf^2))
sum(tf_idf > 0)
max(tf_idf)


