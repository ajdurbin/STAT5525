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
# get user emails
usr_email <- email %>% 
    filter(user == usr)
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


