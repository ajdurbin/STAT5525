options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv")
usb <- read_csv("../raw/device_info.csv")
usb_users <- unique(file$user) 
# cur <- read_csv("../raw/LDAP/2011-05.csv")
# 1 - sum((unique(usb$user) %in% cur$user_id)) / length(unique(usb$user)) 
# attrition rate of 33%, much better for classification
# 
# usr <- sample(unique(usb$user), 1)
# usr_usb <- usb %>% 
#     filter(user == usr)
# usr_file <- file %>%
#     filter(user == usr)


# check if usb users use multiple machines with usb -----------------------

# big_data <- read_csv("../data/big_data.csv", na = "none")
# big_data <- big_data %>% 
#     mutate(email = ifelse(email == "NA", "none", email)) %>% 
#     mutate(web = ifelse(web == "NA", "none", web)) %>% 
#     mutate(activity = ifelse(activity == "NA", "none", activity)) %>% 
#     mutate(attachments = ifelse(attachments == "NA", "none",
#                                 attachments)) %>% 
#     mutate(size = ifelse(size == "NA", "none", size))
# str(big_data)
# usb_traffic <- big_data %>% 
#     filter(usb != "none")
# device <- read_csv("../raw/device_info.csv")
# usb_users <- unique(device$user)
# usb_users_pc_count <- rep(0,length(usb_users))
# for(i in 1:length(usb_users)){
#     
#     usr <- usb_users[i]
#     
#     tmp <- device %>% 
#         filter(user == usr) %>% 
#         select(pc) %>% 
#         unique() %>% 
#         nrow()
#     
#     usb_users_pc_count[i] <- tmp 
#     
# }
# max(usb_users_pc_count)
# # several usb users with 100s of different pc usb connections
# # are they it admins?
# emp <- list.files(path = "../raw/LDAP/", pattern = "*.csv", 
#                   full.names = TRUE)
# emp <- do.call(rbind, lapply(emp, read_csv)) 
# test_usrs <- usb_users[usb_users_pc_count > 1]
# test_usr <- test_usrs[16]
# tmp <- emp %>% 
#     filter(user_id == test_usr)
# # all are in electronic security
# # do they have weird download patterns?
# tmp <- file %>% 
#     filter(user %in% test_usrs)


# wordcloud analysis ------------------------------------------------------

._ <- c("tm", "wordcloud", "animation")
lapply(._, library, character.only = TRUE)
# test_usr <- sample(usb_users, 1)
# usr_data <- file %>%
#     filter(user == test_usr)
# corpus <- Corpus(VectorSource(usr_data$content))
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus,
#                  function(x) removeWords(x, stopwords()))
# corpus <- tm_map(corpus, removeNumbers)
# # corpus <- tm_map(corpus, PlainTextDocument)
# col = brewer.pal(6, "Dark2")
# wordcloud(corpus, min.freq = 30, scale = c(4,2), rot.per = 0.25,
#           random.color = T, max.word = 20, random.order = F, colors = col)
# 
# layout(matrix(c(1, 2), nrow=2), heights=c(0.25, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, paste0(test_usr, " USB Download WordCloud"))
# wordcloud(corpus, main="Alex", random.color = TRUE, random.order = FALSE,
#           colors = brewer.pal(6, "Dark2"), max.words = 30)

# wordcloud gif -----------------------------------------------------------

saveGIF({
    for(usr in usb_users){
        
        usr_data <- file %>% 
            filter(user == usr)
        corpus <- Corpus(VectorSource(usr_data$content))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus,
                         function(x) removeWords(x, stopwords()))
        corpus <- tm_map(corpus, removeNumbers)
        layout(matrix(c(1, 2), nrow=2), heights=c(0.25, 4))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.5, paste0(test_usr, " USB Download WordCloud"))
        wordcloud(corpus, main="Alex", random.color = TRUE, random.order = FALSE,
                  colors = brewer.pal(6, "Dark2"), max.words = 30)
    }
}, movie.name = "usb_wc.gif", interval = 1.5, max = 10000)
