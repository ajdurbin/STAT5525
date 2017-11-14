options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "plyr", "lubridate", "hms", "glmnet", "caret",
        "randomForest", "rpart", "stringr", "tm", "wordcloud",
        "animation", "ggplot2", "SnowballC", "topicmodels")
lapply(._, library, character.only = TRUE)

file <- read_csv("../raw/file_info.csv")
usb <- read_csv("../raw/device_info.csv")
usb_users <- unique(file$user) 
usr <- sample(usb_users, 1)
usr_data <- file %>%
    filter(user == usr)
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

# ._ <- c("tm", "wordcloud", "animation")
# lapply(._, library, character.only = TRUE)
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


# sentiment analysis on usb contents --------------------------------------

positives <- read_table("../sentiment/positive-words.txt", skip = 35,
                      col_names = FALSE)
positives <- as.vector(unlist(positives))
negatives <- read_table("../sentiment/negative-words.txt", skip = 35,
                       col_names = FALSE)
negatives <- as.vector(unlist(negatives))

my_format <- function(content){
    content = gsub("[[:punct:]]", "", content) # no punctuation
    content = gsub("[[:cntrl:]]", "", content) # no control
    content = gsub("[[:digit:]]", "", content) # no digits
    str_to_lower(content) # lowercase
    return(content)
}

my_score <- function(cntnt, pos_wrds, neg_wrds){
    wrds = str_split(cntnt, "\\s+")
    wrds = unlist(wrds)
    pos_score = sum(wrds %in% pos_wrds)
    neg_score = sum(wrds %in% neg_wrds)
    score = pos_score - neg_score
    return(score)
}

my_unpack <- function(my_list){
    pckg <- rep(NA, length(my_list))
    for(i in 1:length(my_list)){
        tmp <- my_list[[i]]
        pckg[i] <- tmp 
    }
    return(pckg)
    
}

test <- lapply(usr_data$content, my_format)
test <- my_unpack(test)
test <- lapply(test, function(g) my_score(g, positives, negatives))
test <- my_unpack(test)
total_score <- sum(test)

ggplot() + 
    geom_histogram(mapping = aes(x = test), binwidth = 1) +
    xlab("Sentiment Score") +
    ggtitle(paste0(usr, " USB Sentiment\nTotal Score ", total_score))
# maybe save this as gif too


# lda analysis ------------------------------------------------------------

docs <- Corpus(VectorSource(usr_data$content)) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(stemDocument)
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- usr_data$filename
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing = TRUE)
freq[ord]

out <- LDA(dtm, k = 4, method = "Gibbs",
           control = list(nstart = 4,
                          seed = list(2003, 5, 63, 765),
                          best = TRUE,
                          burnin = 4000,
                          iter = 2000,
                          thin = 500))
topics <- as.matrix(topics(out))
terms <- as.matrix(terms(out, 6))
probs <- as.data.frame(out@gamma)
rel12 <- lapply(1:nrow(dtm), function(x)
    sort(probs[x, ])[4] / sort(probs[x, ])[4-1])
rel23 <- lapply(1:nrow(dtm), function(x)
    sort(probs[x, ])[4-1] / sort(probs[x, ])[4-2])
