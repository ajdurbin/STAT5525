# sentiment analysis
options(stringsAsFactors = FALSE)
rm(list = ls())
._ <- c("dplyr", "readr", "lubridate", "hms", "stringr", "tidytext",
        "parallel", "ggplot2", "wordcloud", "reshape2", "tidyr")
lapply(._, library, character.only = TRUE)

# data ----

data("stop_words")
file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(extension = str_extract(filename, "[^.]*$")) %>% 
    mutate(after_business_hours = hour > 17 | hour < 8) %>% 
    select(-id) %>% 
    mutate(content = sub(".*? (.+)", "\\1", content))
text <- file %>%
    select(date, filename, content) %>% 
    tidytext::unnest_tokens(word, content) %>% 
    anti_join(stop_words)
text_counts <- text %>%
    inner_join(get_sentiments(lexicon = "bing"))
text_counts <- text_counts %>% 
    count(word, sentiment, sort = TRUE) %>% 
    ungroup()
# figure of words that appear most in positive/negative sentiment
text_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
# positive/negative word cloud in one
text_counts <- text %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)
# add sentiment scores to data frame
# so pass in row and it will output this whole thing
test <- file[1, ] %>%
    tidytext::unnest_tokens(word, content) %>% 
    anti_join(stop_words)
scores <- text_counts %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>% 
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# function declarations ----
score_function <- function(i, .file = file) {
    
    tmp <- .file[i, ] %>% 
        tidytext::unnest_tokens(word, content) %>% 
        anti_join(stop_words) 
    try({
        
        scores <- tmp %>%
            inner_join(get_sentiments("bing")) %>% 
            count(sentiment) %>% 
            spread(sentiment, n, fill = 0) %>% 
            mutate(sentiment = positive - negative)
        return(scores)
        
    }, silent = TRUE)
    
    scores <- data.frame(negative = 0, positive = 0, sentiment = 0)
    return(scores)
    
}

unpack <- function(my_list) {
    
    pckg <- my_list[[1]]
    for (i in 2:length(my_list)) {
        tmp <- my_list[[i]] 
        pckg <- rbind(pckg, tmp)
    }
    
    return(pckg)
    
}

# run ----

# # serial run
# indices <- 1:nrow(file)
# result <- lapply(indices, function(g) score_function(i = g))
# result <- unpack(result)
# parallel run
num_cores <- detectCores()
cl <- makeCluster(num_cores, type = "FORK")
clusterExport(cl = cl, varlist = ls())
result <- parLapply(cl = cl, indices, function(g) score_function(i = g))
stopCluster(cl = cl)
result <- unpack(result)


# using tidytext tutorial found here:
# http://tidytextmining.com/sentiment.html
file <- read_csv("../raw/file_info.csv") %>% 
    mutate(date = lubridate::mdy_hms(date)) %>% 
    mutate(day = lubridate::as_date(date)) %>% 
    mutate(weekday = lubridate::wday(day, label = TRUE, abbr = FALSE)) %>% 
    mutate(time = hms::as.hms(date)) %>% 
    mutate(hour = lubridate::hour(time)) %>% 
    mutate(extension = str_extract(filename, "[^.]*$")) %>% 
    mutate(after_business_hours = hour > 17 | hour < 8) %>% 
    select(-id) %>% 
    mutate(content = sub(".*? (.+)", "\\1", content))
text <- file %>%
    select(date, filename, content) %>% 
    tidytext::unnest_tokens(word, content) %>% 
    anti_join(stop_words)
text_counts <- text %>%
    inner_join(get_sentiments("bing")) %>% 
    count(word, sentiment, sort = TRUE)
# figure of words that appear most in positive/negative sentiment
text_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
ggsave("text_counts.png")
# positive/negative word cloud in one
text_counts <- text %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 100)
ggsave("word_cloud.png")
