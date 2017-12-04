# fit all the models and make a plot of them
rm(list = ls())
options(stringsAsFactors = FALSE)

._ <- c("dplyr", "plyr", "readr", "ggplot2", "stringr", "lubridate", "hms",
        "caret", "glmnet", "rpart", "randomForest", "parallel",
        "gridExtra")
lapply(._, library, character.only = TRUE)

tenure_distribution <- read_csv("../data/tenure_distribution.csv")
fired <- tenure_distribution %>% 
    filter(attrition == 1) %>% 
    arrange(end_date)

start_date <- unique(fired$start_date)
end_dates <- unique(fired$end_date)

# function declarations ----

is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan))
}

is.inf.data.frame <- function(x){
    do.call(cbind, lapply(x, is.infinite))
}

unpack <- function(my_list){
    
    pckg <- my_list[[1]]
    for(i in 2:length(my_list)){
        tmp <- my_list[[i]]
        pckg <- rbind(pckg, tmp)
    }
    return(pckg)
}

fit_models <- function(end_date){
    
    # logon data and fit model
    logon_distribution <- read_csv(paste0("../outputs/", end_date, 
                                          "/logon_distribution.csv")) %>% 
        dplyr::rename(quick_connects_lt_1_min_logon = quick_connects_lt_1_min) %>% 
        dplyr::rename(quick_connects_lt_5_min_logon = quick_connects_lt_5_min)
    logon_distribution[is.na(logon_distribution)] <- 0
    logon_distribution[is.nan.data.frame(logon_distribution)] <- 0
    logon_distribution[is.inf.data.frame(logon_distribution)] <- 0
    raw <- logon_distribution[, -c(1, 2, 4)]
    trc <- trainControl(method = "cv", number = 10)
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                    family = binomial())  
    # paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
    ll <- 1 - logfit$results[, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
    # paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
    lc <- 1 - logfit$results[1, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                    allowParallel = TRUE)
    # paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])
    lr <- 1 - logfit$results[1, 2]
    
    # usb data and fit model 
    usb_distribution <- read_csv(paste0("../outputs/", end_date, 
                                        "/usb_distribution.csv")) %>% 
        mutate(bad_connects_pcs = 
                   ifelse(is.na(bad_connects_pcs), "", bad_connects_pcs)) %>% 
        dplyr::rename(quick_connects_lt_1_min_usb = quick_connects_lt_1_min) %>% 
        dplyr::rename(quick_connects_lt_5_min_usb = quick_connects_lt_5_min) 
    usb_distribution[is.na(usb_distribution)] <- 0
    usb_distribution[is.nan.data.frame(usb_distribution)] <- 0
    usb_distribution[is.inf.data.frame(usb_distribution)] <- 0
    raw <- usb_distribution[, -c(1, 2, 4)]
    rc <- trainControl(method = "cv", number = 10)
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                    family = binomial())  
    # paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
    ul <- 1 - logfit$results[, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
    # paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
    uc <- 1 - logfit$results[1, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                    allowParallel = TRUE)
    # paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])
    ur <- 1 - logfit$results[1, 2]
    
    # web data and fit model
    web_distribution <- read_csv(paste0("../outputs/", end_date, 
                                        "/web_distribution.csv"))
    web_distribution[is.na(web_distribution)] <- 0
    web_distribution[is.nan.data.frame(web_distribution)] <- 0
    web_distribution[is.inf.data.frame(web_distribution)] <- 0
    raw <- web_distribution[, -c(1,2,8)]
    rc <- trainControl(method = "cv", number = 10)
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "glm",
                    family = binomial())  
    # paste0("LASSO overall error rate: ", 1 - logfit$results[, 2])
    wl <- 1 - logfit$results[, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rpart")
    # paste0("CART overall error rate: ", 1 - logfit$results[1, 2])
    wc <- 1 - logfit$results[1, 2]
    logfit <- train(factor(attrition) ~ ., data = raw, trControl = trc, method = "rf",
                    allowParallel = TRUE)
    # paste0("Random Forest overall error rate: ", 1 - logfit$results[1, 2])
    wr <- 1 - logfit$results[1, 2]
    
    
    
    # format and return
    row <- data.frame(date = end_date,
                      logon_lasso = ll,
                      logon_cart = lc,
                      logon_random_forest = lr,
                      usb_lasso = ul,
                      usb_cart = uc, 
                      usb_random_forest = ur,
                      web_lasso = wl,
                      web_cart = wc, 
                      web_random_forest = wr)
    return(row)
    
}


# parallel run ----
num_cores <- detectCores()
cl <- makeCluster(num_cores, type = "FORK")
clusterExport(cl = cl, varlist = ls())
results <- parLapply(cl = cl, end_dates, function(g) fit_models(end_date = g))
stopCluster(cl = cl)
results <- unpack(results)
write_csv(results, "../data/fits.csv")

# figures ----

logon_fits <- results[, 2:4]
colnames(logon_fits) <- c("Logistic LASSO", "CART", "Random Forest")
logon_fits <- stack(logon_fits)
l <- ggplot(data = logon_fits, mapping = aes(x = ind, y = values)) +
    geom_boxplot() +
    xlab("Error Rates") +
    ylab("Method") +
    ylim(0, 0.10) +
    ggtitle("Error Rates For Logon/Logoff Distribution Model Fits") +
    coord_flip()


usb_fits <- results[, 5:7]
colnames(usb_fits) <- c("Logistic LASSO", "CART", "Random Forest")
usb_fits <- stack(usb_fits)
u <- ggplot(data = usb_fits, mapping = aes(x = ind, y = values)) +
    geom_boxplot() +
    xlab("Error Rates") +
    ylab("Method") +
    ylim(0, 0.10) +
    ggtitle("Error Rates For USB Distribution Model Fits") +
    coord_flip()


web_fits <- results[, 8:10]
colnames(web_fits) <- c("Logistic LASSO", "CART", "Random Forest")
web_fits <- stack(web_fits)
w <- ggplot(data = web_fits, mapping = aes(x = ind, y = values)) +
    geom_boxplot() +
    xlab("Error Rates") +
    ylab("Method") +
    ylim(0, 0.10) +
    ggtitle("Error Rates For HTTP Distribution Model Fits") +
    coord_flip()

grid.arrange(l, u, w)

# now get a line plot by time
l <- ggplot(data = results) +
    geom_line(mapping = aes(x = date, y = logon_lasso, color = "LASSO")) +
    geom_line(mapping = aes(x = date, y = logon_cart, color = "CART")) +
    geom_line(mapping = aes(x = date, y = logon_random_forest, color = "Random Forest")) +
    scale_color_manual("Method", values = c("red", "blue", "green")) +
    ylim(0, 0.1) +
    ylab("Error Rates") +
    xlab("Attrition Dates") +
    ggtitle("Error Rates Of Logon Distribution Model Fits\n
            Through Employee Attrition Dates")
u <- ggplot(data = results) +
    geom_line(mapping = aes(x = date, y = usb_lasso, color = "LASSO")) +
    geom_line(mapping = aes(x = date, y = usb_cart, color = "CART")) +
    geom_line(mapping = aes(x = date, y = usb_random_forest, color = "Random Forest")) +
    scale_color_manual("Method", values = c("red", "blue", "green")) +
    ylim(0, 0.1) +
    ylab("Error Rates") +
    xlab("Attrition Dates") +
    ggtitle("Error Rates Of USB Distribution Model Fits\n
            Through Employee Attrition Dates")
w <- ggplot(data = results) +
    geom_line(mapping = aes(x = date, y = web_lasso, color = "LASSO")) +
    geom_line(mapping = aes(x = date, y = web_cart, color = "CART")) +
    geom_line(mapping = aes(x = date, y = web_random_forest, color = "Random Forest")) +
    scale_color_manual("Method", values = c("red", "blue", "green")) +
    ylim(0, 0.1) +
    ylab("Error Rates") +
    xlab("Attrition Dates") +
    ggtitle("Error Rates Of HTTP Distribution Model Fits\n
            Through Employee Attrition Dates")

grid.arrange(l, u, w)
