# k-means store within cluster sum of squares for best objects
# do for k=2:30, 1000 iterations each
rm(list = ls())
raw <- read.table("ClusterSet1.txt")
best_fit <- list("1" <- NULL)
for(i in 2:30){
  fit <- kmeans(raw, centers = i)
  for(j in 1:1000){
    tmp <- kmeans(raw, centers = i)
    if(tmp$tot.withinss < fit$tot.withinss){
      fit <- tmp
    }
  }
  tmp <- as.character(i)
  best_fit[[tmp]] <- fit
}

# collect tot.withinss
sto <- rep(0, 30)
for(i in 2:length(best_fit)){
  tmp <- best_fit[[i]]
  sto[i] <- tmp$tot.withinss
}

ggplot() +
  geom_jitter(mapping = aes(x = 2:30, y = sto[2:length(sto)])) +
  ggtitle("Scree Plot For k-Means Clustering") +
  xlab("Number Of Clusters") +
  ylab("Total Within Clusters Sum Of Squares") +
  ylim(min(sto), max(sto))
