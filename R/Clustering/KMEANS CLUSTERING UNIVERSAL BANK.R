rm(list=ls())
data1 = read.csv("universalBank.csv")
data1 = data1[data1$Personal.Loan == 1,]
data1$Personal.Loan = NULL
data1$ZIP.Code  = NULL
data1$ID = NULL

# data1$Family = as.factor(data1$Family)
data1$Education = as.factor(data1$Education)

summary(data1)
# data1$Experience[data1$Experience <0] = 0

View(cor(data1[,-c(4,6,8,9,10,11)]))

data1$Experience = NULL # High cor with Age
# data1$CCAvg = NULL # High cor with Income Will drop this later

### Convert Cat to numeric
library(dummies)
data1_dummies = dummy.data.frame(data1)
names(data1_dummies)
data1_dummies = data1_dummies[,-c(7)]
summary(data1_dummies)
### Scaling
###Min max scaling
fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
for(i in 1:ncol(data1_dummies)){
  data1_dummies[,i] = fnScaling(data1_dummies[,i])
}
summary(data1_dummies)

### Kmeans clustering
clust =  kmeans(x=data1_dummies,centers = 6)### Only numeric data to be passed

data1$clust = clust$cluster

clust$centers
View(data1)
clust$cluster ## CLUSTER ID
clust$centers ## Centroids
clust$betweenss ## 
clust$withinss
mean(clust$withinss)/clust$betweenss ## IntraCluster/interCluster

data1_dummies$clust = NULL
summary(data1_dummies)
## Identify Best number of clusters
withinByBetween = c()
for(i in 2:15){
  clust = kmeans(x=data1,centers = i)
  ##betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
  withinByBetween = c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}
plot(2:15,withinByBetween,type = 'l')


clust =  kmeans(x=data1,centers = 6)
clust$centers
data1$cluster = clust$cluster
View(data1_dummies[data1_dummies$cluster < 3,])
table(clust$cluster)


data1_dummies$cluster = NULL

library(MASS)
data(Boston)
