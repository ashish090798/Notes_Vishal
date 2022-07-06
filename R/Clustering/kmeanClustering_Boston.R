library(MASS)
data("Boston")
Boston
?Boston
###
summary(Boston)
names(Boston2)

Boston2= Boston[,-c(2,7)]
Boston3= Boston[,c(2,7)]



fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
for(i in 1:ncol(Boston2)){
  Boston2[,i] =  fnScaling(Boston2[,i])
}
summary(Boston3)
Boston3$zn= Boston3$zn/100
Boston3$age= Boston3$age/100

Boston3 = cbind(Boston2, Boston3)
summary(Boston3)

#### Correaltion
library(corrplot)
corrplot(cor(Boston3),method = 'number')
Boston3$rad = NULL


#### Best Number of Clusters
withinByBetween = c()
for(i in 2:15){
  clust = kmeans(x=Boston3,centers = i)
  ##betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
  withinByBetween = c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}

plot(2:15,withinByBetween,type = 'l')

# 8 to 10 clusters
clust = kmeans(x=Boston3,centers = 8)
clust$centers
+++++++++++++++++++++++++++++++++++