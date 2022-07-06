library('MASS')
data('Boston')
?Boston
summary(Boston)
str(Boston)
Boston2=Boston[,-c(2,7)]
Boston3=Boston[,c(2,7)]
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
library(corrplot)
corrplot(cor(Boston), method = 'number')


##best number of clusters using elbow graph 
withinByBetween=c()
for(i in 2:15)
{
  clust=kmeans(x=Boston3, centers=i)
  withinByBetween=c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}

plot(2:15,withinByBetween,type='b')

clust=kmeans(x=Boston3, centers=8)
clust$cluster
summary(clust)


