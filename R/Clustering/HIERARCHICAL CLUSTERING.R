data("mtcars")
mtcars
# rownames(mtcars) = 1:nrow(mtcars)

summary(mtcars)

fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


for(i in 1:ncol(mtcars)){
  mtcars[,i] = fnScaling(mtcars[,i])
}
## Convert all the features from categorical to dummies if neeed
## Not done in the class

## Distance Matrix
distmat = dist(as.matrix(mtcars),method = 'euclidean')##manhattan

View(as.matrix(distmat))

## hierarchical clustering
hierclust = hclust(distmat)
hierclust$method
plot(hierclust)
install.packages('gplots')
library(gplots)
install.packages('dendextend')
library(dendextend)


clusterCut <- cutree(hierclust, k=5)#h=2.0
table(clusterCut)
mtcars$cluster = clusterCut



aggregate(mtcars,list(mtcars$cluster),mean)





####
data("USArrests")
USArrests
hc <- hclust(dist(USArrests))
plot(hc)

clusterCut = cutree(hc,k=3)
plot(hc)
