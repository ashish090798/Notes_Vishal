data('mtcars')
mtcars
fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


for(i in 1:ncol(mtcars)){
  mtcars[,i] = fnScaling(mtcars[,i])
}

distmat = dist(as.matrix(mtcars),method = 'euclidean')
distmat
View(as.matrix(distmat))
hc=hclust(distmat)
hc$method
plot(hc)
clustercut=cutree(hc,k=10)
plot(clustercut)
mtcars$cluster = clusterCut
aggregate(mtcars,list(mtcars$cluster),mean)
