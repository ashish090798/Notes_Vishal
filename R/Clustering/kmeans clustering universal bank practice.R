df=read.csv('UniversalBank.csv')
summary(df)
View(df)
str(df)
df=df[df$Personal.Loan==1,]
df$Education = as.factor(df$Education)
summary(df)
df$Personal.Loan = NULL
df$ZIP.Code  = NULL
df$ID = NULL
View(cor(df[,-c(4,6,8,9,10,11)]))
df$Experience=NULL
install.packages('dummies')
library(dummies)
df$CCAvg=NULL
df_dummies=dummy.data.frame(df)
names(df_dummies)

fn=function(x)
            {
              return(( x-min(x))/(max(x)-min(x)))
}

            for(i in 1:ncol(df_dummies))
            {
              df_dummies[,i]=fn(df_dummies[,i])
            }
View(df_dummies)            

  clust=kmeans(x=df_dummies,centers = 10)            
df$clust=clust$cluster
clust$cluster
clust$centers

