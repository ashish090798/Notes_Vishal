df=read.csv("UniversalBank.csv")
View(df)
df2=df[,c(10:14)]
View(df2)
install.packages("arules")
library(arules)
for(i in 1:ncol(df2))
{
  df2[,i]=ifelse(df2[,i]==0,NA,1)
  
}
df2$age=discretize(df$Age,method = 'frequency', breaks=3,labels = c(1,2,3))
df2$Personal.Loan=as.factor(df2$Personal.Loan)
df2$Securities.Account=as.factor(df2$Securities.Account)
df2$CD.Account=as.factor(df2$CD.Account)
df2$Online=as.factor(df2$Online)
df2$CreditCard=as.factor(df2$CreditCard)
str(df2)
summary(df2)

View(df2)
str(df2)
df2_trans=as(df2, 'transactions')
str(df2)
head(df2_trans)
inspect((head(df2_trans,6)))
rules=apriori(df2_trans,parameter = list(supp=0.02,confidence=0.5,target='rules',minlen=3,maxlen=5))
inspect(head(rules,20))              
-
##subsetting rules
