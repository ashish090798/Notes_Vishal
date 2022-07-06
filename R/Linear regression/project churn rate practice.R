library(corrplot)
library(ggplot2)
library(car)
df1=read.csv("Churn_MV.csv")
summary(df1)
df2=df1[!is.na(df1$Churn),]
View(df2)
summary(df2)
str(df2)
unique(df2$Area.Code)
df2$Churn=as.factor(df2$Churn)
df2$Intl.Plan=as.factor(df2$Intl.Plan)
df2$VMail.Plan=as.factor(df2$VMail.Plan)
df2$Area.Code=as.factor(df2$Area.Code)
str(df2)
df2$Phone=as.factor(df2$Phone)
str(df2)
summary(df2)
df3=df2[!is.na(df2$Daily.Charges.MV),]

#model without removing outliers
set.seed(111)
ids = sample(nrow(df3), nrow(df3)*0.8)

train = df3[ids,]
test = df3[-ids,]
train$Phone=NULL
train$Day.Charge= NULL
train$Account.Length= NULL
train$VMail.Message= NULL
train$Churn= NULL
model=lm(Daily.Charges.MV ~ ., data=train)
summary(model)
test$pred=predict(model,newdata=test)
View(test)
