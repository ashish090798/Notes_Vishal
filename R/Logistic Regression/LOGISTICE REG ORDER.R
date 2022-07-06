df=read.csv("Social_Network_Ads.csv")
summary(df)
df2=df[3:5]
df2
View(df2)

#encoding target value
df2$Purchased=factor(df2$Purchased,levels=c(0,1))
View(df2)

#splitting dataset

install.packages('caTools')
library(caTools)
summary(df2)
str(df2)
set.seed(111)

View(df2)
split=sample.split(df2$Purchased,SplitRatio=0.75)
train=subset(df2, split==TRUE)
test=subset(df2,split==FALSE)

View(train)
View(test)
#feature scaling

train[-3]=scale(train[-3])
test[-3]=scale(test[-3])


View(train)
View(test)
#glm is used to fit generalized linear model
#Fitting logistic Regression to the train set
#amily=binomial means target is distributed into only 0 and 1
model=glm(formula = Purchased ~ . ,family=binomial, data=train)
#predict
pred=predict(model, type='response' , newdata=test[-3])
y_pred=ifelse(pred>0.5,1,0)
test
#evaluate using confusion matrix
cm=table(test[,3],y_pred>0.5)
cm
#model is 86% correct. 4 it predicted false but they were true and 10 it predicted false but they were true. so 14 error in total