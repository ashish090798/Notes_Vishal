library(rpart)				  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
library("lattice")
library("ggplot2")
library(randomForest)
library(randomForest)
library(reshape2)

churn_d = read.csv('Churn_MV.csv')
View(churn_d)
str(churn_d)

c1=churn_d[!is.na(churn_d$Churn), ]
View(c1) 

# Covert the Categorical variables(Churn, State, Area.Code, Phone, Intl.Plan, Vmail.Plan) to Factor variables
str(c1)
c1$Churn= as.factor(c1$Churn)
c1$State=as.factor(c1$State)
c1$Area.Code=as.factor(c1$Area.Code)
c1$Phone=as.factor(c1$Phone)
c1$Intl.Plan=as.factor(c1$Intl.Plan)
c1$VMail.Plan=as.factor(c1$VMail.Plan)
str(c1)

c2 = c1
## Remove High Correlation

c2$Day.Mins = NULL
c2$Eve.Mins = NULL
c2$Night.Mins= NULL
c2$Intl.Mins= NULL
c2$Daily.Charges.MV=NULL

str(c2)
table(c2$Churn)



c5 = c2[,c(15,16)]
table(c5)
c5 = as.data.frame(table(c5))
str(c5)
cluster = kmeans(x=c5$Freq, centers = 7)
c5$clust = cluster$cluster
cluster$centers
c5

##withinss by betweenss

withinByBetween = c()
for(i in 2:15){
  clust = kmeans(x=c5$Freq,centers = i)
  ##betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
  withinByBetween = c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}
withinByBetween
plot(2:15,withinByBetween,type = 'l') ## total 7 clusters

?match
?merge ## Merging the column clust with original data c2 and producing new data set c3
c3 = merge(c5, c2, by.x=intersect(names(c5),names(c2)))
View(c3)
str(c3)

c3$State = NULL
c3$Phone = NULL
c3$Area.Code=NULL
c3$Freq=NULL
c3$clust=as.factor(c3$clust)

str(c3)

## balance the dataset using SMOTE sampling

c4<- ROSE(Churn ~ ., data = c3, p = 0.3)$data
##c4_1 =SMOTE(Churn ~ ., c2, perc.over = 800, perc.under = 200 )
?SMOTE
table(c4$Churn)


set.seed(123)
split=sample.split(c4$Churn, SplitRatio = 0.75) # sample.split takes the target variable and splits the data on the basis of it
#it will take the entire data set and put 75% rows= TRUE and 25% rows = False randomely. It doesn't depend on the value of target variable

training = subset(c4, split==TRUE)
table(training$Churn)
test = subset(c4, split==FALSE)
table(test$Churn)
View(training)


churntree1 = rpart(Churn ~ ., data = training, method = 'class')
fancyRpartPlot(churntree1, cex = 0.50 , tweak = 1.05)

##
### Accuracy on test 
test1 = test
test$pred = predict(churntree1, newdata = test, type = 'class')
View(pred)
table(test$Churn, test$pred)

accuracy = (546+145)/834 
accuracy ## 93.55%

precision = 145/(145+42) ## TP/(TP+FP)
precision  ## 82.85%

recall = 145/(145+101) ## TP/(TP+FN)
recall    ## 58.94%

## Random Forest 

rftree = randomForest(Churn ~ . , data = training )

pred = predict(rftree, newdata = test1, type = 'class')

table(test1$Churn, pred)

r = 149/(149+94)
r
a = (149+559)/(149+559+31+94)

## frequency split on the basis of target variable

?dcast
str(c2)
c7 = c2[,c(15,16,4)]
c7 = as.data.frame(table(c7))
colnames(c7)


c8 = dcast(c7,State+Area.Code~Churn)
View(c8)

str(c8)
c9=c8[, c(3,4)]
cluster = kmeans(x=c9, centers = 7)
cluster$cluster
c8$clust = cluster$cluster
cluster$centers
c8

str(c2)
c3 = merge(c8, c2, by.x=intersect(names(c8),names(c2)))
View(c3)
str(c3)

c3$State = NULL
c3$Phone = NULL
c3$Area.Code=NULL
c3$`0`=NULL
c3$`1`=NULL
c3$clust=as.factor(c3$clust)

str(c3)

## balance the dataset using SMOTE sampling

c4<- ROSE(Churn ~ ., data = c3, p = 0.3)$data
##c4_1 =SMOTE(Churn ~ ., c2, perc.over = 800, perc.under = 200 )
?SMOTE
table(c4$Churn)


set.seed(123)
split=sample.split(c4$Churn, SplitRatio = 0.75) # sample.split takes the target variable and splits the data on the basis of it
#it will take the entire data set and put 75% rows= TRUE and 25% rows = False randomely. It doesn't depend on the value of target variable

training = subset(c4, split==TRUE)
table(training$Churn)
test = subset(c4, split==FALSE)
table(test$Churn)
View(training)


churntree1 = rpart(Churn ~ ., data = training, method = 'class')
fancyRpartPlot(churntree1, cex = 0.50 , tweak = 1.05)

##
### Accuracy on test 
test1 = test
test$pred = predict(churntree1, newdata = test, type = 'class')
View(pred)
table(test$Churn, test$pred)

accuracy = (546+145)/834 
accuracy ## 93.55%

precision = 145/(145+42) ## TP/(TP+FP)
precision  ## 82.85%

recall = 145/(145+101) ## TP/(TP+FN)
recall    ## 58.94%

## Random Forest 

rftree = randomForest(Churn ~ . , data = training )

pred = predict(rftree, newdata = test1, type = 'class')

table(test1$Churn, pred)

r = 138/(138+97)
r ##58.72
a = (138+571)/(138+571+27+97)
a  ## 85.11

## checking the importance of input variables and taking first 10 variables and passing to logistic variable

varImpPlot(rftree)

training1=training
training1$clust=NULL
training1$Account.Length=NULL
training1$Night.Calls=NULL
training1$VMail.Plan=NULL

## logistic regression on top 10 variables
classifier = glm(formula = Churn ~ . , family = binomial, data= training1)

summary(classifier)


test1$Churn=NULL
pred= predict(classifier, type = 'response', newdata = test1)

pred
cm= table(test$Churn,pred>0.4)
cm


