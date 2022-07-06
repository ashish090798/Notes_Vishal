install.packages('ISLR')
install.packages('rattle')
install.packages("RColorBrewer")
install.packages("party")
install.packages("partykit")
install.packages("caret")
library(ISLR)
### Libraries to generate fancyrapart plot 
library(rpart)			  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
library("lattice")
library("ggplot2")
install.packages("randomForest")
library(randomForest)

churn_d = read.csv('Churn.csv')
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

c2 = c1  ## original data c1 with factors converted

## removing state, area code, phone, daily.charges.MV as of now 
c2$State=NULL
c2$Area.Code=NULL
c2$Phone = NULL
c2$Daily.Charges.MV = NULL

## To check the correlation, remove factor variables and run corrplot
str(c2)
c3=c2
c3$VMail.Plan=NULL
c3$Intl.Plan=NULL
c3$Churn = NULL
y=cor(c3)
corrplot(y, method='number')

## Remove High Correlation

c2$Day.Mins = NULL
c2$Eve.Mins = NULL
c2$Night.Mins= NULL
c2$Intl.Mins= NULL

str(c2)
table(c2$Churn)

str(c2)
c3=c2
c3$Intl.Plan=NULL
c3$Churn=NULL
c3$VMail.Plan=NULL

#determing no of PCA's
#c3 should be numeric
#PCA works on numeric and integer data
c3.pca=prcomp(c3,center=TRUE,scale. = TRUE)

plot(c3.pca,type='l')
print(c3.pca)
pca = preProcess(x = c3, method = 'pca', pcaComp = 7)
c3=predict(pca,c3)
c3$Intl.Plan=c2$Intl.Plan
c3$VMail.Plan=c2$VMail.Plan
c3$Churn=c2$Churn
c3$State=c1$State
c3$Area.code=c1$Area.Code


str(c3)
c4=data.frame()
c4=c3[,c(11,12)]
View(c4)
c4=as.data.frame(table(c4))

c4
cluster=kmeans(x=c4$Freq,centers = 7)
c4$clust=cluster$cluster
cluster$centers
View(c4)
#clustering

  withinByBetween = c()
for(i in 2:15){
  clust = kmeans(x=c4$Freq,centers = i)
  ##betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
  withinByBetween = c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}
  withinByBetween
  plot(2:15,withinByBetween,type='l')
##cluster=7
  
  c5=merge(c4,c3,by.x=intersect(names(c4),names(c3)))
View(c5)
c5$Area.code=NULL
c5$State=NULL
c5$Freq=NULL

nrow(c5)
  #  clustering is done when we have a column which has many levels.ex-state
#So if we want to use that var n our model , system will convert it into dummy 
#and there will be many dummy vars and model performance will go down, by doing
#clustering we have grouped them in few clusters and made use of clusters instead of dummy vars
#Performance gets better by it.


library(randomForest)
install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(c3$Churn, SplitRatio = 0.75) # sample.split takes the target variable and splits the data on the basis of it
#it will take the entire data set and put 75% rows= TRUE and 25% rows = False randomely. It doesn't depend on the value of target variable

training = subset(c3, split==TRUE)
table(training$Churn)
test = subset(c3, split==FALSE)
table(test$Churn)


churntree1 = rpart(Churn ~ ., data = c3, method = 'class')
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
r= 141/(141+105)
r ## 54.47  - for max_depth =8, mtry=4

r = 144/(144+102)
r ##58.53   ##  for max_depth =14, mtry=4

r=149/(149+97)
r ##60.56   ##  for max_depth =14, mtry=5

r=170/(170+76)
r ## 69.10  ## for default values
a = (543+170)/(543+45+170+76)

 ## 85.5%

install.packages('e1071')
library(e1071)
View(c2)
pca = preProcess(x = c2[-4], method = 'pca', pcaComp = 2)
training=predict(pca,c2)
View(training)

