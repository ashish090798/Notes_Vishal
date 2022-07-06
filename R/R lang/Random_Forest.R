## CART (rpart - uses gini)

install.packages('ISLR')
install.packages('rattle')
install.packages("RColorBrewer")
install.packages("party")
install.packages("partykit")
install.packages("caret")
library(ISLR)
### Libraries to generate fancyrapart plot 
library(rpart)				  # Popular decision tree algorithm
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

## balance the dataset using SMOTE sampling

c4<- ROSE(Churn ~ ., data = c2, p = 0.3)$data
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
r= 141/(141+105)
r ## 54.47  - for max_depth =8, mtry=4

r = 144/(144+102)
r ##58.53   ##  for max_depth =14, mtry=4

r=149/(149+97)
r ##60.56   ##  for max_depth =14, mtry=5

r=170/(170+76)
r ## 69.10  ## for default values
a = (543+170)/(543+45+170+76)
a ## 85.5%

install.packages('e1071')
library(e1071)
View(c2)
pca = preProcess(x = c2[-4], method = 'pca', pcaComp = 2)
training=predict(pca,c2)
View(training)

