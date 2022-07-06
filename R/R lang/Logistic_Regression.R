install.packages('car')
install.packages('DMwR')
install.packages('lattice')
install.packages('grid')
install.packages('ROSE')
library(ROSE)
library(DMwR)
library(caTools)
library(corrplot)
library(ggplot2)
library(car)
library(MASS)

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
table(c4_1$Churn)

## Min Max scaling
c5=c4
c4$VMail.Plan=NULL
c4$Intl.Plan=NULL
c4$Churn = NULL
scale_function = function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}
for (i in 1:ncol(c4)) {
  c4[,i]=scale_function(c4[,i])
}

## G-score scaling
scale_function = function(x)
{
  return (scale(x))
}
for (i in 1:ncol(c4)) {
  c4[,i]=scale_function(c4[,i])
}


str(c4)
c4$VMail.Plan=c5$VMail.Plan
c4$Intl.Plan=c5$Intl.Plan
c4$Churn = c5$Churn
str(c4)

c6=c4
c6$Churn = NULL



set.seed(123)
split=sample.split(c4$Churn, SplitRatio = 0.75) # sample.split takes the target variable and splits the data on the basis of it
#it will take the entire data set and put 75% rows= TRUE and 25% rows = False randomely. It doesn't depend on the value of target variable

training = subset(c4, split==TRUE)
table(training$Churn)
test = subset(c4, split==FALSE)
table(test$Churn)
View(training)

classifier = glm(formula = Churn ~ . , family = binomial, data= training)

summary(classifier)

test1 = test
test1$Churn=NULL
pred= predict(classifier, type = 'response', newdata = test1)

pred
cm= table(test$Churn,pred>0.25)
cm

r = 110/(132+51)
r
r1 = 149/(149+97)
r1
accuracy = (114+537)/(537+51+132+114)
accuracy
accuracy1 = (149+490)/834
accuracy1
