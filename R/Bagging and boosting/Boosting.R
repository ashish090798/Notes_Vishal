setwd("D:/AP/baging and  RF")
### read the churn file ( telecom churn)
df1 = read.csv("Churn.csv")

colnames(df1)

df1 = df1[ , -c(19:21)]

###BAGGING ON CHURN DATASET

mod = bagging(as.factor(Churn) ~  .  , data=train, coob=TRUE)


pred = predict( mod, newdata = test)


table(test$Churn, pred)

##### Boosting with Gradient boosting machines ####### 

library(gbm)

dim(train)
dim(test)

### to convert variables into numeric values 
### USe only with factor variables 
library(dummies)

head(credit)

credit2 = dummy.data.frame(credit)

dim(credit)
dim(credit2)

head(df1)

### convert the factor variable to INT 


table(df1$Churn)

df1$Churn = as.factor(df1$Churn)

#df1$Churn = ifelse( df1$Churn == "Yes", 1, 0)

str(df1)

set.seed(1234)

ids = sample(nrow(df1), nrow(df1)*0.8)

train = df1[ids, ]
test = df1[-ids,]

### lets call gbm 
?gbm
library(gbm)

str(train)
boost = gbm( Churn ~ ., data=train, n.trees = 100, distribution = "bernoulli",interaction.depth = 10,  n.minobsinnode = 10, shrinkage = 0.1 ) #, interaction.depth =10, n.minobsinnode = 10)
test$pred = predict( boost, newdata=test, n.trees = 100)

hist(test$pred)

test$pred_class = ifelse( test$pred > 0, 1, 0)

cor(train$Day.Mins, train$Day.Charge)

table( test$Churn, test$pred_class)

summary(boost)
#### 
library(xgboost)
?xgboost

y= train$Churn

train$Churn = NULL

xgbmodel = xgboost(data.matrix(train), label = y, nrounds = 100, objective = "binary:logistic", eta = 0.1, max_depth = 10, max_delta_step = 10)

#test_y = test$Churn

#test$Churn = NULL

pred = predict(xgbmodel , data.matrix(test))

hist(pred)

y_class = ifelse( pred >= 0.5 , 1, 0)

table(test_y, y_class)

p = 74/81
r = 74/99
