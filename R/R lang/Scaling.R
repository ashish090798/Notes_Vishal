library(corrplot)
library(ggplot2)
library(car)
library(MASS)

# Now we have already built the model. Here we will try to minimize the RMSE, by scaling.
# different numeric variables have different scale, we will try to normalize them by scaling
# Scaling can be done by 2 types. min-max scalinag and g score

## Perform all the previous steps

d1=read.csv('Churn_MV.csv')
str(d1)

### Step 2: Data Cleaning

d2=d1[!is.na(d1$Churn), ]
View(d2)      # 3333 rows where data is present
# Covert the Categorical variables(Churn, State, Area.Code, Phone, Intl.Plan, Vmail.Plan) to Factor variables
str(d2)
d2$Churn= as.factor(d2$Churn)
d2$State=as.factor(d2$State)
d2$Area.Code=as.factor(d2$Area.Code)
d2$Phone=as.factor(d2$Phone)
d2$Intl.Plan=as.factor(d2$Intl.Plan)
d2$VMail.Plan=as.factor(d2$VMail.Plan)
str(d2)
##################################################################################################################################
#########################################              1. Min max  scaling        ################################################
##################################################################################################################################
d5=d2
# scale all the numeric variables of d5 except the target variable

d5$max_min=max(d5$Account.Length)- min(d5$Account.Length)
min(d5$Account.Length)
max(d5$Account.Length)
View(d5)
d5$max_min=NULL
d5$Account.Length= (d5$Account.Length-min(d5$Account.Length))/(max(d5$Account.Length) - min(d5$Account.Length))
d5$VMail.Message= (d5$VMail.Message-min(d5$VMail.Message))/(max(d5$VMail.Message) - min(d5$VMail.Message))
d5$Day.Mins= (d5$Day.Mins-min(d5$Day.Mins))/(max(d5$Day.Mins) - min(d5$Day.Mins))
d5$Eve.Mins= (d5$Eve.Mins-min(d5$Eve.Mins))/(max(d5$Eve.Mins) - min(d5$Eve.Mins))
d5$Night.Mins= (d5$Night.Mins-min(d5$Night.Mins))/(max(d5$Night.Mins) - min(d5$Night.Mins))
d5$Intl.Mins= (d5$Intl.Mins-min(d5$Intl.Mins))/(max(d5$Intl.Mins) - min(d5$Intl.Mins))
d5$CustServ.Calls= (d5$CustServ.Calls-min(d5$CustServ.Calls))/(max(d5$CustServ.Calls) - min(d5$CustServ.Calls))
d5$Day.Calls= (d5$Day.Calls-min(d5$Day.Calls))/(max(d5$Day.Calls) - min(d5$Day.Calls))
d5$Eve.Calls= (d5$Eve.Calls-min(d5$Eve.Calls))/(max(d5$Eve.Calls) - min(d5$Eve.Calls))
d5$Eve.Charge= (d5$Eve.Charge-min(d5$Eve.Charge))/(max(d5$Eve.Charge) - min(d5$Eve.Charge))
d5$Night.Calls= (d5$Night.Calls-min(d5$Night.Calls))/(max(d5$Night.Calls) - min(d5$Night.Calls))
d5$Night.Charge= (d5$Night.Charge-min(d5$Night.Charge))/(max(d5$Night.Charge) - min(d5$Night.Charge))
d5$Intl.Calls= (d5$Intl.Calls-min(d5$Intl.Calls))/(max(d5$Intl.Calls) - min(d5$Intl.Calls))
d5$Intl.Charge= (d5$Intl.Charge-min(d5$Intl.Charge))/(max(d5$Intl.Charge) - min(d5$Intl.Charge))


# Filter the data where Daily.Charges.MV=NA
ds1= d5[!is.na(d5$Daily.Charges.MV), ]
nrow(ds1)
ds2= d5[is.na(d5$Daily.Charges.MV), ]
nrow(ds2)

#1. There is no missing value
#2. the target variable follows the normal distribution? 

hist(ds1$Daily.Charges.MV)  # Yes


# Outlier Treatment ##############################################################################################################
#4. There are few outliers in Daily.Charges.MV. We need to treat them and bring them into a range where 5%-95% values are residing

x=ds1$Daily.Charges.MV
boxplot(x)  # outliers are there
outliers = boxplot.stats(x)$out
outliers

# calculate 25th and 75th percentile i.e. Q1 nd Q3
qnt=quantile(x, probs = c(0.25, 0.75), na.rm = T)
qnt
Q1=qnt[1]
Q3=qnt[2]
Q1;Q3  # 24.42, 36.745                          

# calculate 5th and 95th percentile 
caps=quantile(x, probs = c(0.05, 0.95), na.rm = T)
caps    # 15.370, 45.897                             


# calculate H, i.e. 1.5*IQR, using IQR function or by (IQR=Q3-Q1)
H=1.5*IQR(x, na.rm = T)
H        # 18.4875
#or H=1.5*(Q3-Q1)


# Now calculate values below lower whiskar and values above upper Whiskar  and replace these values with 5th and 95th percentile
x[x<(Q1-H)] = caps[1]  # Below lower whiskar
x[x>(Q3+H)] = caps[2]  # Above upper whiskar

# Now assign x to original variable Daily.Charges.MV and try the boxplot

ds1$Daily.Charges.MV=x
boxplot(ds1$Daily.Charges.MV)
########################################   Outliers Treatment Done ################################################################

####Split the dat ain test and train
set.seed(234)
ids= sample(nrow(ds1), 0.8*nrow(ds1))
train=ds1[ids,]
nrow(train)
test=ds1[-ids, ]
nrow(test)

## remove the unwanted column from train data set
train$Day.Charge= NULL
train$Churn= NULL
train$Phone= NULL

# Linear regression on train data

Model_s1= lm(Daily.Charges.MV ~ Day.Calls+ Day.Mins, data = train)
summary(Model_s1)


test$pred= predict(Model_s1, newdata = test)
test$err_scale= test$Daily.Charges.MV- test$pred
RMSE_LM_Scale1 = sqrt(mean(test$err_scale ** 2))
RMSE_LM_Scale1

##Que: with outlier treatment : RMSE= 1.2
##    without outlier treatment : RMSE= 0.002 why??

ds2$pred= predict(Model_s1, newdata = ds2)
ds2$err_scale1= ds2$Day.Charge- ds2$pred
RMSE_LM_Scale11 = sqrt(mean(ds2$err_scale1 ** 2))
RMSE_LM_Scale11

##Que: with outlier treatment : RMSE= 0.3
##    without outlier treatment : RMSE= 0.002 why??

##################################################################################################################################
#########################################              1. Z Score scaling       ##################################################
##################################################################################################################################
