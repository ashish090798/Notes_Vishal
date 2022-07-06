
install.packages('corrplot')
library(corrplot)
# data set = Churn_MV
# Target variable= Churn

d1=read.csv('Churn_MV.csv')
View(d1)
str(d1)


# 6 variables should be factors: churn, state, area code, phone, churn, Intl Plan, VMail plan. 3333 rows are complete NA, need to remove those

d1$Churn= as.factor(d1$Churn)
d1$State=as.factor(d1$State)
d1$Area.Code=as.factor(d1$Area.Code)
d1$Phone=as.factor(d1$Phone)
d1$Intl.Plan=as.factor(d1$Intl.Plan)
d1$VMail.Plan=as.factor(d1$VMail.Plan)

summary(d1)

# here 3333 rows are null , so we need to remove them. But which column to be used as a filter condition to remove the 'NA'. 
# Churn is target variable, so we should pick the variable churn to filter our data. Data is of no use where Churn is null
#1st way
d2=d1[!is.na(d1$Churn), ]

View(d2)
summary(d2)

#2nd way
d3=d1[seq(0, nrow(d1), 2), ]
View(d3)

# Treat the 50 NA values in Daily.Charges.MV column. 
# create 2 separate columns mean and median of Daily.Charges.MV
d2$Daily.Charges.MV.Mean= d2$Daily.Charges.MV
d2$Daily.Charges.MV.Median = d2$Daily.Charges.MV

d2[is.na(d2$Daily.Charges.MV.Mean),'Daily.Charges.MV.Mean'] = mean(d2$Daily.Charges.MV, na.rm = T)

d2[is.na(d2$Daily.Charges.MV.Median),'Daily.Charges.MV.Median'] = median(d2$Daily.Charges.MV, na.rm = T)
View(d2)


# Now which one to assign to nA values, mean or median ?
# there is one more column Day.Charge, which is exactly same as Daily.Charges.MV, without NA values. 
# column Day.Charge is the exact original column
# we will try to subtract the original column(Day.Charge) from the mean and median column 
d3= d2[is.na(d2$Daily.Charges.MV), ]
View(d3)
d4= data.frame(d3$Day.Charge, d3$Daily.Charges.MV, d3$Daily.Charges.MV.Mean, d3$Daily.Charges.MV.Median)
View(d4)
d4$mean_diff= d4$d3.Daily.Charges.MV.Mean- d4$d3.Day.Charge
d4$med_diff= d4$d3.Daily.Charges.MV.Median- d4$d3.Day.Charge
View(d4)
###################################################################################################################################################
# Outlier treatment
View(d2)
summary(d2)
boxplot(d2$Day.Charge)
boxplot.stats(d2$Day.Charge)
outlier_values <- boxplot.stats(d2$Day.Charge)$out
outlier_values

range(outlier_values)

x <- d2$Day.Charge

qnt <- quantile(x, probs=c(.01,.02,.03,.04,.05,.1,.15,.20,.25,
                           .30,.35,.40,.45,.50,
                           .55,.60,.65,.70,.75,
                           .80,.85,.90,.95,.98,.99,1), na.rm = T)

qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
IQR(x, na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
caps
H

x[x < (qnt[9] - H)]  <- caps[1]
x[x > (qnt[19] + H)]  <- caps[2]


boxplot(x)
x
d2$Day.Charge=x

d2$Day.Charge

####################################################################################################################################################
#Correlation and scaling

str(d2)

#for(i in range 1:)
#d5=d2[,is.numeric(d2$colname)]

d5=data.frame(d2$Churn,d2$Intl.Plan, d2$VMail.Plan ,d2$State,d2$Area.Code, d2$Phone )

View(d5)
d6= d2
d6$Churn=NULL
d6$Intl.Plan=NULL
d6$VMail.Plan =NULL
d6$State=NULL
d6$Area.Code=NULL
d6$Phone =NULL

str(d6)

M=cor(d6)
M
corrplot(M,method = "number")
?corrplot

####################################### Next part is done  again in another file 2_Linear_Regression_Model_1 more clearly

  ############################################################################################
 #########################  *****  Linear Regression  *****  ##################################
################################################################################################
####                                                                                        ####
####                     Target Variable= Daily.Charges.MV                                  ####
####                                                                                        ####
####                                                                                        ####
################################################################################################
 ##############################################################################################
  ############################################################################################

install.packages('corrplot')
library(corrplot)
# data set = Churn_MV
# Target variable= Churn

d1=read.csv('Churn_MV.csv')
View(d1)
str(d1)


# 6 variables should be factors: churn, state, area code, phone, churn, Intl Plan, VMail plan. 3333 rows are complete NA, need to remove those

d1$Churn= as.factor(d1$Churn)
d1$State=as.factor(d1$State)
d1$Area.Code=as.factor(d1$Area.Code)
d1$Phone=as.factor(d1$Phone)
d1$Intl.Plan=as.factor(d1$Intl.Plan)
d1$VMail.Plan=as.factor(d1$VMail.Plan)

summary(d1)

# here 3333 rows are null , so we need to remove them. But which column to be used as a filter condition to remove the 'NA'. 
# Churn is target variable, so we should pick the variable churn to filter our data. Data is of no use where Churn is null
#1st way
d2=d1[!is.na(d1$Churn), ]

View(d2)
summary(d2)

#2nd way
d3=d1[seq(0, nrow(d1), 2), ]
View(d3)

# Treat the 50 NA values in Daily.Charges.MV column. 
# create 2 separate columns mean and median of Daily.Charges.MV
d2$Daily.Charges.MV.Mean= d2$Daily.Charges.MV
d2$Daily.Charges.MV.Median = d2$Daily.Charges.MV

d2[is.na(d2$Daily.Charges.MV.Mean),'Daily.Charges.MV.Mean'] = mean(d2$Daily.Charges.MV, na.rm = T)

d2[is.na(d2$Daily.Charges.MV.Median),'Daily.Charges.MV.Median'] = median(d2$Daily.Charges.MV, na.rm = T)
View(d2)



View(d2)
nrow(d2)
d7=d2[!is.na(d2$Daily.Charges.MV),]
nrow(d7)
d8=d2[is.na(d2$Daily.Charges.MV),]
nrow(d8)
View(d8)
###RMSE
#1. For Mean
d8$err_mean=d8$Day.Charge-d8$Daily.Charges.MV.Mean
View(d8)
rmse_mean = sqrt(mean(d8$err_mean**2)) ##RMSE for mean
#2. For Median
d8$err_median=d8$Day.Charge-d8$Daily.Charges.MV.Median
View(d8)
rmse_median = sqrt(mean(d8$err_median**2)) ##RMSE for median
rmse_mean 
rmse_median
# Linear Regression Model
View(d7)
# training data and test data
set.seed(1234)
ids = sample(nrow(d7), nrow(d7)*0.8) # this means out of all the rows randomly select 0.8*nrows
View(ids)

train= d7[ids,]
test= d7[-ids,]
View(train)

##outlier treatment on train 


View(train)
summary(train)
boxplot(train$Daily.Charges.MV)
boxplot.stats(train$Daily.Charges.MV)
outlier_values <- boxplot.stats(train$Daily.Charges.MV)$out
outlier_values

range(outlier_values)

x <- train$Daily.Charges.MV

qnt <- quantile(x, probs=c(.01,.02,.03,.04,.05,.1,.15,.20,.25,
                           .30,.35,.40,.45,.50,
                           .55,.60,.65,.70,.75,
                           .80,.85,.90,.95,.98,.99,1), na.rm = T)

qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
IQR(x, na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
caps
H

x[x < (qnt[9] - H)]  <- caps[1]
x[x > (qnt[19] + H)]  <- caps[2]


boxplot(x)
x
train$Daily.Charges.MV=x

train$Daily.Charges.MV

####################################################################################################################################################
#Correlation and scaling

str(train)

#for(i in range 1:)
#d5=d2[,is.numeric(d2$colname)]

train1=train
train1$Phone=NULL
train1$Daily.Charges.MV.Mean=NULL
train1$Daily.Charges.MV.Median=NULL
train1$Day.Charge=NULL
train1$Churn=NULL
train=train1

d9=data.frame(train$Churn,train$Intl.Plan, train$VMail.Plan ,train$State,train$Area.Code, train$Phone )

View(d9)
str(d9)
d10= train1
d10$Intl.Plan=NULL
d10$VMail.Plan =NULL
d10$State=NULL
d10$Area.Code=NULL

str(d10)

M=cor(d10)
M
corrplot(M,method = "number")
?corrplot


str(train1)
hist(train1$Daily.Charges.MV)
Model1=lm(Daily.Charges.MV ~ ., data=train1)
summary(Model1)  

# Now we see that mode is depending only on train1$Day.Mins, train1$Day.Calls, so we can remove all other variables
# 

##cor(train1$Day.Calls, train1$Daily.Charges.MV)

d11=d7
d11$Daily.Charges.MV.Median=NULL
d11$Daily.Charges.MV.Mean=NULL
d11$Day.Charge=NULL
d11$Phone=NULL
d11$Churn=NULL

Model2=lm(Daily.Charges.MV ~Day.Mins+Day.Calls+CustServ.Calls+Eve.Calls+Night.Calls
                +Intl.Charge+Intl.Plan+State+VMail.Plan, data=train)
summary(Model2)

plot(Model2, which=1)
plot(Model2, which=2)
plot(Model2, which=3)
plot(Model2, which=4)
#Predict the target variable using model 1 on the data set test

test$pred= predict(Model1, newdata = test)
View(test)

test$err_lm=test$Daily.Charges.MV-test$pred
View(test)
rmse_lm_test = sqrt(mean(test$err_lm**2))



d8$pred= predict(Model2, newdata = d8) 
#predict the target variable using model 1 on the data set d8 which consists of 50 rows with Daily.Charges.MV='NA'
View(d8)
d8$err_lm=d8$Day.Charge-d8$pred
View(data.frame(d8$Day.Charge, d8$pred))
View(d8)
rmse_lm_d8 = sqrt(mean(d8$err_lm**2))

rmse_lm_test
rmse_lm_d8


#######################################################################################################
#EDA
d1=read.csv('Churn_MV.csv')
View(d1)
d2=d1[!is.na(d1$Churn), ]
View(d2)

d3=d2[d2$Churn==1, ]
View(d3)
d20=d2[d2$Churn==1 & d2$State=='ND', ]
nrow(d20)
d21= data.frame(table(d2$State, d2$Churn))
View(d21)
ggplot(d21, aes(Var1)) + geom_bar()
ggplot(data=d2, aes(x=factor(State), fill=factor(Churn))) + geom_bar(position='dodge')

d22=d2[d2$Churn==1 & d2$State=='NJ', ]
View(d22)


d23=d2[d2$Churn==0 & d2$State=='NJ', ]
View(d23)


ggplot(data=d2, aes(x=Day.Charge, y=Intl.Calls, size=disp, col=hp)) + geom_text(aes(label=Churn))
