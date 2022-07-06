install.packages('corrplot')
library(corrplot)
library(ggplot2)
library(car)
install.packages('car')
library(MASS)
# data set = Churn_MV
############## Target variable= Churn#########

### Step 1. Read the csv File
d1=read.csv('Churn_MV.csv')
View(d1)
str(d1)

### Step 2: Data Cleaning
# there are 3333 rows where entire row data =NA. We need to remov ethose rows
# But the condition we need to give is, filter where target variable=NA
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

## For the upcoming analysis(Linear Regression), our target variable changes here. 
##Linear variable always predicts a continuous variable, So our target variable for LR model will be Daily.Chrges.MV

### Step 3: We will predict 3 models here
##1st - mean, 2nd - median , 3rd - Linear Regression and we will check which one performs better. 
## At the end we will compare thir RMSE Values

# Before that we got to know ,variable - Daily.Charges.MV is NA at 50 places. So we will filter out those 50 places and keep them
# in separate variable
str(d2)
str(d2$Churn)
sum(is.na(d2$Daily.Charges.MV))
d3= d2[!is.na(d2$Daily.Charges.MV), ]
nrow(d3)      # without NA values
d4=d2[is.na(d2$Daily.Charges.MV),]
nrow(d4)      # With NA Values


####################################          # A Mean Model              #############################################

# A.1 Calculate the mean of target variable
charges_mean=mean(d2$Daily.Charges.MV, na.rm = T)

# A.2 replace mean with missing values

d4$Daily.Charges.MV.Mean=charges_mean
View(d4)

# A.3 There is one column Day.Charge in data set which is similar to Daily.Charges.MV.
# The places where Daily.Charges.MV =NA, column Day.Charge has a value. 
# Subtract the new column (Daily.Charges.MV.Mean) from Day.Charge and calculate the RMSE

d4$err_mean=(d4$Day.Charge - d4$Daily.Charges.MV.Mean)

RMSE_Mean= sqrt(mean(d4$err_mean ** 2))
 # 12.29297

####################################          # A Median Model             #############################################

# We need to repeat the same process for Meadian Model
# B.1 Calculate the Median of target variable
charges_median=median(d2$Daily.Charges.MV, na.rm = T)
charges_median
# B.2 replace median with missing values

d4$Daily.Charges.MV.Median=charges_median
View(d4)

# B.3 Subtract the new column (Daily.Charges.MV.Median) from Day.Charge and calculate the RMSE

d4$err_median=(d4$Day.Charge - d4$Daily.Charges.MV.Median)

RMSE_Median= sqrt(mean(d4$err_median ** 2))
RMSE_Median   # 12.29825


   ###########################################################################################
 ########################  *****  Linear Regression  *****  ###################################
################################################################################################
####                                                                                        ####
####                     Target Variable= Daily.Charges.MV                                  ####
####                                                                                        ####
####                                                                                        ####
################################################################################################
 ##############################################################################################
  ###########################################################################################

# First we will consider d3 as our train data and d4 as our test data
# We can split d3 into 80:20 as train:test data, but I tried that and it doesn't predict well. It reduces performance and increses RMSE

## C.1. Data Pre processing:

#1. There is no missing value
#2. the target variable follows the normal distribution? 

hist(d3$Daily.Charges.MV)  # Yes

#3. Covert the Categorical variables(Churn, State, Area.Code, Phone, Intl.Plan, Vmail.Plan) to Factor variables
# Do this Only if this step is not done before. I guess we have perform this step already in d2, while removing missing values
str(d3)
d3$Churn= as.factor(d3$Churn)
d3$State=as.factor(d3$State)
d3$Area.Code=as.factor(d3$Area.Code)
d3$Phone=as.factor(d3$Phone)
d3$Intl.Plan=as.factor(d3$Intl.Plan)
d3$VMail.Plan=as.factor(d3$VMail.Plan)
str(d3)

# Outlier Treatment ##############################################################################################################
#4. There are few outliers in Daily.Charges.MV. We need to treat them and bring them into a range where 5%-95% values are residing

x=d3$Daily.Charges.MV
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

d3$Daily.Charges.MV=x
boxplot(d3$Daily.Charges.MV)
########################################   Outliers Treatment Done ################################################################

# Check the correlation and dependency among input variables
# For that we need to separate the categorical and continuous variable
#1. Continuous Variables

d5=d3
str(d5)
d5$Intl.Plan=NULL
d5$State= NULL
d5$VMail.Plan=NULL
d5$Churn=NULL
d5$Area.Code= NULL
d5$Phone=NULL
str(d5)  # categoricl variables removed

y=cor(d5)
corrplot(y, method = 'number')

# correlation shows that Day.Charge and Daily.Charges.MV are same columns So we can remove Day.charge
# Check the box plots of categorical variables to se their effect on target variable

# Now select only appropriate columns and make the model

train = d3
#Day.Mins+Day.Calls+CustServ.Calls+Eve.Calls+Night.Calls+Intl.Charge+Intl.Plan+State+VMail.Plan
train$Phone=NULL
train$Day.Charge= NULL
train$Account.Length= NULL
train$VMail.Message= NULL
train$Churn= NULL


Model1= lm(Daily.Charges.MV ~ ., data=train)
summary(Model1)



#predict the target variable using model 1 on the data set d4 which consists of 50 rows with Daily.Charges.MV='NA'
d4$pred= predict(Model1, newdata = d4) 
d4$err_lm=d4$Day.Charge-d4$pred
View(data.frame(d4$Day.Charge, d4$pred))

RMSE_LM_d4 = sqrt(mean(d4$err_lm**2))

RMSE_LM_d4
MAPE_LM= mean(abs(d4$err_lm)/ d4$Day.Charge)
MAPE_LM  # 1.2% error, model is performing very good

###################################################### Model2 with 80:20 train:test data

