ins=read.csv("insurance.csv")
head(ins)
##Checking which variable are impacting target data using EDA

cor(ins$age,ins$charges)

## calculate mean of charges by sex in ins df

aggregate(charges~sex,data=ins,FUN=mean)


##checking whether sex is impacting target value charge using a box plot


library(ggplot2)
ggplot(ins,aes(sex,charges)) +geom_boxplot()

##output-gender has strong impact on charges

#checking input variables are independent or not using correlate 

cor(ins$age,ins$bmi)

###checking target variable is normal distributed or not.It show be a normal distribution. Normal distribution is checked on target variable only


hist(ins$charges)

##converting into normal distribution using log

hist(log(ins$charges))

##converting into normal distribution using sq

hist(sqrt(ins$charges))


##making target variable become normal distributed using log
ins$charges=log(ins$charges)


#split data into train and test sets
##it is imp to use set.seed(num x) as it fixes sample function to generate same values every user request to generate.

set.seed(234)
##sample(1:10,8)
##sample(10,5)
ids=sample(nrow(ins),nrow(ins)*0.8)

train=ins[ids,]
test=ins[-ids,]


##model making
##all except charges are input variables and charge is target variable
model1=lm( charges~ . , data=train)
##model=lm(charges~ age+bmi, data=train)
##output- age and bmi are input variables
##model=lm(charges ~ .-bmi, data=train)
##output- all variable except bmi are input variable

summary(model1)

##Coefficients:
##Estimate Std. Error t value Pr(>|t|)    
##(Intercept)      7.0914873  0.0828357  85.609  < 2e-16 ***
  ##age              0.0353696  0.0009818  36.027  < 2e-16 ***
  ##sexmale         -0.0981338  0.0275070  -3.568 0.000376 ***
  ##bmi              0.0106897  0.0023849   4.482 8.19e-06 ***
  ##children         0.0974565  0.0113732   8.569  < 2e-16 ***
  ##smokeryes        1.5585397  0.0340354  45.792  < 2e-16 ***
  ##regionnorthwest -0.0538758  0.0394222  -1.367 0.172029    
##regionsoutheast -0.1529883  0.0388483  -3.938 8.75e-05 ***
##  regionsouthwest -0.1188427  0.0388898  -3.056 0.002300 ** 
  ##output--Y=Estimate*variables
##Pr(>|t|)-P value which determines hypothesis## *-means this variable can be dropped from model
## ***-means this variable cant be dropped.

##R2 is .7722 which means model is decent. the more the values of r2, the better is the model

##model testing

test$pred=predict(model1,newdata = test)
View(test)


##Acess performace using RMSC(Root mean sqaure error)
##In rmsc we compare predicted values and expected values.
##The less the RMSC, the better is the model
test$err=test$charges-test$pred
mean(test$err**2)
sqrt(mean(test$err**2))


##MAPE(Mean absolute percentage erro)
#The lesser the MAPE,the better is the 
##If it is below 5%, it is good model.If less than 10% and greater than 5%, model needs improvement
mean((abs(test$err)/test$charges)*100)

#Linear regression is explaining variation in y because of variation in x
##The more the value of r2, the better is the model.the more closer it is to 1, the better is the model
##It give what amount of variation in y that results because of the variatiomodeln in x.
##If it is 0.6 and above then then it good to consider this model else not.
##if p is less than 0.05, then we go with null hypothesis, if greater than or equal 0.95, then we go with alternate hypothesis.
##if p is less than 0.05 for each input variable, we keep that input variable but if it is greater than 0.05 then it back to the start and doesnt consider it as a input variable impacting the target variable.
##We perform t test for each input variable
##A leverage obs is one which is having extreme value of y for a value of x.
##If cook's distance is greater than cut off distance we remove those leverage observations.
##Variance inflation factor-if it is less or equal to 2-ignroable 
##2-5-ignorable
##5-10-we cant use both variables
##Greater than 10-you cant use both of these two variables
##Multicolearity is dependency between two input variables.To find it, run cor among the variables. 
##If it is greater than 10, you cant use both of these two variables
##Heteroscadacity is ignorable but not auto coorelation
##Outlier detection test is used to identify outliers having impact on model. 
##for multicollearity we have VIP and COR

##linear relationship

plot(ins$age,ins$charges)
cor(ins$age,ins$charges)

##multicollearity

names(ins)
vis(lm(bmi~age+children, data=train))

##since vis is near to 1, multicolleanrity is not  a prob

##constant variance of error types and autocorrealation

std.res=stdres(model1)
pred=model1$fitted.values
plot(pred,std.res)

##Normality of erros
##qqplot
plot(model1,which=2)

##outlier test
##it will only display outliers having strong impact on target. 
outlierTest(model1)

##high leverage obs

plot(model1,which=4)

##autocorrelation
plot(model1,which=1)


