## Anova and linear regression
temp = c(24,26,31,27, 29,31,30,36,33,29,27,34,26)
city = c(rep("A",4), rep("B",5),rep("C",4))

df = data.frame(temp, city)
df
mean(temp)

aggregate(temp ~ city, data = df, FUN=mean)

## analysis of variance 

a = aov(temp ~ city, data = df)
summary(a)


## linear regression 
setwd("D:/AP/linear")

ins = read.csv("insurance.csv")
head(ins)
## EDA 
cor(ins$age, ins$charges)

## between gender and charges 

aggregate(charges ~ sex , data = ins, FUN = mean)

## Box plot 

library(ggplot2)

ggplot(ins, aes(sex, charges)) + geom_boxplot()

## input variables are independent from each other 

cor(ins$age, ins$bmi)

### Target variable normal distribution 

hist(ins$charges)

hist(log(ins$charges))
hist(sqrt(ins$charges))

ins$charges = log(ins$charges)
##  split the data into train and test sets 
hist(ins$charges)
set.seed(234)

ids = sample(nrow(ins), nrow(ins)*0.8)

train = ins[ids,]
test = ins[-ids,]


### model 
model1 = lm( charges ~ . , data = train)
summary(model1)
## model testing 
test$pred = predict(model1, newdata = test)
## model performance 
test$err = test$charges - test$pred
mean(test$err**2)
sqrt(mean(test$err**2))
## mape 
mean((abs(test$err)/test$charges)*100)

## model diagnostics 
