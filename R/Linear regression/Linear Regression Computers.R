library(Ecdat)
install.packages('Ecdat')
library(Ecdat)
df=Computers
summary(df)
hist(df$price)
hist(log(df$price))
df$price=log(df$price)
boxplot(df$price)
install.packages('ggplot2')
library(ggplot2)
#remove na if any or replace them
#comparing price and cd using ggplot as cd is string
#boxplot shares relationship between 2 variables
#if they are diff, we will take take them as input variables, else we will reject
#cor for continous variable and ggplot for non continous
ggplot(df,aes(cd,price))+geom_boxplot()
ggplot(df,aes(multi,price))+geom_boxplot()
ggplot(df,aes(premium,price))+geom_boxplot()
df$screen=as.factor(df$screen)
ggplot(df,aes(screen,price))+geom_boxplot()
unique(df$ram)
cor(df$ram,df$price)
summary(df$ads)
cor(df$ads,df$price)
df2=df
df$ads=NULL
unique(df$trend)
summary(df$trend)
cor(df$trend,df$price)
#checking linear relationship between target and input values
plot(df$price,df$speed)
ggplot(df,aes(df$speed,df$price))+geom_point()
ggplot(df,aes(df$hd,df$price))+geom_point()
hist(df$price)
set.seed(234)
##sample(1:10,8)
##sample(10,5)
df3=sample(nrow(df),nrow(df)*0.8)

train=df[df3,]
test=df[-df3,]
#as we have already removed ads, so now i removing trend also from train
model1=lm(price ~ . -trend,data=train)
summary(model1)

model2=lm(price ~ . -multi -trend,data=train)
summary(model2)

##Diagnostic
##checking multicolleanrity
cor(train$hd,train$ram)



install.packages('car')

library(MASS)

vif(lm(hd ~ screen + ram,data=train))

##normal distribution between errors
plot(model1,which=2)

#Autorelation and heteroscadacity
plot(model2,which=1)
plot(model2,which=2)
plot(model2,which=3)
plot(model2,which=4)

#Confirming autorelation with durbinwatson test

durbinwatson(model2)
#check outlier

outlierTest(model2)


install.packages('ISLR')
