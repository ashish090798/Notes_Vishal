library(car)
df=mtcars
table(df)
summary(df)
table(df$hp)
#target variable normal distrubted
hist(df$mpg)
hist(sqrt(df$mpg))
hist(log(df$mpg))
hist(1/(df$mpg))
