
setwd=("C:/Users/vishu/Documents/R/churn and credit.csv files/")
df11=read.csv("credit.csv")
  df11
getwd()  
setwd("C:/Users/vishu/Documents/R/churn and credit.csv files");
df11=read.csv("credit.csv")
id=c(1,2)
age=c(20,30)
desg=c("A","B")
emp=data.frame(id,age,desg)
View(emp)
nrow(emp)
ncol(emp)
delayedAssign(a)
emp[-1]
emp[,-1]
emp
emp[-1,]
emp[-1]
emp[1]
emp[,1]
emp[,-1]
emp[-1]
emp
emp[,-1]
emp[-1]
emp[-1,]
emp[1,1]
emp[1,]
emp[,c(1:4)]
seq(1,10,5)
gender=c("m","f","m")
emp$gender=gender
gender2=c("m","r")
emp$gender2=gender2
msg=c("hi","hello")
emp=data.frame(emp,msg)
emp$gender2=NULL
emp$msg=NULL
emp

id=c(1,1,2,2)
dept=c('a','b','a','b')
sal=c('100','200','100','200')
df1=data.frame(id,dept,sal)
id=cdept=c('a','a','b','b')
gen=c('m','m','f','f')
df2=data.frame(id,dept,gen)
df2
df1
desg=c('a','a','b','b')
id=c(1,2,1,2)
gen=c('m','m','f','f')
df2=data.frame(id,desg,gen)
df2
df3=merge(x=df1,y=df2,by)
df1
df2
dept=c('a','a','b','b')
df2=data.frame(id,dept,gen)
df3=merge(x=df1,y=df2,by=c('id','dept'))
df3
age=49
if(age<=25)
{
  agegroup='0-25'
}else if(age>26 & age<=30)
{
  agegroup='26-30'
}else if(age>30 &age<=40)
{
  agegroup='31-40'
}else if (age>=41)
{
  agegroup='40+'
}
agegroup=46
agegroup=ifelse(age<=25,'0-25',ifelse(age>26&age<=35,'25-35',ifelse(age>35&age<45,'35+','45+')))
age=c(21,36,49,54)
#agegroup=c('0-25','26-30','31-40','40+')
#if with dataframe
#creating a new column in dataframe using if and one existing column of dataframe
df=data.frame(age)
df
for(i in 1:nrow(df))
{
  if(df$age[i]<=25)
  {
    df$agegroup1[i]='0-25'
  }
  else if (df$age[i]>25&df$age[i]<=30)
  {
    df$agegroup1[i]='26-30'
  }
  else if (df$age[i]>31&df$age[i]<40)
  {
    df$agegroup1[i]='31-40'
  }
  else
  {
    df$agegroup[i]='40+'
}
}

