a=10
#hi

class(a)
a=10.5
class9a
class(a)
#create object as integer
a=10L

class(a)
##typecastimh
a=as.numeric(a)
class(a)
a=as.character(a)
class(a)
a="apple"
class(a)
a=as.numeric(a)
a
b="hi"
class(b)
b=as.numeric(b)
is.numeric(b)
b="hello"
c="hey"
c=as.numeric(c)
a="10.5a"
class(a)
b="hi5"
b=as.numeric(b)
a=as.character(a)
a
k="1 jf"
k=as.integer(k)
b=TRUE
m=F
A=1
class(b)
class(m)
a=c(10,20,30,40)
a=c(10,20,30,c,"B")
b=a[c(1,2)]
a[-1]
m=a[-c(2:5)]
m
a=c(10,20,30,40)
a=as.integer(a)
a[5]=50.5
id=c(1,2,3,4,5)
age=c(20,30,40,50,60)
desg=c("a","b","a","b","a")
emp=data.frame(id,age,desg)
View(emp)
head(emp)
head(emp,4)
View(tail)
tail(emp)
str(emp    )
dim(emp)
emp[1:4,]
emp[c(1,3,5),]
seq(1,10,2)
emp
emp[,c(1,3)]
emp[,c(1:4)]
gen=c("m","f","m","f","f")
emp=data.frame(emp,gen)
emp$gender=gen
emp
emp
emp$gen=as.factor(emp$gen)
str(emp)
emp$gender=as.factor(emp$gender)
emp
str(emp)
emp$gender=NULL
emp
setwd=("C:\Users\vishu\Documents\R\")
df2=read.csv("credit.csv")
head(df2)
setwd("C:/Users/vishu/Documents/R/churn and credit.csv file");
setwd("C:/Users/vishu/Documents/R/churn and credit.csv files");
df2=read.csv("credit.csv")
head(df2);
str(df2)
df3=df2[seq(1,17,2)]
head(df3)
tail(df3)
str(df3)
str(df2)

df2$checking_balance=NULL
df2
str(df2)
df3=df2[seq(0,17,2)];
df3
tail(df3);
df4=df2[seq(0,17,2)];
tail(df4)
df5=df2[seq(0,1000,2),];
tail(df5);
ncol(df2)
df6=df2[seq(10,1000,10),];
tail(df6)
df1=df2[,-(3,8,13,17)]
head(df1);
ncol(df1)
str(df2)
df10=df2[df2$credit_history=="good"&&df2$amount>=1500,]
nrow(df10)
df9=df2[df2$credit_history=="good",]
df9
nrow(df9)
df10=df2[df2$credit_history=="good"&df2$amount>=1500,]
unique(df2$credit_history)
table(df2$credit_history)
df10=df2[df2$credit_history!="critical",];
df10=df2[df2$credit_history%in%c("good","perfect"),]
unique(df2$j
df2@agege30=ifelse(df2$age>=30, "yes", "no")
str(df2)
unique(df2$housing)
unique(df2$amount)
table(df2$housing)
df2[1,]
df2[-1,]
df1=df2[-1,]
e=c(c,1)
a=c("a","b",4,5)
a
a=a*2
c=c(1,2)
c[1]=3
c[2]=c
d=c(1,"c")
e=c(z,1)
p=c(2,3,4,5,6,m)
n=c("a",7,8,9,5,6,c)
n

df2=read.csv("credit.csv")

df2=data.frame(df2,gen)

gender=c("m","f","m","f","f","f","f")
n=data.frame(n,gender)
?data.frame
n$gen=gender
str(n)
df2$gender=gender
df2$gender=NULL
df1=df2
df1=df2[df2$amount>1000,]
df1=df1[df1$credit_history=="good",]
str(df1)
df1=df1[df1$job=="management",]
haid(df1)
head(df1)
df1=df1[df1$phone=="yes",]
tail(df1)
df1=df1[df1$age %in% c("40","54"),]
df1=df2[1,]
df1

