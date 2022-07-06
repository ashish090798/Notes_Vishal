setwd("C:/Users/vishu/Documents/R")
ch=read.csv("Churn_MV.csv")
ch
head(ch)
table(ch)
View(ch)
str(ch)
#converting int into factor
ch$Churn=as.factor(ch$Churn)
str(ch)
ch$Area.Code=as.factor(ch$Area.Code)
str(ch)
ch$VMail.Message=as.numeric(ch$VMail.Message)
str(ch)

ch$Intl.Plan=as.factor(ch$Intl.Plan)
str(ch)
ch$VMail.Plan=as.factor(ch$VMail.Plan)
summary(ch)
ch2=ch

#taking only those rows where churn has valid values i.e no na 
ch=ch[!is.na(ch$Churn),]
summary(ch)
#calculating mean and replacing na values of Daily charges
Daily.Charges_mean=mean(ch$Daily.Charges.MV,na.rm=T)
ch$Daily.Charges.Mean=ch$Daily.Charges.MV
ch[is.na(ch$Daily.Charges.Mean),'Daily.Charges.Mean']=Daily.Charges_mean
str(ch)
View(ch)

Daily.Charges_median=median(ch$Daily.Charges.MV,na.rm=T)
ch$Daily.Charges.Median=ch$Daily.Charges.MV
ch[is.na(ch$Daily.Charges.Median),'Daily.Charges.Median']=Daily.Charges_median
#calculating differences

ch$Daily.Charges.Mean.Diff=Daily.Charges_mean-ch$Day.Charge
ch$Daily.Charges.Median.Diff=Daily.Charges_median-ch$Day.Charge

#removing outliers
boxplot(ch$Day.Charge)
outlier_values <- boxplot.stats(ch$Day.Charge)$out
outlier_values
x = ch$Day.Charge
qnt <- quantile(x, probs=c(.01,.02,.03,.04,.05,.1,.15,.20,.25,
                           .30,.35,.40,.45,.50,
                           .55,.60,.65,.70,.75,
                           .80,.85,.90,.95,.98,.99,1), na.rm = T)
qnt
ch$Day.Charge
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[9] - H)]  <- caps[1]
x[x > (qnt[19] + H)]  <- caps[2]
boxplot(x)
str(ch)


ch3=ch
ch3$Churn=NULL
ch3$Intl.Plan=NULL
ch3$VMail.Plan=NULL
ch3$State=NULL
ch3$Area.Code=NULL
ch3$Phone=NULL
#corplot
library(corrplot)
q=cor(ch3)
q
corrplot(q, method="number")


