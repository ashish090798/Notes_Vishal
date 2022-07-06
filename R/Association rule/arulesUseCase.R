data = read.csv('C:/Users/phsivale/Documents/Trainings/universalBank.csv')
data$ID = NULL
data$ZIP.Code = NULL



hist(data$Age)

library(arules)
discretize()

data$Age2 = discretize(data$Age,
                       method = 'interval',
                       categories = 3,## breaks in new version of arules package
                       labels = c('Low','med','high'))

head(data[,c('Age','Age2')],10)
# data = na.omit(data)


##Bucketing Age to 5 Levels
hist(data$Age)
data$Age2 = ifelse(data$Age <= 18, '<18',
                   ifelse((data$Age > 18 & data$Age <= 30),'18_30',
                          ifelse((data$Age > 30 & data$Age <= 40),'30_40',
                                 ifelse((data$Age > 40 & data$Age <= 50),'40_50',
                                        ifelse((data$Age > 50 & data$Age <= 60),'50_60','>60')))))
table(data$Age2)
hist(data$Mortgage)
### Mortgage
data$Mortgage = as.factor(ifelse(data$Mortgage==0,'low','high'))

##Bucketing Exp to 5 Levels
hist(data$Experience)
data$Experience2 = ifelse(data$Experience <= 5, '<5',
                          ifelse((data$Experience > 5 & data$Experience <= 10),'5_10',
                                 ifelse((data$Experience > 10 & data$Experience <= 20),'10_20',
                                        ifelse((data$Experience > 20 & data$Experience <= 30),'20_30','>30'))))
table(data$Experience2)

##Bucketing CCAvg to H, M, L
quantile(data$CCAvg,0.5)
data$CCAvg2 = ifelse(data$CCAvg <= quantile(data$CCAvg,0.25),'Low',
                     ifelse(data$CCAvg >= quantile(data$CCAvg,0.75),'High',"Med"))
table(data$CCAvg2)

##Bucketing income to H, M, L
data$Income2 = ifelse(data$Income <= quantile(data$Income,0.25),'Low',
                     ifelse(data$Income >= quantile(data$Income,0.75),'High',"Med"))
table(data$Income2)
str(data)

##Handling Binary Variables
binaryVars =c("Personal.Loan","Securities.Account","CD.Account","Online","CreditCard")
for(i in binaryVars){
  data[,i] = as.numeric(as.character(data[,i]))
  data[data[,i]==0,i] = NA
}
head(data)
# data$Mortgage = NULL
# Creating New dataset with required columns
colsToUse = c("Education","Personal.Loan","Securities.Account","CD.Account",
              "Online","CreditCard","Age2","Experience2","CCAvg2","Income2","Mortgage")
data_new = data[,colsToUse]

head(data_new)
##Converting to Factors
for(i in 1:ncol(data_new)){
  data_new[,i] = as.factor(data_new[,i])
}

head(data_new)

library(arules)
# discretize()
#Creating Transactions Data
data_trans = as(data_new,'transactions')

inspect(head(data_trans,20))

#arules
rules = apriori(data_trans,parameter = list(supp = 0.01, target = "rules"))
inspect(head(rules,20))

## Subsetting Rules
rules_subset = subset(rules,subset = rhs %pin% "Personal.Loan")
inspect(rules_subset)
