setwd("C:\\Users\\vishu\\Documents\\R\\churn and credit.csv files")
getwd()
train=read.csv("train.csv")
#how many rows with a marital status=0
for(i in 1:nrow(train))
{
  if(train$Marital_Status[i]==0)
  {
    count=count+1
  }
}  
count=0


#unique of Age
t=unique(train$Age)
length(t)
distinct(train$Age)
#unique user id having age=26-35 and marital status=0
t=train
t2=t[t$Age=='26-35'& t$Marital_Status=='0',]
t3=unique(t2$User_ID)
#most frequent product id
t4=table(t$Product_ID)
sort(t4)
which.max(t4)
t4[2537]


#avg purchase rate of gender=m or gender=f
t5=t[t$Gender=='M'|t$Age=='0-17',]
mean(t5$Purchase)
#avg value of purchase within the odd rows of train
t5=t[seq(1,nrow(t),2),]
mean(t5$Purchase)
#create new dataset that doesnt have any row in train
nrow(t)
train2=na.omit(t)
nrow(train2)  


train2=is.na(t)
nrow(train2)
#cite_category in which most of the users within age grp 0-17 live

t2=t[t$Age=='0-17',]
t3=data.frame(table(t2$City_Category))
t3=t3[order(t3$Freq,decreasing = T),]
t3[1,]
#for how many rows product_category_2  missing a value

sum(is.na(t$Product_Category_2))
count()


#which value of product_category_1 occurs the most whenever product_category_2 is missing

t2=t[is.na(t$Product_Category_2),]
t2=data.frame(table(t2$Product_Category_1))
t2=t2[order(t2$Freq,decreasing = T),]
t2[1,]


#t=merge(x=test,y=train,by="User_ID")
#head(t)
#of all the users in train how many exists in train


length(intersect(unique(train$User_ID),unique(test$User_ID)))

#avg purchase of customers who exist in train but not in tes
t=train
t1=t[!(t$User_ID %in% (intersect(unique(t$User_ID),unique(test$User_ID)))),]
head(t1)

#function taken name as input and gives out the frequenct of occurrence table of the distinct values of variable
myfirstfun(Age)


myfirstfun=function(n)
  
{
  t1=unique(t$n)
  t2=table(t1)
  t2
}

