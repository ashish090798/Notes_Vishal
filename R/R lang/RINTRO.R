### assignment 
a=10
class(a)
## type casting 

a = as.integer(a)
class(a)

## evaluate data tpe 
is.integer(a)
is.numeric(a)

is.character(a)

a= 20.5

class(a)

is.numeric(a)
is.integer(a)

a = 100L

class(a)
## 

a= "Apple"
class(a)
a
a = as.factor(a)
class(a)
a
a = 20.5

class(a)
a = as.integer(a)
a
###### data structures 
#Vector 
a = c( 10,20,30,40)
class(a)
a
a = c(10,20,30,40,50.1)
a
class(a)

## a mixed vector 

a = c(10,20,"M","F",T,F)
class(a)
a

#### list 
x = c(10,20)
y = c("M","F")
d = list(x,y)
d

#### Vector slicing 

age = c( 25,26,27,28,29)


age = c(25,26,27,28,29)
age
age[1]
b = age[2]
age[2] = 35
age
c = age
age[2] = 39

age[1:3]
age[-1] 
age[-(1:3)]
age[c(1,4,5)]

age[-5:-2]

age[-c(1,5)]

### add elements
age[6] = 49
age
age[8] = 69
age

age = age[-8]
age = age[-7]
age = age[-c()]

### dataframes 

age = c(10,20,30,40,50,60,70,80)
gender = c("M","F","M","F","M","F","M","F")
desg = c("A","B","A","B","A","B","A","B")


emp = data.frame(age,gender,desg)
emp
head(emp, 4) ## by default prints first 6 records 
head(x, n) ## will print n records from x dataframe

### number of elemnts should be same in all vectors 
a = c(10,20,30)
b = c("a", "b")
k = data.frame(a,b)

### slicing a dataframe 

emp[1,] ## first row 
emp[,2] ## second column 

emp[1,2] # first row and second columns 
emp[1:3,] ## first three rows
emp[-1,] ## exclude first row
emp[,-3] ## exclude 3 column
emp[c(1,4,8), ] ## Slected rows
emp2 = emp[c(1,4,7), ] ## will create a new dataframe emp2 
emp2 = emp[1:4, 1:2] ## first four rows and two columns
k = emp[ -c(1:3), -1]

### using column names 

age = emp$age
emp$id = 1:8
## rearranging the variable order
emp = emp[,c(4,1:3)]

#### delete a variable 

emp$id = NULL ## deletes the variables id from emp 
emp = emp[,-1]
emp = emp[,c("id")]

rm(emp) ### removes from the evironment

### create datframes from external sources 

## flatfiles 
getwd() ## current working directory 
setwd("D:/AP/baging and  RF")
getwd()
### create a dataframe from CSV file 
## first using the ImportDataset option in UI 

### Read csv file into R using a program 

Churn = read.csv("Churn.csv", header = T, sep = ",")

Dataframe = read.csv("filename.ext", header = T, sep="??") # ?? Sep = "/t" tab, Sep=";"
### Using
str(Churn)

Churn$Phone = as.character(Churn$Phone)
str(Churn)

## slicing 

Churn2 = Churn[ , c(1,5,9,13)] ##selcts these four variables and creates a new dataframe 
Churn3 = Churn[ 1:100, ] ## first 100 rows 

### exclusion 

Chrun4 = Churn[ , -c(1,21)]

## dimesnions 
dim(Churn)
nrow(Churn) ## number of rows 
ncol(Churn) ## number of columns
## for a vector we can use length function for dimension 
length(age)

### Crate a new variable( rownumber) 
Churn$rownum = 1:nrow(Churn) 


## Deleting variables 
Churn$Phone = NULL
Churn = Churn[,-21]

colnames(Churn)

## Change column names 

names(dataframe) = c("Pass as many variables we have in the dataframe")

names(Churn) = c()


### arithematic operations 

a = 20 
b = 30 

a+b ## addition
a-b ## substraction
a/b ## division 
a*b # multiplication 

a ** b ; a^b 

b%%a

### vectors 

a = c(10,20,30,40,50)
b = c(1,2,3,4,5)
a+b

a = c(10,20,30,40)
b = c(1,2,3)
a+b

a = c(10,20,30,40)
b = c(1,2)

a/b

a = c(10,20,30,40,NA)
b = c(1,2,3,4,5)

k = a+b

is.na(k)
## arithematic on data frames 

Churn$Tot.Usage = Churn$Day.Mins + Churn$Eve.Mins + Churn$Night.Mins
Churn$Acct.len.years = Churn$Account.Length/12

### comparison operators 
a = c(10,20,30,40,50)
b = c(1,2,3,4,5)
a > b ## greater than 

a < b ## less than 

a >= b ## greater than or equal to
a <= b ## less than or equal to 
a == b ## equal to  
a != b ## not eqaul to b 

##COmparison operations to slice data 

Churn_accnt = Churn[ Churn$Account.Length >=100 , ]

Churn_3 = Churn[ Churn$State == "PA", ]

Churn_3 = Churn[ Churn$State=="PA" | Churn$State == "VA", ]
Churn_4 = Churn[ Churn$State %in% c("PA","VA"), ]



emp = c(1,2,3,4)
age = c(10,20,30,40)
gen = c("M","F","M","F")

df1 = data.frame(emp, age, gen)

emp = c(1,2,3,5)
sal = c(100,200,300,500)
dept = c("A","B","A","B")

df2 = data.frame(emp,sal,dept)

## innermerge 

df3 = merge(df1, df2, by="emp")
df3

## left merge 

df3 = merge(df1,df2, by="emp", all.x=T)
df3

## right merge 

df3 = merge(df1,df2, by="emp", all.y=T)
df3

### outer or full merge 

df3 = merge(df1, df2, by ="emp", all.x= T, all.y=T)
df3

## complex merge conditions 

## when there is no single unique key 

emp = c(1,1,2,2)
dep=c("A","B","A","B")
gen=c("M","F","M","F")

df1 = data.frame(emp,dep,gen)

emp1 = c(1,1,2,3)
dep=c("A","B","A","B")
sal = c(100,200,300,400)
df2 = data.frame(emp,dep,sal)



### inner join with only one variable(key)
df3 = merge(df1,df2, by=c"emp")
df3

## inner join with combination of variables as key 

df3 = merge(df1, df2, by=c("emp","dep"))
df3


### composite key 

emp1 = c(1,1,2,2)
dep=c("A","B","A","B")
gen=c("M","F","M","F")

df1 = data.frame(emp1,dep,gen)

emp2 = c(1,1,2,3)
dep=c("A","B","A","B")
sal = c(100,200,300,400)
df2 = data.frame(emp2,dep,sal)


df3 = merge(df1,df2, by.x = c("emp1","dep"), by.y=c("emp2","dep"))
df3

##### Control statements 

## Conditional statements 

age = 29 ## create agegroups are "Adult" and "Child" 

if(age >=18){ 
  agegrp = "Adult"
  } else agegrp = "Child"

agegrp

### agegrps are '0-25', '26-35', '36-45', '45+'

if (age <=25){ 
  agegrp = '0-25'
}else if(age > 25 & age <=35){
    agegrp = '26-35'
}else if(age > 35 & age <= 45){ 
    agegrp = '36-45'
  } else agegrp = '45+'

agegrp

## ifelse function 

agegrp = ifelse(age >= 18 , 'Adult', 'Child')

agegrp = ifelse( age <=25 , '0-25',(ifelse(age > 25 & age <=35,"26-35","35+")))

agegrp

## converting M and F to Male and Female 

## ifelse( condition, "truevalue", "falsevalue")

Gender = "M"

Gender = ifelse(Gender == "M", "Male", "Female")

Gender

#### Loops -- for loop 

#agegrp '0-25', '26-35', '36-45', '45+'

age = c(18, 29, 36)

agegrp = ''
agegrp=c()
for ( i in 1:length(age)){
  
  if (age[i] <=25){ 
    agegrp[i] = '0-25'
  }else if(age[i] > 25 & age[i] <=35){
    agegrp[i] = '26-35'
  }else if(age[i] > 35 & age[i] <= 45){ 
    agegrp[i] = '36-45'
  } else agegrp[i] = '45+'
  
}

agegrp

agrp = c()
agrp[2] = '0-25'
agrp

s = 45
s[2] = 46

#### running loop on a dataframe

age = c(25,36,49,54)
gen = c("M","F","M","F")
desg = c("A","B","A","B")

dfx = data.frame(age, gen, desg)
df

## create agegrp on to the df 

for( i in 1:nrow(df)) {
  
  if(df$age[i] <= 25){ 
    df$agegrp[i] = '0-25'
  } else if(df$age[i] >25 & df$age[i] <=35){
      df$agegrp[i] = '26-35'
  }else if(df$age[i] > 35 & df$age[i] <= 45){
      df$agegrp[i] = '36-45'
    }else df$agegrp[i] = '45+'
  
}

a = c("Male", "Female","Male","Female")
a = as.factor(a) 

### Functions 

## Create Dataframe Churn 

getwd()

dfc = read.csv("Churn.csv")

str(df)

summary(df) ### Summarizes the entire dataset 

summary(df$Day.Mins)

### Date variables 

a = "10/01/2018"
class(a)

a = as.Date(a, format = "%d/%m/%Y")
a

library(lubridate)
a = "10JAN2018"
a = as.Date(a, format = "%d%b%Y")
a
?as.Date

mon = month(a)

df$mon = month(df$Date)

###ODBC connections 
library(RODBC)
##install.packages("RODBC", dependencies = T)
dbcon = odbcConnect(dsn = mysql, uid = "a1234", pwd = "")

df = sqlQuery(channel = dbcon, query = "select * from ")

library("sqldf")

a = sqldf("select * from df")

a = sqldf("select Churn, count(*) as cnt from df group by Churn ")
a
## table fucntion 
table(df$Churn)

table(df$Churn, df$Intl.Plan)

a = sqldf("select Churn,State, count(*) as cnt from df group by Churn, State ")
a

prop.table(table(df$Churn))

## ordering data 

age = c(21, 39, 18, 46, 54, 22, 16)

age = age[order(-age)]

age

## order dataframes 

dfx

dfx = dfx[ order(-age), ]
dfx

dfx = dfx[order(age, gen), ]
dfx

dfy = dfx[order(-age), ]
dfy

### aggregate 
aggregate(dfc$Day.Mins, by = list(dfc$Churn), FUN = mean)


## apply 

library("dplyr")
head(dfc)


dfx = select(dfc, VMail.Message, Day.Mins, Eve.Mins)

dfx = rename(dfx,  DayMins = Day.Mins  ) #= , Day.Mins = DayMins )
?rename

## sorting data using arrange function
df10 = arrange(dfc, Day.Mins)
df11 = arrange(dfc, Day.Mins, Eve.Mins)

### Sorting in descendig order 

df12 = arrange(dfc, desc(Day.Mins))

df13 = arrange(dfc, Day.Mins, desc(Eve.Mins))


### Filter data ( slicing rows)

df15 = filter( dfc, State == "VA")

df16 = filter(dfc, State == "VA" | State == "NY")

df17 = filter(dfc, State == "VA", Day.Mins > 100)

df18 = filter(dfc, State %in% c("VA","NY","NJ"))

### SUmmarize 
?summarise

summarize( dfc, mean(Day.Mins))

summarize(group_by(dfc, State), mean(Day.Mins))

df_samp = sample_n( dfc, 1000)

df_samp = sample_frac(dfc, 0.1)

### mutate 

dfk = mutate( dfc, totus = Day.Mins + Eve.Mins+Night.Mins, totch = totus*2)
