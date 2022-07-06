##############################################################################
# R Session	- Data Preparation(pre-processing)					                                                       #
# Date:  08/28/2017									                                         #
# Author: Manish                              							                 #	
##############################################################################

##1. Data Cleaning
##2. Feature Engineering

##############################################################################
# 					                                                                 #
# 									         Data Cleaning                                   #
#                                                                            #
##############################################################################
#In this section we will see:
        #Importing Data
        #Basic Data Cleaning
               #1. De-duplication
               #2. Removing redundancy
               #3. Correcting Datatypes
        #Feature Specific Data Cleaning
               #1. Missing Value Treatment
               #2. Outlier Treatment


##Why Data Cleaning??
   ##Data is the key; better the data better would be the 
   ##predictability of the model. Typically, 
   ##data is residing in various platforms and the first job 
   ##of a data scientist is to collate the data from various 
   ##sources to a unified platform.


##Read the Census data
setwd("C:\\Analytics\\Personal\\Machine Learning\\Training\\R\\Dataset")
census <- read.csv("income_Census.csv",header = F)

names(census) <- c ("Age","IndustryCode","Education",
                    "educationnum",
                    "maritalstatus","Relationship",
                    "Race","Gender","capitalgain",
                    "capitalloss",
                    "hoursperweek","GT50K")
str(census)



##1. De-duplication:
# What do you understand by de-duplication?
# Reason for de-duplication?

##Check the dulicate

name = c("Anne","Anne", "Pete", "Cath", "Cath", "Cath")
age = c(28,28,30,25,29,35)
child <- c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE)
df2 <- data.frame(name,age,child)
df2
duplicated(df2)

sum(duplicated(census))

##Removing duplicate - First way:
unique(df2)
df <- unique(census)

##Removing duplicate - Second way:
df2
df2[!duplicated(df2),]


df1 <- census[!duplicated(census),]

df2
duplicated(df2$name)
##Removing duplicate - Third way: - Only if Primary key exist
#df1 <- census[!duplicated(census$PrimaryKeyColumn),]


##2. Removing Redundancy 
    ##Homonyms (Same Column Names but different Information)
    ##Synonyms (Different Column Names but Same Information)

# To deal with Homonyms, change column names
colnames(census)[1] <- "AGE"

colnames(census)[c(1,2,3)] <- c("Age","Industrycode","education")

#To deal with synonyms delete one of the column

##Correct Format
str(census)
census$educationnum <- as.factor(census$educationnum)

##Correct the date format
as.Date('1/15/2001', format = '%m/%d/%Y')
as.Date('April 26 2001',format = '%B %d %Y')
as.Date('22Jun01', format = '%d%b%y')



#Change the default format of R
format(as.Date('1/13/99', format = '%m/%d/%y'),
       '%d-%m-%Y')


##Why Missing Value and OUtlier occur?
   ##Way of collection
   ##Method of storage in wharehouse
   ##Way of importing from different sources

##Impact of OUtlier
   ##Wrong Information (but not always)

##Impact of Missing Value
   ##Represent of loss of information
   ##REduce the Accuracy

##Missing Value Treatment
   ##1. Remove the Missing Values - 
     #Impact (Loss of information)
   ##2. Replace with the appropriate values 
     ##- zero, mean,mode
         sum(is.na(census$Age))
         #Calcualte the mean  
         x <- round(mean(census$Age,
                                     na.rm = T),0)

        #Replace the missing values with mean
        census$Age[is.na(census$Age)] <- x


        census$capitalgain[is.na(census$capitalgain)] <- 0
        sum(is.na(census$capitalgain))
        table(census$Gender)
        census$Gender[census$Gender=='?'] <- NA
        sum(census$Gender=='?')
        table(census$Gender)
        sum(is.na(census$Gender))
        census$Gender <- as.character(census$Gender)
        table(census$Gender)
        #Mode -- Male
        census$Gender[is.na(census$Gender)] <- "Male"
        census$Gender <- as.factor(census$Gender)
    ##3. Regression Technique (Advance)

##OUtlier Treatment

setwd("C:\\Analytics\\Personal\\Machine Learning\\Training\\R\\Dataset\\EDA & VIZ\\EDA & VIZ")

#Importing the dataset
credit <- read.csv("credit.csv")


#Outlier Identification and Treatment
# 1. Outliers can be detected using boxplot or by 
#    generating the percentile
#    distribution
# 2. To treat the outlier, flooring and 
#    capping can be usedbased on percentile 
#    distribution


boxplot(credit$amount)

outlier_values <- boxplot.stats(credit$amount)$out


#Outlier treatment
#imputation
#capping and flooring
x <- credit$amount
qnt <- quantile(x, probs=c(.01,.02,.03,.04,.05,.1,.15,.20,.25,
                           .30,.35,.40,.45,.50,
                           .55,.60,.65,.70,.75,
                           .80,.85,.90,.95,.98,.99,1), na.rm = T)
#qnt1 <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[9] - H)]  <- caps[1]
x[x > (qnt[19] + H)]  <- caps[2]



##Feature Engineering:
   ##Feature Extraction: 
         ##Feature extraction is the first and 
         ##most basic feature engineering step where 
         ##the current features in the data set are 
         ##leveraged to generate new, possibly more informative features

         census$Workload[census$hoursperweek>45] <- "High"
         census$Workload[census$hoursperweek<40] <- "Low"
         census$Workload[
           which(census$hoursperweek<=45 & 
                   census$hoursperweek>=40)] <- "Med"
   
   ## Feature Transformation
         #Transformation helps us in reducing the skewness from the data
         #Log Transformation - for right skewed
         #Square - for left skewed
         #Square Rt - Right skewed
         #Reciprocal - makes bigger points smaller and vice versa
x = c(1.0, 1.2, 1.1, 1.1, 2.4, 2.2, 2.6, 4.1, 5.0, 
              10.0, 4.0, 4.1, 4.2, 4.1, 5.1, 4.5, 5.0, 15.2, 
              10.0, 20.0, 1.1, 1.1, 1.2, 1.6, 2.2, 3.0, 4.0, 10.5)
hist(x) 

#Since the data is right-skewed, we will apply common transformations for 
#right-skewed data:  square root, cube root, and log.

#Squar root transformation
hist(sqrt(x))

#Cube root transformation
#The cube root transformation is stronger than the square root transformation.
hist(sign(x) * abs(x)^(1/3)) 

#Log transformation
#The log transformation is a relatively strong transformation.  
hist(log(x))


  ## Feature Standardisation
        #Feature Normalization 
            #min-Max Score
              age <- c(20, 30, 40)
              salary <- c(500000, 1000000, 1500000)
              df <- data.frame( "Age" = age, "Salary" = salary, stringsAsFactors = FALSE)
              #(X - min(X))/(max(X) - min(X))
              #THis could be programmed as the following function in R:
              normalize <- function(x) {
                return ((x - min(x)) / (max(x) - min(x)))
              }
              
              #In order to apply above normalize function on each of the 
              #features of above data frame
              
              dfNorm <- as.data.frame(lapply(df, normalize))
              
            #(z-score normalization = (v-mean)/SD)
              #The disadvantage with min-max normalization technique is that it 
              #tends to bring data towards the mean. 
              #If there is a need for outliers to get weighted more than the 
              #other values, z-score standardization technique suits better.  
              View(scale(census$Age))
              
        #Feature Conversion
            #Categorical to Numeric
               #Use factor() if two categories
              census$Gender <-  as.factor(as.numeric(as.factor(census$Gender)))
               #Use dummy encoding if more than 2 categories
              table(census$education) ##16 varaibles
              dummy <- as.data.frame(model.matrix(~education,data = census))
            #NUmerical to Categorica
                #Useful for making some cross tabulation for a variable
                ?cut
                Cat_Age <- cut(census$Age,breaks = c(0,20,30,40,50,100),
                    labels = c("A","B","C","D","E"))
                  #intervals are left open and right closed
                census$Age[1:5]
                Cat_Age[1:5]
                #Make is left closed and right open
                Cat_Age <- cut(census$Age,breaks = c(0,20,30,40,50,100),
                               labels = c("A","B","C","D","E"),right = F)
                census$Age[1:5]
                Cat_Age[1:5]
                
              

         