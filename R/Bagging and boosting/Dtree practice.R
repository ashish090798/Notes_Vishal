    library(ISLR)
data("Default")
head(Default)
library(rpart)
install.packages()
tree=rpart(default ~ ., data = Default) 
table(Default$default)
library(rpart)		# Popular decision tree algorithm
install.packages('rattle')
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
install.packages('party')
library(party)					# Alternative decision tree algorithm
install.packages('partykit')
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
fancyRpartPlot(tree)


chrn = read.csv("churn.csv")
table(chrn$Churn)
prop.table(table(chrn$Churn))
names(chrn)
#how to remove rows from the dataframe
chrn = chrn[,-c(19:21)]
names(chrn)
chrn$Churn = as.factor(chrn$Churn)
set.seed(1234)
ids=sample(nrow(chrn), nrow(chrn)*0.8)
train = chrn[ids,]
test = chrn[-ids,]

library(rpart)
tchurn=rpart(chrn$Churn ~ ., data=chrn, method="class")
fancyRpartPlot(tchurn)

