install.packages('DMwR')
install.packages('lattice')
install.packages('grid')
library(DMwR) 
#DMwr-data mining with r
data("iris")
iris
unique(iris$Species)
f= iris
View(f)
table(f$Species)
# 4 independent input variables and 1 target variable = Species
# Let us first consider only 2 independent variables and the target variable

f1=f[,c(1,2,5)]
head(f1)
f1$Species = factor(ifelse(f1$Species == "setosa","rare","common"))
str(f1)
table(f1$Species)  # common = 100 and rare = 50, which means this data set is not balanced

# let's balance the data set using SMOTE
?SMOTE

f2= SMOTE(Species ~ ., f1, perc.over = 400, perc.under = 125)
head(f2)

table(f2$Species)
