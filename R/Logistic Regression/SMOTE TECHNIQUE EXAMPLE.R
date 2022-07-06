data(iris)
head(iris)
View(iris)
data <- iris[, c(1, 2, 5)]
table(iris$Species)
head(data)
View(data)
data$Species <- factor(ifelse(data$Species == "setosa","rare","common"))
str(data)
table(data$Species)

 install.packages('DMwR')
library(DMwR)
newData <- SMOTE(Species ~ ., data, perc.over = 600,perc.under=200)
?SMOTE
head(newData)
table(newData$Species)
par(mfrow = c(1, 2))
?par
# plot(1:3,4:6,pch=7:9) # pch = plotting character
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")
