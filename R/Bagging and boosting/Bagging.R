library(rpart)
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
install.packages('adabag')
library('adabag')
Vehicle.bagging <- bagging(Class ~.,data=Vehicle[sub, ],mfinal=5, 
                           control=rpart.control(maxdepth=5, minsplit=15))
#Using the pruning option
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub, ], newmfinal=3)
Vehicle.bagging.pred
Vehicle.bagging.pred$error
Vehicle.bagging.pred$confusion