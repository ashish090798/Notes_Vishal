install.packages('xgboost')
require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
##Each variable is a list containing two things, label and data. The next step is to feed this data to xgboost. Besides the data, we need to train the model with some other parameters:
##nrounds: the number of decision trees in the final model
##objective: the training objective to use, where "binary:logistic" means a binary classifier.

model <- xgboost(data = train$data, label = train$label,
                 nrounds = 2, objective = "binary:logistic")
preds = predict(model, test$data)
cv.res <- xgb.cv(data = train$data, label = train$label, nfold = 5,
                 nrounds = 2, objective = "binary:logistic")
##By setting the parameter early_stopping,
##xgboost will terminate the training process if the performance is getting worse in the iteration.
bst <- xgb.cv(data = train$data, label = train$label, nfold = 5,
              nrounds = 20, objective = "binary:logistic",
              early.stop.round = 3, maximize = FALSE)
