library(caret)
library(psych)

# read in the data
data <- sat.act
head(data)
data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(ACT ~ gender + age + SATV + SATQ,   # model to fit
                     data = data,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)

model_caret