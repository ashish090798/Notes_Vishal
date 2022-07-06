# ROSE (Random Over Sampling Examples) package - 
# Helps in implementing techniques for handling class imbalance 
install.packages("ROSE")
library(ROSE)
data(hacide)
head(hacide.train)
str(hacide.train)

# Check for data imbalance
table(hacide.train$cls)
prop.table(table(hacide.train$cls))

# Lets build the logistic regression model on this data
# (without balancing the data)
classifier_imb = glm(cls ~ ., data = hacide.train, family = 'binomial')
summary(classifier_imb)
pred_classifier_imb = predict(classifier_imb, type = 'response', newdata = hacide.test)
y_pred = pred_classifier_imb > 0.5
print(y_pred)
# All y_pred are 0 (False), hence the model was unable to
# accurately predict the minority class cases
# Lets use accuracy.meas of ROSE to get prediction metrics

accuracy.meas(hacide.test$cls, pred_classifier_imb)

# Lets also check the model's performance using ROC
# We use the roc.curve function from ROSE for this
roc.curve(hacide.test$cls, pred_classifier_imb)

###### Oversampling ######
# Lets now apply oversampling for balancing the data
# We use the ovun.sample function from ROSE
# N is the number of data points in the resulting balanced data
data_balanced_over <- ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 2000)$data
table(data_balanced_over$cls)

###### Undersampling ######
# Lets now apply undersampling for balancing the data
# We use the ovun.sample function from ROSE
# N is the number of data points in the resulting balanced data
?ovun.sample
data_balanced_under <- ovun.sample(cls ~ ., data = hacide.train, method = "under",N = 90)$data
table(data_balanced_under$cls)

###### Undersampling & Oversampling ######
# We can apply both these techniques for balancing the data
# p is the approx probability of the positive class in the resulting balanced data
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both",p = 0.1)$data
table(data_balanced_both$cls)

###### Synthetic Data Generation (ROSE) ######
# Lets now generate synthetic data using ROSE
data_balanced_rose <- ROSE(cls ~ ., data = hacide.train, p = 0.5)$data
table(data_balanced_rose$cls)

###### Comparing the sampling techniques #######
# Lets now make models using the different techniques
classifier_over = glm(cls ~ ., data = data_balanced_over, family = 'binomial')
classifier_under = glm(cls ~ ., data = data_balanced_under, family = 'binomial')
classifier_both = glm(cls ~ ., data = data_balanced_both, family = 'binomial')
classifier_rose = glm(cls ~ ., data = data_balanced_rose, family = 'binomial')

# Predicting using the above models
pred_classifier_over = predict(classifier_over, type = 'response', newdata = hacide.test)
pred_classifier_under = predict(classifier_under, type = 'response', newdata = hacide.test)
pred_classifier_both = predict(classifier_both, type = 'response', newdata = hacide.test)
pred_classifier_rose = predict(classifier_rose, type = 'response', newdata = hacide.test)

# Confusion metrics of the above models
table(hacide.test$cls, pred_classifier_imb > 0.5)
table(hacide.test$cls, pred_classifier_over > 0.5)
table(hacide.test$cls, pred_classifier_under > 0.5)
table(hacide.test$cls, pred_classifier_both > 0.5)
table(hacide.test$cls, pred_classifier_rose > 0.5)


# Evaluating the models using ROC
roc.curve(hacide.test$cls, pred_classifier_over)
roc.curve(hacide.test$cls, pred_classifier_under)
roc.curve(hacide.test$cls, pred_classifier_both)
roc.curve(hacide.test$cls, pred_classifier_rose)

# Plotting the cases
par(mfrow = c(2, 2))
plot(hacide.train[, 2], hacide.train[, 3], pch = as.integer(hacide.train[, 1]),
     main = "Original Data")
plot(data_balanced_over[, 2], data_balanced_over[, 3], pch = as.integer(data_balanced_over[, 1]),
     main = "Oversampled Data")
plot(data_balanced_under[, 2], data_balanced_under[, 3], pch = as.integer(data_balanced_under[, 1]),
     main = "Undersampled Data")
plot(data_balanced_rose[, 2], data_balanced_rose[, 3], pch = as.integer(data_balanced_rose[, 1]),
     main = "ROSE Data")

