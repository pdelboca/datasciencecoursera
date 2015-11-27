# QUIZ 4
# Question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
# Then set the seed to 33833. 
set.seed(33833)
# Fit (1) a random forest predictor relating the factor variable y to the 
# remaining variables and 
fitRF <- train(y ~ ., method="rf", data=vowel.train)
# (2) a boosted predictor using the "gbm" method. 
fitGBM <- train(y ~ ., method="gbm", data=vowel.train)

# What are the accuracies for the two approaches on the test data set?
# What is the accuracy among the test set samples where the two methods agree?
predRF <- predict(fitRF, vowel.test)
predGBM <- predict(fitGBM, vowel.test)

confusionMatrix(predRF, vowel.test$y)$overall[1]
confusionMatrix(predGBM, vowel.test$y)$overall[1]

agree <- data.frame(predRF, predGBM, y=vowel.test$y, agree=predRF==predGBM)
accuracy <- sum(predRF[agree$agree] == agree$y[agree$agree]) / sum(agree$agree)
accuracy

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Set the seed to 62433
set.seed(62433)
# and predict diagnosis with all the other variables using a random forest ("rf"),
fit1 <- train(diagnosis ~ ., method = "rf", data=training)
# boosted trees ("gbm") 
fit2 <- train(diagnosis ~ ., method = "gbm", data=training)
# and linear discriminant analysis ("lda") model. 
fit3 <- train(diagnosis ~ ., method = "lda", data=training)
# Stack the predictions together using random forests ("rf").
pred1 <- predict(fit1, testing)
pred2 <- predict(fit2, testing)
pred3 <- predict(fit3, testing)
stack <- data.frame(pred1, pred2, pred3, diagnosis = testing$diagnosis)

ensemble <- train(diagnosis ~ ., method="rf", data=stack)
predEnsemble <- predict(ensemble, testing)

a1 <- confusionMatrix(pred1,testing$diagnosis)$overall[1]
a2 <- confusionMatrix(pred2,testing$diagnosis)$overall[1]
a3 <- confusionMatrix(pred3,testing$diagnosis)$overall[1]
a4 <- confusionMatrix(predEnsemble,testing$diagnosis)$overall[1]

print(c(a1,a2,a3,a4))

# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
# Set the seed to 233
set.seed(233)
# and fit a lasso model to predict Compressive Strength. 
fit <- train(CompressiveStrength ~ ., method="lasso", data=training)
# Which variable is the last coefficient to be set to zero as the penalty 
# increases? (Hint: it may be useful to look up ?plot.enet).
plot.enet(fit$finalModel, xvar="penalty", use.color=TRUE)

# Question 4
library(lubridate)  # For year() function below
dat = read.csv("~/Repos/datasciencecoursera/practical-machine-learning/data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
fit <- bats(tstrain)
pred <- forecast(fit, level=95, h=dim(testing)[1])

predComb <- cbind(testing, data.frame(pred))
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & (predComb$visitsTumblr < predComb$Hi.95)
# How many of the testing points is the true value within the 
# 95% prediction interval bounds?
prop.table(table(predComb$in95))[2] # 0.9617021

# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325 and 
set.seed(325)
# fit a support vector machine using the e1071 package 
# to predict Compressive Strength using the default settings. 
library(e1071)
fit1 <- svm(CompressiveStrength ~ . , data = training)
#Predict on the testing set. What is the RMSE?
pred <- predict(fit1, testing)
accuracy(pred, testing$CompressiveStrength)
