##########
# QUIZ 3 #
##########

# Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
set.seed(125)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test", ]

fit <- train(Class ~ ., method="rpart", data=training)

fancyRpartPlot(fit$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

# PS
# WS
# PS
# Not possible to predict

# Question 2

# The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

# Question 3

library(pgmm)
data(olive)
olive = olive[,-1]

fitOlive <- train(Area ~ ., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))

pred <- predict(fitOlive, newdata = newdata)

# 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

# Question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
             method="glm",
             family="binomial",
             data=trainSA)
predictionTrain <- predict(fit, trainSA)
predictionTest <- predict(fit, testSA)

missClass = function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
  }

missClass(trainSA$chd, predictionTrain)
missClass(testSA$chd, predictionTest)


# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)


set.seed(33833)

fit <- train(y ~ ., data=vowel.train, method="rf")
varImp(fit, useModel = TRUE)
