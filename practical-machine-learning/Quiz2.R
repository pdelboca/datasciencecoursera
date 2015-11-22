#Quiz 2

# Question 1

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(concrete$Superplasticizer, breaks=50)
hist(log(concrete$Superplasticizer), breaks=50)

sum(concrete$Superplasticizer == 0)
# There are values of zero so when you take the log() transform those values will be -Inf.

# Question 3

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- grepl('^IL', colnames(training))
trainingIL <- training[,cols]

# thresh parameters set the proportion of variance needed to be explained
pca <- preProcess(trainingIL, method="pca", thresh = 0.9)
pca$numComp

#Question 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- grepl('^IL', colnames(training))
trainingIL <- training[,cols]
trainingIL$diagnosis <- training$diagnosis
testingIL <- testing[,cols]
testingIL$diagnosis <- testing$diagnosis

fit <- train(trainingIL$diagnosis ~ .,
             method="glm",
             data=trainingIL)

control <- trainControl(preProcOptions = list(thresh=0.8))
fitPCA <- train(trainingIL$diagnosis ~ .,
                 method="glm",
                 preProcess = "pca",
                 trControl = control,
                 data=trainingIL)

round(confusionMatrix(testingIL$diagnosis, predict(fit, testingIL))$overall[1],2)
round(confusionMatrix(testing$diagnosis, predict(fitPCA, testingIL))$overall[1],2)

