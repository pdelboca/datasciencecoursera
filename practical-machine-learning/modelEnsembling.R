# Model Ensembling Example
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select=-c(logwage))

# Creating Training, Testing and Validation Sets
##################################################
#                     Wage                       #
##################################################
#             buildData             # validation #
##################################################
#        training       #  testing  # validation #
##################################################

inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

# Training models
fit1 <- train(wage ~ ., method="glm", data = training)
fit2 <- train(wage~ . , method = "rf", trControl = trainControl("cv"),
              number=3, data=training)

pred1 <- predict(fit1, testing)
pred2 <- predict(fit2, testing)

qplot(pred1, pred2, colour=wage, data=testing) +
    geom_abline(intercept=0,slope=1)
qplot(pred1, testing$wage) + geom_abline(intercept=0,slope=1)
qplot(pred2, testing$wage) + geom_abline(intercept=0,slope=1)

preDF <- data.frame(pred1, pred2, wage=testing$wage)
combModFit <- train(wage ~ ., method="gam", data=preDF)
combPred <- predict(combModFit, preDF)

qplot(combPred, testing$wage) + geom_abline(intercept=0,slope=1)

# Testing errors
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))

# Predict on Validation Set
pred1V <- predict(fit1, validation)
pred2V <- predict(fit2, validation)
preDFV <- data.frame(pred1=pred1V, pred2=pred2V)
combPredV <- predict(combModFit, preDFV)

sqrt(sum((pred1V - validation$wage)^2))
sqrt(sum((pred2V - validation$wage)^2))
sqrt(sum((combPredV - validation$wage)^2))

