########################
#   Loading Packages   #
########################

library(ggplot2)
library(caret)

########################
#       Load Data      #
########################
data(iris)
names(iris)
table(iris$Species)

indexTrain <- createDataPartition(iris$Species, p = 0.70, list = FALSE)
training <- iris[indexTrain,]
testing <- iris[-indexTrain, ]

qplot(Petal.Width, Sepal.Width, col=Species, data=training)


########################
# Classification Trees #
########################

fit <- train(Species ~ ., method="rpart", data=training)
print(fit$finalModel)

pred <- predict(fit, newdata = testing)
confusionMatrix(testing$Species, pred)


########################
#    Random  Forest    #
########################

rfFit <- train(Species ~ ., data=training, method="rf", prox=TRUE)
rfFit

getTree(rfFit$finalModel, k=2)

# Class Center

irisP <- classCenter(training[,c(3,4)], training$Species, rfFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=10, shape=4, data=irisP)

# Prediction Color
pred <- predict(rfFit, testing)
testing$PredRight <- testing$Species == pred 
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, col=PredRight, data=testing, main="New Data Prediction")

########################
#     Ozone Bagging    #
########################
library(dataset)
data(airquality)
airquality <- airquality[!is.na(airquality$Ozone), ]
plot(airquality$Ozone, airquality$Temp)

ll <- matrix(NA, nrow = 10, ncol = 155)
for(i in 1:10){
    ss <- sample(1:dim(airquality)[1], replace=TRUE)
    airqualityTemp <- airquality[ss,]
    airqualityTemp <- airqualityTemp[order(airqualityTemp$Ozone), ]
    loessTemp <- loess(Temp ~ Ozone, data = airqualityTemp, span=0.1)
    pred <- predict(loessTemp, newdata=data.frame("Ozone" = 1:155))
    ll[i, ] <- pred
    print(summary(pred))
}

plot(airquality$Ozone, airquality$Temp, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i,], col="grey",lwd=2)}
lines(1:155, apply(ll,2,mean), col="red", lwd=2)
