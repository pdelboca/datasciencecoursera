library(caret)
library(kernlab)
library(reshape2)
library(ggplot2)
set.seed(31415)

# Split the data
data(spam)
indexTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[indexTrain, ]
testing <- spam[-indexTrain, ]

# Highly correlated variables
M <- abs(cor(training[,-58]))
qplot(x=Var1, y=Var2, data=melt(M), fill=value, geom="tile")
diag(M) <- 0
which(M > 0.8, , arr.ind = TRUE)
names(spam)[c(34,32)]
plot(spam[,34], spam[,32])

# PCA
smallSpam <- spam[,c(34,32)]
pca <- prcomp(smallSpam)
plot(pca$x[,1], pca$x[,2])
pca$rotation

# PCA on Spam Data
typeColor <- ((spam$type == "spam") * 1 + 1) 
pcaSpam <- prcomp(log10(spam[,-58] + 1))
plot(pcaSpam$x[,1], pcaSpam$x[,2], col=typeColor, xlab="PC1", ylab="PC2")


# PCA with caret
preProc <- preProcess(log10(spam[,-58] + 1), method="pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58] + 1))
plot(spamPC[,1], spamPC[,2], col=typeColor, xlab="PC1", ylab="PC2")


# Prediction with PCA
modelFit <- train(training$type ~ ., method="glm", preProcess = "pca", data=training)
confusionMatrix(testing$type, predict(modelFit,testing))
