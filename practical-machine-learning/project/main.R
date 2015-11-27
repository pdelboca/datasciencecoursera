# Load libraries
library(ggplot2)
library(caret)
library(reshape2)
setwd("~/Repos/datasciencecoursera/practical-machine-learning/project")

# Get Data
# To be reproducible, there is code to donwload the data.

trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainFile <- "./data/pml-training.csv"
testFile  <- "./data/pml-testing.csv"
if (!file.exists("./data")) {
    dir.create("./data")
}
if (!file.exists(trainFile)) {
    download.file(trainUrl, destfile=trainFile, method="curl")
}
if (!file.exists(testFile)) {
    download.file(testUrl, destfile=testFile, method="curl")
}
trainingRaw <- read.csv('./data//pml-training.csv')
testingRaw <- read.csv('./data//pml-testing.csv')

# Clean Data
# Since both files has the same structure, Ive created a function to clean the 
# data and keep the code DRY.

cleanData <- function(dataset){
    dataset <- dataset[, colSums(is.na(dataset)) == 0]
    dataset <- dataset[, !grepl("timestamp", colnames(dataset))]
    dataset <- dataset[, !grepl("X", colnames(dataset))]
    dataset <- dataset[, !grepl("window", colnames(dataset))]
    dataset <- dataset[, !grepl("user_name", colnames(dataset))]
    classe <- dataset$classe
    dataset <- dataset[,sapply(dataset, is.numeric)]
    dataset$classe <- classe
    dataset
}

training <- cleanData(trainingRaw)
sum(!complete.cases(training)) # Check if all rows are completed
testing <- cleanData(testingRaw)
sum(!complete.cases(testing)) # Check if all rows are completed
# Exploratory Data Analysis
# The data seems to be ready to be used to train a model. From the PCA analysis
# we can conclude that the classes seems separable without further 
# pre-processing. Check appendix for detailed graphs.


# Train Model
# I will use Random Forest since it is a straight forward method to tackle 
# classification problems. To avoid overfitting I will use a cross validation
# method with k = 7. Why 7? Because I like the number ;).

fit <- train(classe ~ ., method="rf", data=training)

# Apendix
# Principal Component Analysis
preProc <- preProcess(training, method="pca", pcaComp = 2)
pca <- predict(preProc, training)
ggplot(pca, aes(x=PC1, y=PC2,colour=classe)) + geom_point()

pca$user_name <- trainingRaw$user_name 
ggplot(pca, aes(x=PC1, y=PC2,colour=user_name)) + geom_point()

# Highly correlated variables
M <- abs(cor(training[,-53]))
qplot(x=Var1, y=Var2, data=melt(M), fill=value, geom="tile")
diag(M) <- 0
which(M > 0.8, , arr.ind = TRUE)
