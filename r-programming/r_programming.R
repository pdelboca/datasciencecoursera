# >============== Quiz Week 2 ================<
# QUESTION 1
cube <- function(x, n) {
    x^3
}
cube(3)


# QUESTION 2
x <- 1:10
if(x > 5) {
    x <- 0
}

# QUESTION 3
f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}

z <- 10
f(3)

# QUESTION 4
x <- 5
y <- if(x < 3) {
    NA
} else {
    10
}
y

# QUESTION 5
h <- function(x, y = NULL, d = 3L) {
    z <- cbind(x, d)
    if(!is.null(y))
        z <- z + y
    else
        z <- z + f
    g <- x + y / z
    if(d == 3L)
        return(g)
    g <- g + 10
    g
}

# >============== Quiz Week 3 ================<
# Question 1
# Take a look at the 'iris' dataset that comes with R. The data can be loaded
# with the code:
library(datasets)
data(iris)
# A description of the dataset can be found by running
?iris
# There will be an object called 'iris' in your workspace. In this dataset,
# what is the mean of 'Sepal.Length' for the species virginica? (Please only
#enter the numeric result and nothing else.)
library(dplyr)

iris <- tbl_df(iris)
species <- group_by(iris,Species)
means <- summarize(species,mean(Sepal.Length))
means

# Question 2
# Continuing with the 'iris' dataset from the previous Question, what R code 
# returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 
# 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)
#apply(iris[, 1:4], 1, mean)
#colMeans(iris)
#apply(iris, 1, mean)


# Question 3
# Load the 'mtcars' dataset in R with the following code
library(datasets)
data(mtcars)
# There will be an object names 'mtcars' in your workspace. You can find some 
#information about the dataset by running
?mtcars
# How can one calculate the average miles per gallon (mpg) by number of 
# cylinders in the car (cyl)?
sapply(split(mtcars$mpg, mtcars$cyl), mean)
# lapply(mtcars, mean)
# mean(mtcars$mpg, mtcars$cyl)
# apply(mtcars, 2, mean)


# Question 4
# Continuing with the 'mtcars' dataset from the previous Question, what is the 
# absolute difference between the average horsepower of 4-cylinder cars and the 
# average horsepower of 8-cylinder cars?
library(dplyr)
mtcars <- tbl_df(mtcars)

hp_mean_4 <- mtcars %>% filter(cyl == 4) %>% summarise(mean(hp))
hp_mean_4 <- as.double(hp_mean_4)

hp_mean_8 <- mtcars %>% filter(cyl == 8) %>% summarise(mean(hp))
hp_mean_8 <- as.double(hp_mean_8)

abs(hp_mean_4-hp_mean_8)

# Question 5
# If you run
debug(ls)
# what happens when you next call the 'ls' function?
Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.
#You will be prompted to specify at which line of the function you would like to suspend execution and enter the browser.
#Execution of the 'ls' function will suspend at the 4th line of the function and you will be in the browser.
#The 'ls' function will return an error.

#>============== QUIZ WEEK5 ================<
# Question 1:
# What is produced at the end of this snippet of R code?
# set.seed(1)
# rpois(5,2)
# After set.seed(1), always same numbers -> reproducibility
set.seed(1)
rpois(5,2)
rpois(5,2)
rpois(5,2)
set.seed(2)
rpois(5,2)
rpois(5,2)
set.seed(1)
rpois(5,2)


# Question 5
set.seed(10)
x <- rbinom(10,10,0.5)
x
e <- rnorm(10,0,20)
e
y <- 0.5 + 2 * x + e
y

plot(x,y)


# Question 8
x1 <- rnorm(10,0,20)
x2 <- 0.5 + 2 * x
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
summaryRprof()
