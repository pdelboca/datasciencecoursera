# Exercises Page 12
# 1
x <- c(0.725,0.429,-0.372,0.863)
mu = mean(x)
sum((x-mu)^2)

# 2
x <- c(0.725,0.429,-0.372,0.863)
w <- c(2,2,1,1)
sum(w*x) / sum(w) # General rule for minimizing with weights

# 3
library(UsingR)
data(galton)
y <- galton$parent
x <- galton$child

yc <- y - mean(y)
xc <- x - mean(x)

lm(yc ~ xc - 1)

##############################################################

# QUIZ 1
# Question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

sum(w*x) / sum(w)

# Question 2
library(ggplot2)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y ~ x - 1)

p <- ggplot(data.frame(x,y), aes(x=x,y=y))
p <- p + geom_point()
p <- p + geom_smooth(method="lm")
p

# Question 3
data(mtcars)

plot(mtcars$wt, mtcars$mpg)

coef(lm(mtcars$mpg ~ mtcars$wt))[2]

# Question 4

# s = cor(y,x) * (sd(y) / sd(x)) 
# sd(x) = 1/2 * sd(y)
# cor(y,x) = .5
.5 * 2

# Question 5
# cor(y,x) = 0.4
# x = 1.5
# y = cor(y,x) * x + 0
y <- 0.4 * 1.5 + 0
y 

# Question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x - mean(x)) / sd(x)
xn[1]


# Question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)

# Question 8
# It must be identically 0.

# Question 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# Problem 10.
# = var(Y)/var(X)


