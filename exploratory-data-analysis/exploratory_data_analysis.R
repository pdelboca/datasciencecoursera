#>============== QUIZ 2 ==============<
### Question 2

library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)

# A set of 3 panels showing the relationship between weight and time for each diet.

### Question 3
?text()

### Question 4
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
print(p)


### Question 5
?trellis.par.set()


### Question 7
library(datasets)
library(ggplot2)
data(airquality)

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

?geom()


### Question 9
library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
print(g)


### Question 10
qplot(votes, rating, data = movies) + geom_smooth()
