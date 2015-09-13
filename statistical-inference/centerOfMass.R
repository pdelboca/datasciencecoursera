library(UsingR)
data(galton)
library(ggplot2)
library(reshape2)
library(manipulate)

# Plotting histograms of Childs and Parents height
longGalton <- melt(galton, measure.vars = c("child","parent"))
g <- ggplot(longGalton, aes(x=value)) + 
    geom_histogram(aes(y=..density.., fill = variable), binwidth=1, color="black") +
    geom_density() +
    facet_grid(. ~ variable)
g

# Using manipulate to explore the mean
myHist <- function(mu){
    g <- ggplot(galton, aes(x=child)) +
        geom_histogram(fill = "salmon", binwidth=1, aes(y=..density..), color="black") +
        geom_density() +
        geom_vline(xintercept=mu, size=2)
    mse <- round(mean((galton$child - mu)^2), 3)
    g <- g + labs(title=paste('mu = ', mu, ' MSE = ', mse))
    g
}
manipulate(myHist(mu), mu = slider(62,74, step = 0.1))
