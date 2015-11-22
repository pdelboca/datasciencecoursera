# File to compare to different coding styles in R
# Based on the Spam detection example
# Conclusion: If there is a for loop, somethings not right :P
library(kernlab)
data(spam)
set.seed(333)

smallSpam <- spam[sample(nrow(spam),size=10), ]
spamLabel <- ifelse(smallSpam$type == "spam", 2, 1) # Label to color the plot
plot(smallSpam$capitalAve, col=spamLabel)

# Good Coding Rule
rule <-  function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.40] <- "nonspam"
    prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
    prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
    return(prediction)
}

# Bad Coding Example
ruleBadCoding <- function(x){
    prediction <- rep(NA, length(x))
    for (i in seq_along(x)){
        if (x[i] > 2.7){ prediction[i] <- "spam"}
        if (x[i] < 2.4){ prediction[i] <- "nonspam"}
        if (x[i] >= 2.40 & x[i] <= 2.45){ prediction[i] <- "spam"}
        if (x[i] > 2.45 & x[i] <= 2.7){ prediction[i] <- "nonspam"}
    }
    return(prediction)
}

# Print number of NOT MATCHING CASES Between Functions for each Test
# Expected Result: All zeros, so both functions are equal.
for(i in 1:10){
    x <- runif(200, 0, 10)
    notMatching <- sum(!(rule(x) == ruleBadCoding(x)))
    print(sprintf("Not Matching in test %i: %i", i, notMatching))
}

# Testing performance of Both methods
longData <- runif(1000000,0,10) 
n <- 10
t1 <- 0
t2 <- 0
for(i in 1:n){
    temp <- system.time(rule(longData))[1]
    t1 <- t1 + temp
    temp <- system.time(ruleBadCoding(longData))[1]
    t2 <- t2 + temp
}
sprintf("Average Time Rule for %f observations: %f", length(longData), t1/n)
sprintf("Average Time Rule for %f observations: %f", length(longData), t2/n)

# 0.2 Sec vs 9.11 sec