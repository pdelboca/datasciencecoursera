# SPAM EXAMPLE

library(kernlab)
data(spam)
set.seed(333)

smallSpam <- spam[sample(nrow(spam),size=10), ]
spamLabel <- ifelse(smallSpam$type == "spam", 2, 1) # Label to color the plot
plot(smallSpam$capitalAve, col=spamLabel)

# Overfitted Rule
overfittedRule <-  function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.40] <- "nonspam"
    prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
    prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
    return(prediction)
}
# Rule 1 Accuracy
table(overfittedRule(smallSpam$capitalAve),smallSpam$type)

# Not-Overfitted Rule
goodRule <-  function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.8] <- "spam"
    prediction[x <= 2.8] <- "nonspam"
    return(prediction)
}

# Rule 2 Accuracy
table(goodRule(smallSpam$capitalAve),smallSpam$type)

# Apply to complete dataset
table(overfittedRule(spam$capitalAve), spam$type)
table(goodRule(spam$capitalAve), spam$type)

# "Precision" Overfitted Rule
sum(overfittedRule(spam$capitalAve) == spam$type)
# "Precision" Good Rule
sum(goodRule(spam$capitalAve) == spam$type)
