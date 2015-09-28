# QUESTION 1
# In a population of interest, a sample of 9 men yielded a sample average brain 
# volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T
# confidence interval for the mean brain volume in this new population?
n <- 9
smean <- 1100
se <- 30
sigma <- 1.96

ci <- smean + c(-1,1) * qt(.95+(1-.95)/2, n-1) * se / sqrt(n)

# Question 2
# A diet pill is given to 9 subjects over six weeks. The average difference in 
# weight (follow up - baseline) is -2 pounds. What would the standard deviation 
# of the difference in weight have to be for the upper endpoint of the 95% T 
# confidence interval to touch 0?
ave_difference <- -2
p <- .95
n <- 9
sd <- ave_difference * sqrt(n) / qt(p+(1-p)/2, n-1)
sd

# Question 3
# In an effort to improve running performance, 5 runners were either given a
# protein supplement or placebo. Then, after a suitable washout period, they 
# were given the opposite treatment. Their mile times were recorded under both 
# the treatment and placebo, yielding 10 measurements with 2 per subject. 
# The researchers intend to use a T test and interval to investigate the 
# treatment. Should they use a paired or independent group T test and interval?

answer <- "A paired interval."

# Question 4
# In a study of emergency room waiting times, investigators consider a new and
# the standard triage systems. To test the systems, administrators selected 20 
# nights and randomly assigned the new triage system to be used on 10 nights 
# and the standard system on the remaining 10 nights. 
# They calculated the nightly median waiting time (MWT) to see a physician.
# The average MWT for the new system was 3 hours with a variance of 0.60 while 
# the average MWT for the old system was 5 hours with a variance of 0.68. 
# Consider the 95% confidence interval estimate for the differences of the mean 
# MWT associated with the new system. Assume a constant variance. 
# What is the interval? Subtract in this order (New System - Old System).

quantile = 0.975 # 2.5% on both sides

n_y <- 10 # nights new system
n_x <- 10 # nights old system
var_y <- 0.60 # variance new (sqrt of sd)
var_x <- 0.68 # variance old (sqrt of sd)
mu_y <- 3 # average hours new system
mu_x <- 5 # average hours old system

# calculate pooled standard deviation
sd_p <- sqrt(((n_x - 1) * var_x + (n_y - 1) * var_y)/(n_x + n_y - 2))

confidenceInterval <- mu_y - mu_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * sd_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)

