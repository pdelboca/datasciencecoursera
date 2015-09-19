# Question 2:
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally 
# distributed with a mean of 80 (mm Hg) and a standard deviation of 10. 
# About what is the probability that a random 35-44 year old has a DBP 
# less than 70?
pnorm(70,80,10)

#Question 3:
# Brain volume for adult women is normally distributed with a mean of 
# about 1,100 cc for women with a standard deviation of 75 cc. What brain volume 
# represents the 95th percentile?
qnorm(0.95,1100,75)

# Question 4:
# Refer to the previous question. Brain volume for adult women is about 1,100 cc 
# for women with a standard deviation of 75 cc. Consider the sample mean of 100 
# random adult women from this population. 
# What is the 95th percentile of the distribution of that sample mean?
mu = 1100
sd = 75
n = 100
# mu = sample mean
# sd of sample = sd population / sqrt(n)
tl = mu + 1.645*sd / sqrt(n)
qnorm(.95, mu, sd / sqrt(n) )


# Question 5:
# You flip a fair coin 5 times, about what's the probability of getting 4 or 5 
# heads?
pbinom(3, size=5, prob=0.5 ,lower.tail = FALSE)
choose(5,4) * 0.5^5 + choose(5,5) * 0.5^5


# Question 6:
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for a 
# specific population has a mean of 15 (sleep events per hour) and a standard 
# deviation of 10. They are not normally distributed. Give your best estimate of 
# the probability that a sample mean RDI of 100 people is between 14 and 
# 16 events per hour?

# By CLT, sample means are distribuited normally with mean = mu and SE = sd/sqrt(n)
pmu <- 15 
psd <- 10
n <- 100
SE <- psd / sqrt(n)

ll <- pnorm(14, mean = pmu, sd = SE)
tl <- pnorm(16, mean = pmu, sd = SE)

prob <- (tl - ll) * 100

# Question 7:
# Consider a standard uniform density. The mean for this density is .5 and the 
# variance is 1 / 12. You sample 1,000 observations from this distribution and 
# take the sample mean, what value would you expect it to be near?
pmu = 0.5
psd = 1/12
n = 1000

pnorm(0.5, mean = pmu, sd = psd / sqrt(n))

# Question 8:
# The number of people showing up at a bus stop is assumed to be Poisson with a 
# mean of 5 people per hour. You watch the bus stop for 3 hours. 
# About what's the probability of viewing 10 or fewer people?
mean = 5
t = 3
ppois(10, lambda = mean*t)
