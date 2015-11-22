# Question 1
# A pharmaceutical company is interested in testing a potential blood pressure 
# lowering medication. Their first examination considers only subjects that 
# received the medication at baseline then two weeks later. 
# Consider testing the hypothesis that there was a mean reduction in blood 
# pressure? Give the P-value for the associated two sided T test.
# (Hint, consider that the observations are paired.)
data <- data.frame(baseline = c(140, 138, 150, 148, 135), 
                    week2 = c(132, 135, 151, 146, 130))
t.test(data$baseline, data$week2, alternative = "two.sided", paired = TRUE)

# Question 2
# A sample of 9 men yielded a sample average brain volume of 1,100cc and a 
# standard deviation of 30cc. 
# What is the complete set of values of mu0 that a test of H0:mu=mu0 would fail to 
# reject the null hypothesis in a two sided 5% Students t-test?
n <- 9
mu <- 1100
sd <- 30
alpha <- .05
tstat <- qt(1 - alpha/2, n - 1)
mu + c(-1, 1) * tstat * sd / sqrt(n)

# Question 3
# Researchers conducted a blind taste test of Coke versus Pepsi. 
# Each of four people was asked which of two blinded drinks given in random 
# order that they preferred. The data was such that 3 of the 4 people chose Coke.
# Assuming that this sample is representative, report a P-value for a test of 
# the hypothesis that Coke is preferred to Pepsi using a one sided exact test.
binom.test(x = 3, n = 4, p = .5, alternative = "greater") # p-value = 0.3125

# Question 4
# Infection rates at a hospital above 1 infection per 100 person days at risk 
# are believed to be too high and are used as a benchmark. A hospital that had 
# previously been above the benchmark recently had 10 infections over the last 
# 1,787 person days at risk. About what is the one sided P-value for the 
# relevant test of whether the hospital is *below* the standard?
p <- 1 / 100
pr <- 10 / 1787
n <- 1787
se <- sqrt(p * (1-p) / n)
z <- (p-pr) / se
pnorm(z, lower.tail = FALSE)

# Problem 6
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill 
# and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline 
# and again after having received the treatment or placebo for four weeks. 
# The average difference from follow-up to the baseline (followup - baseline)
# was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. 
# The corresponding standard deviations of the differences was 1.5 kg/m2 for 
# the treatment group and 1.8 kg/m2 for the placebo group. Does the change in 
# BMI appear to differ between the treated and placebo groups? Assuming 
# normality of the underlying data and a common population variance, give a 
# pvalue for a two sided t test.
n1 <- 9
n2 <- 9
df <- n1 + n2 - 2
meanTreat <- -3
meanPlacebo <- 1
sdTreat <- 1.5
sdPlacebo <- 1.8
pooledVar <- (sdTreat^2 * n1 + sdPlacebo^2 * n2)/df
se.diff <- sqrt(pooledVar/n1 + pooledVar/n2)
tstat <- (meanTreat - meanPlacebo) / se.diff
tstat
pValue <- 2 * pt(tstat, df = df)
pValue

# Question 7.
# Researchers would like to conduct a study of 100 healthy adults to detect a 
# four year mean brain volume loss of .01 mm3. Assume that the standard 
# deviation of four year volume loss in this population is .04 mm3. About what 
# would be the power of the study for a 5% one sided test versus a null 
# hypothesis of no volume loss?
n <- 100
mu <- .01
sd <- .04
power.t.test(n, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$power # 0.7989855

# Question 8
# Researchers would like to conduct a study of n healthy adults to detect a four
# year mean brain volume loss of .01 mm3. Assume that the standard deviation of
# four year volume loss in this population is .04 mm3. About what would be the 
# value of n needded for 90% power of type one error rate of 5% one sided test 
# versus a null hypothesis of no volume loss?
power <- .9
power.t.test(power = power, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$n

# Problem 10.
# As you increase the type one error rate, α, or use one-sided test instead of two-sided test,
# or increase n, power will get larger.

# Problem 11. P-value of two-sided Z-test
n <- 288
mu1 <- 44
mu2 <- 42.04
sd <- 12
se <- sd * sqrt(1/n + 1/n)
z <- (mu1 - mu2)/se
z
pValue <- 2 * pnorm(-abs(z))
pValue # 0.04999579

# Problem 12. FWER: family wise error rate
m <- 10
alpha <- .05
alphaFwer <- alpha / m
alphaFwer