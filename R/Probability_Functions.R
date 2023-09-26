#### Probability functions ####

## Simulate random draws from a particular distribution ##
# Normal distribution:
# n = number of values to simulate
x <- rnorm(n=10, mean =100, sd=10)

# Binomial distribution:
# size = number of trials, prob = probability of given outcome
y <- rbinom(n=10, size = 100, prob=0.5)

# Poisson distribution (counts of things per unit time):
# lambda = the rate parameter
z <- rpois(n=10, lambda=20)


## Calculate probability density (the probability of a given value or values,
# conditional on the parameters of a probability distribution) ##
# x = vector of quantiles
val <- 90
q <- dnorm(x = val, mean = 100, sd = 10)

## Applied examples #1: What is the probability of getting 20 heads out of 100
# trials, with a fair coin?
dbinom(x=20, size=100, prob=0.5)

# Example 2: What is the probability of counting 100 fish over the weir in an
# hour, if we expect a rate of 200/hr?
dpois(x=100, lambda=200)

## Use the cumulative probability density (CDF) to calculate the area under 
# the/to the left of/less than a given value.
pnorm(q = val, mean = 100, sd = 10)

# Visualize the CDF of a range of values
vals <- 50:150
probs <- pnorm(q=vals, mean=100, sd=10)
plot(x=vals, y=probs)

# Applied example #3: What is the probability of seeing less than 170 fish in
# an hour?
ppois(q=170, lambda=200)

# Applied example #4: What is the probability of values falling between 75 and
# 95 under a normal distribution with mean 100 and sd of 10?
pnorm(q=95, mean=100, sd=10) - pnorm(q=75, mean=100, sd=10)
# The math is different, but code is the same for Poisson distribution.


