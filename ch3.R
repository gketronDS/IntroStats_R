library(ISwR)
# Ch 3
# Probability and distributions
# 
# Most Experiments are not perfectly reproducable, 
#so accuracy, distributions and probability are important to know
#
#learn basic probability and functions in R for random sampling and 
#handling distribtuions
#
#Simulate card shuffling with the sample function
print(sample(1:40, 5))
#defaults to sample w/o replacement
#replace = T, can simulate coin tosses
print(sample(c("H", "T"), 10, replace = TRUE, prob = NULL))
#prob arg uses to assign nonequalt probabilities
print(sample(c("Success", "Failure"), 10, replace = TRUE, prob = c(0.9, 0.1)))
#for two outcomes, you should actually use a binomial distribution
#prod fxn gives product of a vector 
print('chance of the sample(1:40, 5) being selected')
print(1/prod(40:36))

print('chance of the sample(1:40, 5) being selected if order doesnt matter')
print(prod(5:1)/prod(40:36))
#this function is equivalent to n(40) choose k(5) how to calculate 
#the number of ways to choose 5 out of 40. "from 40 choose 5" "from n choose k"
#(40) = 40! / (5!*35!) = 658008
#(5)
#use the choose fxn:
print(1/choose(40, 5))

#Discrete distributions
#integers or classes only selected for the random variable 
#Random variable X has a probability  distribution using point probas
#f(x)=P(X=x) or cumulative distribution fxn F(x)=P(X=<x)
#Discrete gives dist without point probas:
#f(x) = (n choose x) p^x*(1-p)^(n-x)
#aka binomial dist, n x binomial coefs
#p = prob of success outcome 
#since there is 0 chance of infinate points from getting selected, 
#you need a probability density function
#F(x)=  S x to -inf f(x) dx
#uniform dist has a constant probability density. 
#normal distribution: 
#f(x) = 1/sqrt(2*pi*stddev) * e^(-(x-mean(x))^2/(2*stddev^2))
#gives a bell shape 

#four items can be calulated for a distribution: 
#*Density/point proba (dnorm)
#Cumulative probability/dist fxn (pnorm)
#quantiles (qnorm)
#pseudo random numbers (rnorm)

#bell curve density dist. 

x <- seq(-4, 4, 0.1)
plot(x, dnorm(x), type='l')

#pin diagram for distinct values
#binomial dist with n=50, p=0.33

x <- 0:50
plot(x, dbinom(x, size=50, prob=.33), type='h')

#cumulative dist fxn

print(1-pnorm(160, mean = 132, sd=13))

print(1-pbinom(15, size = 20, prob=.5))

print(1-pbinom(15, size = 20, prob=.5)+pbinom(4, 20, .5))

#quantiles-inverse of the cumulative dist fxn, used to calculate 
#confidence intervals or power calculations in experimental design
#
#n normally dist observations, with the same mean and std dev
#average reading xbar will be around pop mean, 
#with std dev of pop standard dev/sqrt(n)

#95% confidence interval for pop mean: 
#xbar + sigma/sqrt(n)*qnorm(0.025)=< u =< xbar+sigma/sqrt(n)*qnorm(0.975)

#0.025 = 2.5% quantile, 0.975 = 97.5% quantile

xbar <- 83
sigma <- 12
n <- 5
sem <- sigma/sqrt(n)
print(sem)
print(xbar + sem*qnorm(0.025))
print(xbar + sem*qnorm(0.975))

#quantiles are also used for q-q plots, used to asses if a set of data 
#can be assumed to come from a certain distribution

print(rnorm(10))
print(rnorm(10, mean = 7, sd=5))
print(rbinom(10, size=20, prob=.5))

#q1: what is the probability of: 
#a) a standard normally distributed variable is larger than 3? yes
print(1 - pnorm(3)) 
#b) a normally distributed variable with mean of 35 and standard deviation 6 is larger than 42 yes
print(1-pnorm(42, mean=35, sd = 6))
#c) getting 10 out of 10 sucesses in a binomial distribution with a probability of 0.8
#print(pbinom(9, size = 10, prob = 0.8))#wrong
print(dbinom(10, size = 10, prob = 0.8))#correct
#d)X < 0.9 when X has the standard uniform distribution yes
print(punif(0.9))
#e)X > 6.5 in a chisquared distribution with 2 degrees of freedom. yes
print(1-pchisq(6.5, 2))


print(1-dbinom(1, size = 100, prob = 0.01))
x <- 0:1000
plot(x, 1-pbinom(1, size=x, prob=.01), type='h')
print('check')

#2. 5% of the normal distribution lies outside 2 standard deviations
#is this true? what about 1%? 0.5%, and 0.1%? 
#what are the quantile positions meaured in statistical deviations? 
print(pnorm(-2)*2)
print(qnorm(1-.01/2))
print(qnorm(1-.005/2))
print(qnorm(1-.001/2))
print(qnorm(0.25))
print(qnorm(0.75))
#3.3
print(dbinom(0, size = 10, prob = 0.2))

#3.4 simulate cointoss with rbinom
print(rbinom(10, size=1, prob = 0.5))