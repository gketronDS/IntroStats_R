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




