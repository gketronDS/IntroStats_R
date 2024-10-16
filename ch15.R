library(ISwR)
# Epidemiology studies calculate rates, of death or incidence rates of a chronic
# or acute disease. Based on counting the events occuring in a certain time-frame
# Poisson regrssion used to analyze rate data. Data that are amounts of time until
# an event can be analyzed by a similair technique

# 15.1 Basic idea
# 2 forms: aggregate of observed count x based on number of person-years T. 
# (approx with tables of population size), and with grouping, which we may want 
# to compare differences in rates. Individual level data, in which each subject has 
# a time under observation Ti and an indicator 0 or 1 xi if the subject had the 
# event. x = Sum(xi)  T = Sum(Ti), where the sums are over all individuals in the 
# group. 

#15.1.1 Poisson Distribution
# Poisson dist can be described as the limiting case of the binomial distrbutions 
# when size parameter N increases while the expected number of successes l = Np is
# fixed. Used to describe rare events in large populations (like incomeinequality)
# dist of point prob: f(x) = (l^x / x!) * e^-l where x = 0,1,...

# dist is unbounded, but probability of large x is low. dpois, ppois for this. 

# Epidemology: interesting value is counts per observed timespan as x. 
# (rate of events) can compare different sizes and differnt time lengths 
# parameterize the dist with rho = l / T

# Possion Likelihood: fit models with maximum likelihood. If we parameterize by 
# rho, the log-likelihood becomes: L(rho) = constant + x log rho - rhoT
# which is maximized when rho = l/T. 

#15.1.2 Survival analysis with constant hazard
# mortality study terms. but could be other events. 
# individual data is basically survival data like in ch14, but with changes in 
# notation. Rate analysis assumes that hazard does not change over time, or at 
# least not abruptly. rates obtained over short time period, and origin of 
# timescale usually not keyed to disease onset or major surgery. if constant 
# hazard, then dist of lifetime is an exponential dist with density rho*e^-l and
# survival fxn e^-l

# Likelihood analysis
# censor data likelihood can be buit with terms or probability dist at time of 
# death or survival prob in case of censoring. W constant hazard, only 2 factors 
# differ with rho, which can be encoded into xi, so the log likelihood terms become
# L(rho) = xi*log(rho) - rho*Ti
# Except for the constant, this is the same as poisson. xi = 1 if dead, 0 if 
# censoring only. this trick allows survial data to be analyzed by poisson. 

# Can be extended out to hazards that are peicewise constant or about constant for
# the viewing period. break lifetime into Ti(1), Ti(2)...
# where hazard is constant in each section. 
# likelihood term is: l(rho1, rho2, ..rhoi)= Sum j = 1 to k (xi(j) log (rhoj)-rhoj*Ti(j))
# k-1 = j will have xi = 0. only the last is 1. 

# Baiscally the same as if it was k other individuals who were censored. 
# This is the rationale behind time splitting techniques where one observation is
# broken into observations of multiple pseudoindividuals. 

# *note that you cant pretend that the data comes from a poisson dist. Difference
# is to what extent the random variation lies in the counts or the amount of time. 
# When data is frequently censored, (event is rare) the survival model approximates
# poisson

#15.2 Fitting Poisson models
# glm also includes poisson, which uses log link by default. fromulates models in 
# terms of log rho = b0 + b1x1 + ... + bkxk + log T
# (since glm needs a model for the expected counts rather than rates), log T 
# needed as an offset

print(names(eba1977))
attach(eba1977)
#lung cancer by age in 4 danish cities in 1977
# fit a model with multiplicative effects of age and city on the rate of lung cancer.
fit <- glm(cases~city+age+offset(log(pop)), family = poisson)
print(summary(fit))

#coefficents indicate differences in log of rate ratio between city of 
#fredericia and 50-54 yo. intercept gives the log rate ratio of 50-54 yo in fredericia

#goodness of fit stat given by comparison of residual variance with chisq dist
#on the stated deg of freedom. Considered valid if expected count in all cells is greater than 5.

print(min(fitted(fit)))
print(pchisq(deviance(fit), df.residual(fit), lower=F))

#if pchisq was less than 0.05, the model would not be fititng the data well.

#There is an age effect, but what about a city effect? DO chisq test with drop1,
#on each term and see how the deviance changes.

print(drop1(fit, test='Chisq'))

#city not significant, but it was in the model. Maybe a paricular city has a higher propensity.

fit2 <- glm(cases~(city=="Fredericia")+age, offset = log(pop), family = poisson)

print(anova(fit, fit2, test="Chisq"))
#anova not significantly differnet?

print(drop1(fit2, test="Chisq"))
#just fredericia is sig
print(summary(fit2))

#fit 2 is all significant



