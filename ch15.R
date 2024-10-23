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
cf <- coefficients(summary(fit2))
est <- cf[,1]
s.e. <- cf[,2]
rr <- exp(cbind(est, est-s.e.*qnorm(.975), est + s.e.*qnorm(.975)))
colnames(rr) <- c("RateRatio", "CI.lo", "CI.hi")
print(rr)

print(exp(cbind(coef(fit2), confint(fit2))))
detach()
#15.3 Computing rates
head(nickel.expand)
print(subset(nickel.expand, id==325))

nickel.expand <- within(nickel.expand, lung.cancer <- as.numeric(icd %in% c(162,163)))
attach(nickel.expand)

pyr <- tapply(ageout-agein, list(ygr,agr), sum)
print(round(pyr), na.print = "-")

count <- tapply(lung.cancer, list(ygr, agr), sum)
print(count, na.print="-")

print(round(count/pyr*1000, 1), na.print="-")

#compute expected counts in each cell based on the standard mortality table 
#and then compare to the actual counts

expect.count <- tapply(lung/1e6*(ageout-agein), list(ygr, agr), sum)
print(round(expect.count, 1), na.print="-")

#observed counts are larger than expected so we need to scale them to the overall standard mortality rate. 
#gotten from ratio of total cases to expected number of cases:family

expect.tot <- sum(lung/1e6*(ageout-agein))
print(expect.tot)
count.tot <- sum(lung.cancer)
print(count.tot)
print(count.tot/expect.tot)

#This dataset has 6 times as many cancer deaths as the general population. 

#15.4 Models with piecewise constant intensities
#Can formulate SMR as a Poisson regression model. 
#SMR asssumption is that there is a constant rate ratio to standard mortality,
#so we can fit a model with only an intercept while having an offset, 
#Which is the log of the expected count. Similar to modeling rates. Pop mortality pi 
#is just absorbed into the offset log pi + log Ti = log pi Ti. 

fit <- glm(lung.cancer ~ 1, poisson, offset = log((ageout-agein)*lung/1e6))
print(summary(fit))

#model based on individual data, dependent variable is 0 or 1.
# could make model based on cross classification of agr, and ygr, but you 
#couldnt check covariates like age at first exposure

#cant use the deviances for model checking both because the expected counts per 
#cell are very small 

print(exp(coef(fit)))

#We can analyze the data more thoroughly using regression methods. 
#check is smr stays constant over year and age groups using a multiplicative poisson model 

#this requires simplified groupings because some of the groups contain very few cases.

print(tapply(lung.cancer, agr, sum))
print(tapply(lung.cancer, ygr, sum))

#need at least 10 cases per level

detach()
nickel.expand <-within(nickel.expand,{
    A <- factor(agr)
    Y <- factor(ygr)
    lv <- levels(A)
    lv[1:6] <- "<50"
    lv[11:13] <- "70+"
    levels(A) <- lv
    lv <- levels(Y)
    lv[7:10] <- "1961ff"
    levels(Y) <- lv
    rm(lv)
})
attach(nickel.expand)

fit <- glm(lung.cancer ~ A + Y, poisson, offset = log((ageout-agein)*lung/1e6))
print(drop1(fit, test="Chisq"))
#Dont need age groups

fit <- glm(lung.cancer ~ Y - 1, poisson, offset = log((ageout-agein)*lung/1e6))
print(summary(fit))

print(round(exp(coef(fit)),1))

expect.count <- tapply(lung/1e6*(ageout-agein), Y, sum)
count <- tapply(lung.cancer, Y, sum)
print(cbind(count=count, expect=round(expect.count,1), SMR=round(count/expect.count, 1)))

detach()
nickel.expand <- within(nickel.expand, {
    TFE <- cut(agein-age1st, c(0, 20, 30, 40, 50, 100), right=F)
    AFE <- cut(age1st, c(0, 20, 27.5, 35, 100), right=F)
    YFE <- cut(dob + age1st, c(0, 1910, 1915, 1920, 1925), right=F)
    EXP <- cut(exposure, c(0, 0.5, 4.5, 8.5, 12.5, 25), right=F)
})

attach(nickel.expand)
fit <- glm(lung.cancer ~ TFE + AFE + YFE + EXP, poisson, offset = log((ageout-agein)*lung/1e6))
print(drop1(fit, test="Chisq"))
detach()
#Ex.1 
bcmort21 <- within(bcmort, { 
    period <- area <- cohort
    levels(period) <- rep(c("1991-2001", "1981-1991"), each=2)
    levels(area) <- rep(c("Cph+Frb", "Nat"), 2)
})
attach(bcmort21)

fit <- glm(bc.deaths ~ (age + period + area)^2, poisson, offset = log(p.yr), data=bcmort21)
print(summary(fit))
print(drop1(fit, test="Chisq"))
print(confint(fit, parm="period1981-1991:areaNat"))
detach()
#Ex 2
stroke.trim <- function(t1, t2) subset(transform(stroke,
    entry=t1, exit=pmin(t2, obstime),
    dead=dead & obstime <=t2),
    entry < exit)
stroke2 <- do.call(rbind, mapply(stroke.trim, c(0,0.5,2, 12), c(0.5, 2, 12, Inf), SIMPLIFY = F))
print(table(stroke$dead))
print(table(stroke2$dead))

print