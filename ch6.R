library(ISwR)

#object of the chapter is to show how to perform: 
#*basic regression analysis 
#*plots for model checking and display of confidence and prediction intervals. 
#*correlation for both parametrics and non-parametric uses

#6.1 Simple Linear regression
attach(thuesen)
print(lm(short.velocity~blood.glucose))
print(summary(lm(short.velocity~blood.glucose)))

plot(blood.glucose, short.velocity)
abline(lm(short.velocity~blood.glucose))

cc <- complete.cases(thuesen)
attach(thuesen[cc,])
lm.velo <- lm(short.velocity~blood.glucose)
print(fitted(lm.velo))

plot(blood.glucose, short.velocity)

lines(blood.glucose[!is.na(short.velocity)], fitted(lm.velo))


lm.velo <- lm(short.velocity~blood.glucose)

segments(blood.glucose, fitted(lm.velo), blood.glucose, short.velocity)
plot(fitted(lm.velo), resid(lm.velo))


qqnorm(resid(lm.velo))

#6.3 Prediction and confidence bands
#confidence bands reflect uncertainty about the line itself, need to 
#establish that ybar and bhat are uncorrelated

#prediction bands include uncertainty around future observations
# limits approach withing 2 standard deviations of true line as observations increase,
#small makes curves, relies on the assumption that normally distributed errors w constant variance. 
#need to make sure its a reasonable approximation before use. 

print(predict(lm.velo))

print(predict(lm.velo, int="c"))
#confidence
print(predict(lm.velo, int="p"))
#prediction
#matlines function to add to scatterplot
pred.frame <- data.frame(blood.glucose=4:20)
pp <- predict(lm.velo, int="p", newdata=pred.frame)
pc <- predict(lm.velo, int="c", newdata=pred.frame)
plot(blood.glucose, short.velocity, ylim=range(short.velocity, pp, na.rm = T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,3), col= "black")
matlines(pred.gluc, pp, lty=c(1,3,3), col= "black")
#6.4 Correlation, 
#corr. coef. is scale invariant score of association between 2 variable
#Pearson corr. (R2)
#rooted in 2d normal distribution where theoretical correlation of zero 
# can transform to a t distribution variable to test for significance of the R2 value

print(cor(blood.glucose, short.velocity, use='complete.obs'))

print(cor.test(blood.glucose, short.velocity))

#Spearman's rho
#nonparametric variant

print(cor.test(blood.glucose, short.velocity, method='spearman'))

#Kendalls's tau
#count concordant and discordant pairs
print(cor.test(blood.glucose, short.velocity, method='kendall'))

#6.5.1
print(rmr)

fit <- lm(metabolic.rate ~ body.weight, data=rmr)

print(summary(fit))
print(predict(fit, newdata=data.frame(body.weight=70)))
print(confint(fit))

#6.2
print(summary(lm(sqrt(igf1)~age, data=juul, subset=age>25)))

#6.3
print(malaria)
print(summary(lm(log(ab)~age, data=malaria)))
plot(log(ab)~age, data=malaria)

#6.4
rho <- .9
n <- 100
x <- rnorm(n)
y <- rnorm(n, rho * x, sqrt(1-rho^2))
plot(x,y)
print(cor.test(x,y))
print(cor.test(x,y, method='spearman'))
print(cor.test(x,y, method='kendall'))
