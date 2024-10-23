library(ISwR)

t <- 0:10 
y <- rnorm(11, mean=5*exp(-t/5), sd=.2)
plot(y ~ t)

nlsout <- nls(y ~ A*exp(-alpha*t), start=c(A=2, alpha=0.05))
print(summary(nlsout))

#16.2 Finding starting values

attach(subset(juul2, age<20 & age>5 & sex==1))
plot(height ~ age)

plot(log(5.3-log(height))~age)

print(lm(log(5.3-log(height))~age))
#supply these guesses as the intital values of the nls to converge

fit <- nls(height ~ alpha*exp(-beta*exp(-gamma*age)), start=c(alpha=exp(5.3), beta=exp(0.42), gamma=0.15))
print(summary(fit))
#overlay the fitted curve to eyeball lol

plot(height~age)
newage <- seq(5, 20, length=500)
lines(newage, predict(fit, newdata=data.frame(age=newage)), lwd=2)

#dispersion increases with age, so we try a log-scale fit. 
fit <- nls(log(height) ~ log(alpha*exp(-beta*exp(-gamma*age))), start=c(alpha=exp(5.3), beta=exp(0.12), gamma=0.12))
print(summary(fit))
plot(age, log(height))
lines(newage, predict(fit, newdata=data.frame(age=newage)), lwd=2)

#16,3 Self starting models

print(summary(nls(height~SSgompertz(age,Asym, b2, b3))))

cf <- coef(nls(height~SSgompertz(age, Asym, b2, b3)))
print(summary(nls(log(height)~log(as.vector(SSgompertz(age, Asym, b2, b3))), start=as.list(cf))))

#16.4 Profiling
par(mfrow=c(3,1))
plot(profile(fit))

print(confint.default(fit))

#16.5 Fine control of fitting
