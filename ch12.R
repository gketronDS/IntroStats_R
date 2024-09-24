library(ISwR)
#12.1 Polynomical regression
attach(cystfibr)
print(summary(lm(pemax~height+I(height^2))))

pred.frame <- data.frame(height=seq(110,180,2))
lm.pemax.hq <- lm(pemax~height+I(height^2))
pp<-predict(lm.pemax.hq, interval="pred", newdata=pred.frame)
pc <- predict(lm.pemax.hq, interval="conf", newdata=pred.frame)
plot(height, pemax, ylim=c(0,200))
matlines(pred.frame$height, pp, lty=c(1,2,3), col="black")
matlines(pred.frame$height, pc, lty=c(1,3,3), col="black")

#12.2 Regression through the origin
x <- runif(20)
y <- 2*x +rnorm(20,0,0.3)
print(summary(lm(y ~ x)))
print(summary(lm(y ~ x-1)))
print(anova(lm(y ~ x)))
print(anova(lm(y ~ x-1)))

#12.3 Design matrixes and dummy variables