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
print(model.matrix(pemax~height+weight))
detach(cystfibr)
attach(red.cell.folate)
print(model.matrix(folate~ventilation))
detach(red.cell.folate)
#12.4 Linearity over groups
attach(fake.trypsin)

print(summary(fake.trypsin))
print(anova(lm(trypsin~grpf)))
print(anova(lm(trypsin~grp)))

model1 <- lm(trypsin~grp)
model2 <- lm(trypsin~grpf)

print(anova(model1, model2))

xbar.trypsin <- tapply(trypsin, grpf, mean)
stripchart(trypsin~grp, method="jitter", jitter=0.1, vertical=T, pch=20)
lines(1:6, xbar.trypsin, type="b", pch=4,cex=2, lty=2)
abline(lm(trypsin~grp))

n <- c(32, 137, 38, 44, 16, 4)
tryp.mean <- c(128, 152, 194, 207, 215, 218)
tryp.sd <- c(50.9, 58.5, 49.3, 66.3, 60, 14)
gr <- 1:6
print(anova(lm(tryp.mean~gr+factor(gr), weights = n)))

print(sum(tryp.sd^2*(n-1)))
print(sum(n-1))
print(sum(tryp.sd^2*(n-1))/sum(n-1))

#f stat for gr
print(206698/3318.007)
#pval
print(1-pf(206698/3318.007, 1, 265))
#F stat for factor(gr)
print(4351/3318.007)
#p val
print(1-pf(4351/3318.007, 4, 265))

#12.5 special interaction terms.
#use a colon a+b+a:b models interactions between all possible levels of factors a and b

#12.6  Two-way Anova w replication
detach(fake.trypsin)
attach(coking)

print(anova(lm(time~width*temp)))
#interaction term is significant
print(tapply(time, list(width, temp), mean))
detach(coking)
#12.7 Analysis of Covariance
print(hellung)
hellung$glucose <- factor(hellung$glucose, labels=c("Yes", "No"))
attach(hellung)

plot(conc, diameter, pch=as.numeric(glucose), log="xy")
#legend(locator(n=1), legend = c("glucose", "no glucose"), pch=1:2)

tethym.gluc <- hellung[glucose=="Yes",]
tethym.nogluc <- hellung[glucose=="No",]

lm.gluc <- lm(log10(diameter)~log10(conc), data=tethym.gluc)
lm.nogluc <- lm(log10(diameter)~log10(conc), data=tethym.nogluc)

abline(lm.gluc)
abline(lm.nogluc)

#is the difference between these slopes significant?

#12.7.2 Comparison of regression lines
print(summary(lm.gluc))
print(summary(lm.nogluc))

#manual t test of differences suggests that they are not significantly different,
#but its better to fit the entire dataset, and then check if there are equal slopes

#can be generalized to more complex models.
#this approach can also tell you the estimate of the common slope, an distance between parallel lines

print(summary(lm(log10(diameter)~log10(conc)*glucose)))

#cell cultures with glucose
# log10(D) = 1.63 - 0.053 * log10(C)

#cell cultures with no glucose
# log10(D) = (1.63 + 0.0034) - (0.053 + 0.0065) * log10(C)

#same slopes as the joint analysis

#without the interaction term and treating the lines a parallel
print(summary(lm(log10(diameter)~log10(conc)+glucose)))

#cell cultures with glucose
# log10(D) = 1.63 - 0.053 * log10(C)

#cell cultures with no glucose
# log10(D) = (1.63 - 0.0282) - 0.053 * log10(C)

#when parallel, they are offset by 0.0282

#in the non long scale, this means no glucose are 6.3% lower

#This analysis assumes equal variance which can be checked with
print(var.test(lm.gluc, lm.nogluc))#since pval = 0.67, ratio of variance is = to 1

#use bartlett's if more than 2 models to compare

print(anova(lm(log10(diameter)~log10(conc)*glucose)))

#its possible to assume the same slope or the same intercept but not both at once

print(anova(lm(log10(diameter)~log10(conc)*glucose)))

print(t.test(log10(diameter)~glucose))
detach(hellung)
#12.8 Diagnositcs
#reg diagnostics used to evaluate model assumtions and check if there are outliers with a large influence over the analysis

attach(thuesen)
options(na.action = "na.exclude")
lm.velo <- lm(short.velocity~blood.glucose)
opar <- par(mfrow=c(2,2), mex=0.6, mar=c(4,4,3,2)+0.3)
plot(lm.velo, which=1:4)
par(opar)


opar <- par(mfrow=c(2,2), mex=0.6, mar=c(4,4,3,2)+0.3)
plot(rstandard(lm.velo))
plot(rstudent(lm.velo))
plot(dffits(lm.velo), type="l")
matplot(dfbetas(lm.velo), type="l", col="black")
lines(sqrt(cooks.distance(lm.velo)), lwd=2)
par(opar)

print(summary(lm(short.velocity~blood.glucose, subset = -13)))
detach(thuesen)
attach(cystfibr)
cookd <- cooks.distance(lm(pemax~height+weight))
cookd <- cookd/max(cookd) #normalize
cook.colors <- gray(1-sqrt(cookd))
plot(height, weight, bg=cook.colors, pch=21, cex=1.5)
points(height, weight, pch=1, cex=1.5)
detach(cystfibr)

attach(secher)
rst <- rstudent(lm(log10(bwt)~log10(ad)+log10(bpd)))
print(range(rst))
rst <- rst/3.71
plot(ad, bpd, log="xy", bg=gray(1-abs(rst)), pch=ifelse(rst>0, 24, 25), cex=1.5)

#12.9 Exercises

#12.1
