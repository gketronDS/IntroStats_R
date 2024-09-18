library(ISwR)

daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
print(mean(daily.intake))
print(sd(daily.intake))
print(quantile(daily.intake))

print(t.test(daily.intake, mu=7725))

#print(wilcox.test(daily.intake, mu=7725))

#print(t.test(expend ~ stature, var.equal=T))

#print(var.test(expend ~ stature))

#print(wilcox.test(expend ~ stature))

#attach(intake)
#print(intake)

#print(t.test(pre, post, paired=T))

#print(wilcox.test(pre, post, paired=T))

#5.1
qqnorm(react)
print(mean(react))
print(sd(react))
print(quantile(react))
print(t.test(react))#
#normally distributed, not centered on 0, but -0.8 ~-1
#significantly different from 0

#5.2
print(t.test(vital.capacity ~ group, conf=0.99, data=vitcap))

#5.3
print(wilcox.test(react))

print(wilcox.test(vital.capacity ~ group, data=vitcap))

#5.4
attach(intake)
opar <- par(mfrow=c(2,2))
plot(post~pre)
abline(0,1)
plot((post+pre)/2, post-pre, ylim=range(0, post-pre))
abline(h=0)
hist(post-pre)
qqnorm(post-pre)
detach(intake)
par(opar)

#5.5
print(shapiro.test(react[-c(1,334)]))
qqnorm(react[-c(1,334)])

#5.6 

print(ashina)
attach(ashina)
print(t.test(vas.active, vas.plac, paired=T))
print(t.test((vas.active-vas.plac)[grp==1],(vas.plac-vas.active)[grp==2]))

#5.7

print(t.test(rnorm(25))$p.value)
print(t.test(rt(25, df=2))$p.value)
print(t.test(rexp(25), mu=1)$p.value)
x <- replicate(5000, t.test(rexp(25), mu=1)$p.value)
qqplot(sort(x), ppoints(5000), type='l', log='xy')