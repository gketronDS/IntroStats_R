library(ISwR)

daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
print(mean(daily.intake))
print(sd(daily.intake))
print(quantile(daily.intake))

print(t.test(daily.intake, mu=7725))

print(wilcox.test(daily.intake, mu=7725))

print(t.test(expend ~ stature, var.equal=T))

print(var.test(expend ~ stature))

print(wilcox.test(expend ~ stature))

attach(intake)
print(intake)

print(t.test(pre, post, paired=T))

print(wilcox.test(pre, post, paired=T))
