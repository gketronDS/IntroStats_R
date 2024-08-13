library(ISwR)

x <- rnorm(50)
print(mean(x))
print(sd(x))
print(var(x))
print(median(x))
print(quantile((x)))
pvec <- seq(0, 1, 0.1)
print(quantile(x, pvec))

attach(juul)
print(mean(igf1, na.rm=T))
print(sum(!is.na(igf1)))
print(summary(juul))
detach(juul)
juul$sex <- factor(juul$sex, label=c("M", "F"))
juul$menarche <- factor(juul$menarche, labels=c("No", "Yes"))
juul$tanner <- factor(juul$tanner, labels=c("I", "II", "III", "IV", "V"))
attach(juul)
print(summary(juul))
#modifying a dataframe does not effect attached versions, so it must be 
#detached and reattached
#you could also have used the transform function

# juul <- transform(juul, 
#     sex=factor(sex, labels=c("M", "F")), 
#     menarche = factor(menarche, label = c("No", "Yes")), 
#     tanner = factor(tanner, labels=c("I", "II", "III", "IV", "V")))


#4.2 Graphical display of distributions
#4.2.1 Histograms, how many observations fall in each bin?
hist(x)
#specifying breaks=n in hist call, you get approx. n bars in the histogram. 
#Specifying as a number gives exact of control.
mid.age <- c(2.5, 7.5, 13, 16.5, 17.5, 19, 22.5, 44.5, 70.5)
acc.count <- c(28, 46, 58, 20, 31, 64, 149, 316, 103)
age.acc <- rep(mid.age, acc.count)

brk <- c(0, 5, 10, 16, 17, 18, 20, 25, 60, 80)
hist(age.acc, breaks=brk)

#first 3 generate pseudo-data 
#correct histogram shows where the area of the column is 
#proportional to the number 
#y-axis density units, , proportion of data per x unit. 

#raw number (frequency) freq = T. Equidistant breakpoint, + freq = F. 

#4.2.2 Emprical cumulative distribution is the fraction of data smaller than or equal to x. 
#(If x is the kth smallest observation, then the proportion k/n of the data is smaller than or equal to x)

n <- length(x)
plot(sort(x), (1:n)/n, type="s", ylim=c(0,1))

#parameter s gives the step function and ylim gives extreme y values. 
#ecdf function 


#4.2.3 QQ plots
#used to determine if the data is normally distributed by plotting 
#quantiles against expect quantiles

qqnorm(x)

#4.2.4 Box plots 
par(mfrow=c(1,2))
boxplot(IgM)
boxplot(log(IgM))
par(mfrow=c(1,1))

#4.3 Summary Stats by groups
attach(red.cell.folate)
print(tapply(folate, ventilation, mean))
print(tapply(folate, ventilation, sd))
print(tapply(folate, ventilation, length))

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
print(cbind(mean=xbar, std.dev=s, n=n))

print(tapply(igf1, tanner, mean, na.rm=T))

print(aggregate(juul[c("age", "igf1")], list(sex=juul$sex), mean, na.rm=T))

print(by(juul, juul["sex"], summary))

attach(energy)
expend.lean <- expend[stature=="lean"]
expend.obese <- expend[stature=="obese"]

par(mfrow=c(2,1))
hist(expend.lean, breaks=10, xlim=c(5,13), ylim=c(0,4), col = 'white')
hist(expend.obese, breaks=10, xlim=c(5,13), ylim=c(0,4), col = 'grey')
par(mfrow=c(1,1))

#4.4.2 Parallel boxplots 

boxplot(expend ~ stature)
boxplot(expend.lean, expend.obese)

#4.4.3 Stripcharts
opar <- par(mfrow=c(2,2), mex=0.8, mar=c(3,3,2,1)+.1)
stripchart(expend ~ stature)
stripchart(expend ~ stature, method="stack")
stripchart(expend ~ stature, method="jitter")
stripchart(expend ~ stature, method="jitter", jitter=.03)
par(opar)

#4.5 Tables 
caff.martial <- matrix(c(652, 1537, 598, 242, 36, 46, 38, 21, 218, 327, 106, 67), nrow = 3, byrow=T)
print(caff.martial)
colnames(caff.martial) <- c("0", "1-150", "151-300", ">300")
rownames(caff.martial) <- c("Married", "Prev.married", "Single")
print(caff.martial)

names(dimnames(caff.martial)) <- c("marital", "consumption")
print(caff.martial)

print(as.data.frame(as.table(caff.martial)))

print(table(sex))
print(table(sex, menarche))
print(table(menarche, tanner))
print(xtabs(~ tanner + sex, data=juul))
print(xtabs(~ dgn + diab + coma, data=stroke))
print(ftable(coma + diab ~ dgn, data=stroke))
print(t(caff.martial))

#4.5.2 Marginal tables and relative frequency 
#sometimes want sums from tables' dimensions

tanner.sex <- table(tanner, sex)
print(margin.table(tanner.sex, 2))
print(prop.table(tanner.sex, 1))
print(tanner.sex/sum(tanner.sex))

#4.6 Graphical display of tables
#4.6.1 barplots: 
total.caff <- margin.table(caff.martial, 2)
barplot(total.caff, col="white")

par(mfrow=c(2,2))
barplot(caff.martial, col="white")
barplot(t(caff.martial), col="white")
barplot(t(caff.martial), col="white", beside=T)
barplot(prop.table(t(caff.martial)), col="white", beside=T)
par(mfrow=c(1,1))

barplot(prop.table(t(caff.martial), 2), beside=T, legend.text=colnames(caff.martial), col=c("white", "grey80", "grey50", "black"))

#4.6.2 Dotchart

dotchart(t(caff.martial), lcolor = "black")

#4.6.3 Piecharts

opar <- par(mfrow=c(2,2), mex=0.8, mar=c(1,1,2,1))
slices <- c("white", "grey80", "grey50", "black")
pie(caff.martial["Married",], main = "Married", col=slices)
pie(caff.martial["Prev.married",], main = "Previously Married", col = slices)
pie(caff.martial["Single",], main = "Single", col = slices)
par(opar)