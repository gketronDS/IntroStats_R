#Analysis of variance and the Kruskal Wallis Test
# comparisons of more than 2 groups (analysis of variance)
#nonparametric: w Kruskal wallis test

#2 way analysis of variance in case of one observation per cell

#7.1 One way analysis of variance
#x = mean + group deviation from mean + obs deviation from group

#null hypothesis: groups without differences when group deviation = 0
#obs deviation is assumed to be independent and have same variance
library(ISwR)
attach(red.cell.folate)
print(summary(red.cell.folate))

print(anova(lm(folate ~ ventilation)))

juul$tanner <- factor(juul$tanner, labels=c("I", "II", "III", "IV", "V"))

attach(juul)
print(summary(tanner))
print(anova(lm(igf1 ~ tanner)))

#7.1.1 Pairwise comparisions and multiple testing
print(summary(lm(folate~ventilation)))
#group 1 vs 2 or 1 vs 3 is fine, but cant do 2 vs 3 normally
#need to correct for multiple testing across these additional groups

#many tests increases the chance that one will be significant.
#bonferroni divides significance by number of tests made.  bc the probability of observing at least 1 of n events is less than the sum of probabilties for each event.
#w bonferroni, prob of a significant result is less than or equal to the formal significance levels(more conservative than shown)

print(pairwise.t.test(folate, ventilation, p.adjust.method = 'bonferroni'))
print(pairwise.t.test(folate, ventilation))

#7.1.2 Relaxing the variance assumption
print(oneway.test(folate~ventilation))
print(pairwise.t.test(folate, ventilation, pool.sd = F))

#7.1.3 Graphical representation
xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
stripchart(folate~ventilation, method='jitter', jitter=0.05, pch=16, vert=T)
arrows(1:3, xbar+sem, 1:3, xbar-sem, angle=90, code=3, length=.1)
lines(1:3, xbar, pch=4, type="b", cex=2)

#7.1.4 Bartlett's test
print(bartlett.test(folate~ventilation))

#7.2 Kruskal-Wallis Test
print(kruskal.test(folate~ventilation))

#7.3 Two-way analysis of variance
attach(heart.rate)

print(anova(lm(hr~subj+time)))

#7.3.1 Interaction plot
interaction.plot(ordered(time), subj, hr)

#7.4 Friedman test (nonparametric 2 way)

print(friedman.test(hr~time|subj, data=heart.rate))

#7.5 Anova table in regression analysis

attach(thuesen)
lm.velo <- lm(short.velocity~blood.glucose)
print(anova(lm.velo))

#7.6 Exercises
#7.1
attach(zelazo)
print(summary(zelazo))

walk <- unlist(zelazo)
group <-factor(rep(1:4, c(6,6,6,5)), labels = names(zelazo))
print(summary(lm(walk~group)))

print(t.test(zelazo$active, zelazo$ctr.8w))
print(t.test(zelazo$active, unlist(zelazo[-1])))

#7.2 
attach(lung)
print(lung)
fit <-lm(volume~subject+method)
print(anova(fit))
print(summary(fit))

#7.3
print(kruskal.test(walk~group))
print(wilcox.test(zelazo$active, zelazo$ctr.8w))
print(wilcox.test(zelazo$active, unlist(zelazo[-1])))
print(friedman.test(volume ~ method|subject, data=lung))
print(wilcox.test(lung$volume[lung$method=='A'], lung$volume[lung$method=='C'], paired=TRUE ))

#7.4
attach(juul)
tapply(sqrt(igf1), tanner, sd, na.rm=TRUE)
plot(sqrt(igf1)~jitter(tanner))
print(oneway.test(sqrt(igf1)~tanner))
#sqrt goodm, but logs get skewed, and do not impact the one-way test, and age effects them more.

