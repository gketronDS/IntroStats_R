library(ISwR)
#Power and computation of sample size
curve(pt(x,25,ncp=3), from=0, to=6)
abline(v=qt(.975, 25))

print(pt(qt(0.975, 25), 25, ncp=3))
#9.2 two sample problems
print(power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9))

print(power.t.test(n=450, delta=0.5, sd=2, sig.level = 0.01))

print(power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9, alt='one.sided'))
#9.3 One sample problems and paired test
print(power.t.test(delta=10, sd=10*sqrt(2), power=0.85, type="paired"))

#9.4 Comparison of proportions
print(power.prop.test(power=0.85, p1=.15, p2=0.3))

#9.5 Exercises
#9.1

print(power.t.test(delta=0.30, sd=0.2, sig.level = 0.05, power = 0.8))

#manual approx
print((qnorm(0.975)+qnorm(0.8))^2*2*(.2/.3)^2)
print(power.t.test(n=8, delta=0.3, sd=0.2))
d2 <- 0.3 * sqrt(2/8) / sqrt(1/6+1/10)
print(power.t.test(n=8, delta = d2, sd=0.2))

#16 only has a power of 0.53, should have 29 samples instead to have 0.8 power
#actually 0.3, 8.06 is enough. one sided only needs 7
#9.2
print(power.prop.test(power=0.8, p1=0.6, p2=0.75))
#60%-> 75% binary needs 203 patients to have 90% pow
#80 pow needs only 152

#9.3

curve(dt(x-3,25), from=0, to=7)
curve(dt(x,25,3), add=TRUE)

