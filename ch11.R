library(ISwR)

#11.1 Plotting multivariate data

#par(mex=0.5)
#pairs(cystfibr, gap=0, cex.labels=0.9)
#gap and cex control appearance by removing space between subplots, and decreasing font size

#attach(cystfibr)

#11.2 Model specification and output
#print(summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)))

#m1 <- lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
#m2 <- lm(pemax~age)

#print(anova(m1, m2))

#11.4 Exercises
#print(summary(lm(log(bwt)~log(bpd)+log(ad),data=secher)))
#since volume scales as cubicly with lenght increases, log coeficients summing to 3 is expected to reflect this physical law. 

#11.2
#detach(cystfibr)
#tlc broken lol
#print(pairs(tlc))

#11.3
#regression coefficient gives a value to scale only women (1 vs 0 for men).

#11.4
print(summary(lm(sqrt(igf1)~age, data=juul2, subset=(age >= 25))))
print(summary(lm(sqrt(igf1)~age+height+weight, data=juul2, subset=(age >= 25))))
print(anova(lm(sqrt(igf1)~age+height+weight, data=juul2, subset=(age >= 25))))

#adding height and weight actually makes age from very to not significant, very strange


#11.5
print(summary(lm(dl.milk~ . - no, data=kfm)))
print(summary(lm(dl.milk~ . - no - mat.weight, data=kfm)))
print(summary(lm(dl.milk~ . - no - mat.weight - sex, data=kfm)))
print(summary(lm(dl.milk~ weight + mat.height, data=kfm)))

