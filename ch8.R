library(ISwR)
#Ch 8 Tabular Data
#analyze tabular data, fxns: prop.test, binom.test, chisq.test, fisher.test

#8.1 Single Proportions
print(prop.test(39,215,0.15))
#39 of 215 patients have asthma. Is the true percent of people with asthema 15%?
#confidence interval usually a more interesting result for this fxn

print(binom.test(39,215,0.15))

#8.2 Two independent proportions
#prop can compare two or more
lewitt.machin.success <- c(9,4)
lewitt.manchin.total <- c(12,13)

print(prop.test(lewitt.machin.success, lewitt.manchin.total))

lewitt.machin <- matrix(c(9,4,3,9), 2)
print(fisher.test(lewitt.machin))

print(chisq.test(lewitt.machin))
#8.3 k proportions, test for trend

caesar.shoe.yes <- caesar.shoe["Yes",]
caesar.shoe.total <- margin.table(caesar.shoe, 2)
print(prop.test(caesar.shoe.yes, caesar.shoe.total))

print(prop.trend.test(caesar.shoe.yes, caesar.shoe.total))

#8.4 r x c tables

#row totals fixed in advance, test for if the distribution over columns is the same for each row, or vice versa if columns are fixed. 
#may also want to check to see if there is statistical independence between row and column. 

caff.martial <- matrix(c(652, 1537, 598, 242, 36, 46, 38, 21, 218, 327, 106, 67), nrow = 3, byrow=T)
colnames(caff.martial) <- c("0", "1-150", "151-300", ">300")
rownames(caff.martial) <- c("Married", "Prev.married", "Single")
print(caff.martial)
print(chisq.test(caff.martial))

#significant, so we can conclude that the data contradicts the hypothesis of independednce. (rows or columns are not independent? i dont get this)
print(chisq.test(caff.martial)$expected)

E <-chisq.test(caff.martial)$expected
O <-chisq.test(caff.martial)$observed

print((O-E)^2/E)

#not easy to describe where the deviation from statistical independence is

#8.5 Exercises
#8,1
print(dbinom(0, size = 10, prob = 0.2))
print(binom.test(0,10,0.2, alternative = 'less'))
print(binom.test(0,13,0.2, alternative = 'less'))
print(binom.test(0,14,0.2, alternative = 'less'))

#8.2
print(prop.test(c(210,122), c(747, 661)))
#yes there is a significant difference between these two groups

#8.3
drugs <- matrix(c(23,7,18,13),2,2)
print(fisher.test(drugs))
print(chisq.test(drugs))
print(prop.test(drugs))

# -0.08462185  0.50657307 confidence interval, no sig dif in healing ability

#8.4 
tbl <- c(42, 157, 47, 62, 4, 15, 4, 1, 8, 28, 9, 7)
dim(tbl)<-c(2,2,3)
dimnames(tbl)<- list(c("A", "B"), c("not pierced", "pierced"), c("ok", "broken", "cracked"))
ftable(tbl)
print(fisher.test(tbl["B",,]))
print(fisher.test(tbl["A",,]))
print(fisher.test(margin.table(tbl,2:3)))
#no sig diff

#8.5
p <- seq(0, 1, 0.001)
pval <- sapply(p, function(p) binom.test(3,15,p=p)$p.value)
plot(p, pval, type='l')
