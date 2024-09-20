library(ISwR)
#Advanced data handling
#10.1.1. cut func
#2 args number vector, and vector of breakpoints
age <- subset(juul, age >= 10 & age <= 16)$age
print(range(age))
