library(ISwR)

message <- "Hello World!"
print(message)

plot(rnorm(1000))

#rnorm = random normal distribution.

x <- 2

print(x + x)

#1.3 Vectorized arithmetic.
#construct function c() makes vectors

weight <- c(60, 72, 57, 90, 95, 72)
print(weight)

# can do calculations as long as vectors are the same length.
height <- c(1.75, 1.8, 1.65, 1.9, 1.74, 1.91)
bmi <- weight/height^2
print(bmi)

#you can calculate on different lengths 

xbar <- sum(weight)/length(weight)

sumofsquareddeviation <- sum((weight-xbar)^2)

standarddev <- sqrt(sumofsquareddeviation/(length(weight)-1))
print(standarddev)

#One sample T-Test to assess if has a mean of 22.5 (middle of bmi range.)
print(t.test(bmi, mu=22.5))
plot(height, weight, pch=2)

hh <- c(1.65, 1.7, 1.75, 1.8, 1.85, 1.9)
lines(hh, 22.5*hh^2)

#other functions that create vectors: c(), seq(), rep()
#concatenate can assign names to values.
x <- c(red="Huey", blue="Dewey", green="Louie")
print(x)
print(names(x))

#seq() used for equidistant series of numbers

y <- seq(4,9,5)
print(y)

#often used for building graphics
#rep aka replicate, generates repeated values.
print(rep(y, 4))
#this type repeats the set number of times
print(rep(y, 1:2))
#used for group codes
print(rep(1:2, c(10,15)))

#matrix math
x<-1:12
dim(x) <- c(3,4)
print(x)

#do all that with one line
print(matrix(1:12,nrow = 3, byrow=T))
x <- matrix(1:12, nrow = 3, byrow=T)
rownames(x) <- LETTERS[1:3]
print(t(x))
#glue vectors with cbind and rbind. columnbind or rowbind.
#factors: stat data has categorical variables. input with a numerical code.
#should be specified as factors to assign meaningful names to the numbers. 
pain <- c(0,3,2,2,1)
fpain <- factor(pain, levels = 0:3)
levels(fpain) <- c("none", "mild", "medium", "severe")
print(fpain)
print(as.numeric((fpain)))
#using ordered() instead allows for ordinal variables instead of nominal. 

#Lists
intake.pre <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
intake.post <- c(3910, 4220, 3885, 5160, 5645, 460, 5265, 5975, 6790, 6900, 7335)
#you can combine muliple vectors into one with the list command, but sorted
#by subnames ex: mylist$before 
mylist <- list(before = intake.pre, after=intake.post)
print(mylist)
print(mylist$before)
#Dataframse show up as a matrix. 
d <- data.frame(intake.pre, intake.post)
print(d)
print(intake.pre[-c(3,5,7)])

#conditional selection 
print(intake.post[intake.pre > 7000])
print(intake.post[intake.pre > 7000 & intake.pre <= 8000])

d <- data.frame(intake.pre, intake.post)
print(d[1:2,])
sel <- d$intake.pre>7000
print(head(d))

# use sort to go by small to large or vise versa, order() to sort by another column.
#problems 1-5

#Problem 1
#How would you check whether two vectors are the same if they may contain
#NA values? 

x <- c(NA, 1, 5, 6)
cee <- c(NA, 1, 7, 6)
dee <- c(NA, 1, 5, 6)

print(length(x) == length(x[x==cee]))
print(length(x) == length(x[x==dee]))

#length of the vector is the same as the length of the
#vector when they share the same values

#theres probably a better way to do this... lol
#suggested answer: 
x <- y <- c(7, 9, NA, NA, 13)
print(all(is.na(x)==is.na(y)) & all((x==y)[!is.na(x)]))

#Problem 2: If x is a factor with n levels, and y is a length n vector, what
#happens when you compute y[x]

#hypothesis: will print the relative size aka "none", "low", "medium", "high"

x <- factor(c(0, 3, 2, 2, 1), levels=0:3)
levels(x) <- c("none", "low", "medium", "high")
y <- c(4, 6, 7, 8)

print(y[x])
# result: 4 8 7 7 6  It prints out 0, 3, 2, 2, 1 with the y values as the names
# y[x] is basically the same as the levels command tbh.
#"Useful forselecting plot symbols"

#Problem 3: Write the logical expression to use to extract girls between 7 and
#14 years of age in the juul data set.

d <- data.frame(juul)
print(head(d))

s <- d[((floor(d$age) >= 7)&(floor(d$age) < 14)),]
s <- s[!is.na(s$age),]
print(s)

#suggested answer forgot to sort by gender
juul.girl <- juul[juul$age >= 7 & juul$age < 14 & juul$sex == 2,]
print(summary(juul.girl))

#problem 4
# What happens if you change the levels of a factor (with levels) and give the
#same value to 2 or more levels? 
x <- factor(c(0, 3, 2, 2, 1), levels=0:3)
levels(x) <- c("none", "medium", "medium", "high")
print(x)

#treats it distinctly anyway, just cant see from the printout. 

#Problem 5
#On. p.27, replicate was used to simulate the distribution of the mean of 20 
#random numbers from the exponential distribution by repeating the operation 10
#times. How would you do the same thing with sapply?

print(replicate(10, mean(rexp(20))))

print(sapply(1:10, function(i) mean(rexp(20))))
