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
