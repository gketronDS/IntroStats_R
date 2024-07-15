# Ch 2
# The R environment
# 
# Structure of the workspace
# graphical devices and parameters
# elementary programming
# and data entry
xxbar <- 1
ls()
#should give a list of all the variables, but doesnt really show here
#save workspace image with save.image()
#load with load()

#only contains R objects, not images or outputs
#need to use save to file for that. 
#sink("myfile") command to cut and paste in batch processing.

#better to work with scripts like this one, by using a source command
#(happens automatically in vscode R).


#graphics subsystem
x <- runif(50, 0, 2)
y <- runif(50, 0, 2)

plot(x, y, main="Main Title", sub="subitile", xlab="x-label", ylab="y-label")
text(0.6, 0.6, "text at 0.6, 0.6")
abline(h = 0.6, v= 0.6)
for (side in 1:4) mtext(-1:4, side=side, at=0.7, line=-1:4)
mtext(paste("side", 1:4), side=1:4, line = -1, font=2)

plot(x, y, type= 'n', xlab= '', ylab= '', axes=F )
points(x, y)
axis(1)
axis(2, at=seq(0.2, 1.8, 0.2))
box()
title(main="Main title", sub="subtitle", xlab="x-label", ylab="y-label")

#par settings allow control over line width, type, character size, font, #color, style of axis, size , fig regions. clipping, and subfigures

#default is 5, 4, 4,2
#combining plots

# x <- rnorm(100)
# hist(x, freq=F)
# curve(dnorm(x), add=T)

# h <- hist(x, plot=F)
# ylim <- range(0, h$density, dnorm(0))
# hist(x, freq=F, ylim=ylim)
# curve(dnorm(x), add=T)

#R Programming
#covers basic stats proceudres that can be ran on the command line. 
#wraps the above code to call from a single line: 
#hist.with.normal(rnorm(200))

hist.with.normal <- function(x, xlab=deparse(substitute(x))){
    h <- hist(x, plot=F)
    s <- sd(x)
    m <- mean(x)
    ylim <- range(0, h$density, dnorm(0, sd=s))
    hist(x, freq=F, ylim=ylim, xlab=xlab)
    curve(dnorm(x, m, s), add=T)
}

hist.with.normal(rnorm(200))
#elipses not working with vscode...

#newton's method for calculating square root. 
y <- 12345
x <- y/2
while(abs(x*x-y) > 1e-10) x <- (x + y/x)/2

print(x)
print(x^2)

# follows format of while(condition) expression which gets evaluated for as #long as the condition is true. 

x <- seq(0, 1, .05)
plot(x, x, ylab='y', type='l')
for (j in 2:8) lines(x, x^j)


weight <- c(60, 72, 57, 90, 95, 72)
print(weight)

# can do calculations as long as vectors are the same length.
height <- c(1.75, 1.8, 1.65, 1.9, 1.74, 1.91)
bmi <- weight/height^2
print(bmi)

print(t.test(bmi, mu=22.5)$p.value)

#R data editor
#ascii format > read.table
#can write these directly into emacs or vi.

#Problem 1: How to insert value using append
#?append
x <- c(50, 51, 52, 54)
x <- append(x, 53, 3)
print(x)

#without append you could use indexing. 
x <- c(50, 51, 52, 54)
print(x[1:3])
y <- c(x[1:3], 53, x[4])
print(y)
#works lol

#do a bunch of edits with text editor 
write.table(thuesen, file="IntroStats_R/foo.txt", na=".")
read.table("IntroStats_R/foo.txt", na.strings = ".")