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