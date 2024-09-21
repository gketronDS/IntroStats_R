library(ISwR)
#Advanced data handling
#10.1.1. cut func
#2 args number vector, and vector of breakpoints
age <- subset(juul, age >= 10 & age <= 16)$age
print(range(age))
agegr <- cut(age, seq(10, 16, 2), right=F, include.lowest=T)
print(length(age))
print(table(agegr))
agegr2 <- cut(age, seq(10,16,2), right=F)
print(table(agegr2))

q <- quantile(age, c(0, 0.25, 0.5, 0.75, 1))
print(q)

ageQ <- cut(age, q, include.lowest=T)
print(table(ageQ))

levels(ageQ) <- c("1st", "2nd", "3rd", "4th")
levels(agegr) <- c("10-11", "12-13", "14-15")

#10.1.2 Manipulation factor levels
pain <- c(0, 3, 2, 2, 1)
fpain <- factor(pain, levels=0:3, labels=c("none", "mild", "medium", "severe"))

text.pain <- c("none", "severe", "medium", "medium", "mild")
ftpain <- factor(text.pain)
ftpain2 <- factor(ftpain, levels=c("none", "mild", "medium", "severe"))
ftpain3 <- ftpain2
levels(ftpain3) <- list(none="none", intermediate=c("mild", "medium"), severe="severe")
print(ftpain3)

ftpain4 <- ftpain2
levels(ftpain4) <- c("none", "intermediate", "intermediate", "severe")
print(ftpain4)

#10.1.3 Working with dates 
stroke <- read.csv2(system.file("rawdata", "stroke.csv", package="ISwR"), na.strings=".")
names(stroke) <- tolower(names(stroke))
print(head(stroke))

stroke <- transform(stroke, died = as.Date(died, format="%d.%m.%Y"), dstr = as.Date(dstr, format="%d.%m.%Y"))

print(summary(stroke$died))

print(summary(stroke$dstr))

print(summary(stroke$died - stroke$dstr))

print(head(stroke$died - stroke$dstr))

stroke <- transform(stroke, end = pmin(died, as.Date("1996-1-1"), na.rm=T), dead = !is.na(died) & died < as.Date("1996-1-1"))
print(head(stroke))

stroke <- transform(stroke, obstime = as.numeric(end - dstr, units="days")/365.25)
print(head(stroke))

rawstroke <- read.csv2(system.file("rawdata", "stroke.csv", package="ISwR"), na.strings=".")

ix <- c("DSTR", "DIED")

rawstroke[ix] <- lapply(rawstroke[ix], as.Date, format="%d.%m.%Y")
print(head(rawstroke))

ix <- 6:9
rawstroke[ix] <- lapply(rawstroke[ix], factor, levels=0:1, labels=c("No", "Yes"))

