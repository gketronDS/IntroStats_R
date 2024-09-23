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


#10.2 Conditional Calculations
strokesub <- ISwR::stroke[1:10,2:3]
print(strokesub)

strokesub <- transform(strokesub, event = !is.na(died))
strokesub <- transform(strokesub, obstime = ifelse(event, died-dstr, as.Date("1996-1-1") - dstr))

print(strokesub)
#10.3 Combine and restructure dataframes
#Append frames
juulgirl <- subset(juul, sex==2, select=-c(testvol, sex))
juulboy <- subset(juul, sex==1, select=-c(menarche, sex))

juulgirl$sex <- factor("F")
juulgirl$testvol <- NA
juulboy$sex <- factor("M")
juulboy$menarche <- NA

juulall <- rbind(juulboy, juulgirl)
print(names(juulall))

print(levels(juulall$sex))

#Merge dataframes
print(head(nickel))
print(head(ewrates))

nickel <- transform(nickel, agr = trunc(agein/5)*5, ygr=trunc((dob+agein-1)/5)*5+1)

mrg <- merge(nickel, ewrates, by.x = c("agr", "ygr"), by.y = c("age", "year"))

print(head(mrg))

print(head(alkfos))

a2 <- alkfos
names(a2) <- sub("c", "c.", names(a2))

print(names(a2))

a.long <- reshape(a2, varying = 2:8, direction = "long")
print(head(a.long))
print(tail(a.long))

o<- with(a.long, order(id, time))

print(head(a.long[o,], 10))

a.long2 <- na.omit(a.long)
attr(a.long, "reshapeLong") <- NULL

a.wide2 <- reshape(a.long2, direction = "wide", v.names="c", idvar="id", timevar="time")
print(head(a.wide2))

l <- split(a.long$c, a.long$id)
print(l[1:3])

l2 <- lapply(l, function(x) x / x[1])
print(l2[1:3])

a.long$c.adj <- unsplit(l2, a.long$id)
print(subset(a.long, id==1))

#a.long$c.adj <- ave(a.long$c, a.long$id, FUN = function(x) x/x[1])

entry <- pmax(nickel$agein, 60)
exit <- pmin(nickel$ageout, 65)

valid <- (entry < exit)
entry <- entry[valid]
exit <- exit[valid]
cens <- (nickel$ageout[valid] > 65)
nickel60 <- nickel[valid,]
nickel60$icd[cens] <- 0
nickel60$agein <- entry
nickel60$ageout <- exit
nickel60$agr <- 60
nickel60$ygr <- with(nickel60, trunc((dob+agein-1)/5)*5+1)

print(head(nickel60))

#10.6
#10.1
attach(thuesen)
bloodset <- cut(blood.glucose, c(4,7,9,12,20), right=F)
levels(bloodset) <- c("low", "intermediate", "high", "very high")
print(bloodset)

#10.2
bcmort2 <- within(bcmort, { 
    period <- area <- cohort
    levels(period) <- rep(c("1991-2001", "1981-1991"), each=2)
    levels(area) <- rep(c("Cph+Frb", "Nat"), 2)
})

print(summary(bcmort2))

#10.3

print(ashina)
ashina.long <- reshape(ashina, varying=1:2, timevar = "treat", direction="long")
print(head(ashina.long))
ashina.long <- within(ashina.long, {
    m <- matrix(c(2,1,1,2), 2)
    id <- factor(id)
    treat <- factor(treat)
    grp <- factor(grp)
    period <- factor(m[cbind(grp, treat)])
    rm(m)
})
print(head(ashina.long))

#10.4
stroke.trim <- function(t1, t2) subset(transform(stroke,
    entry=t1, exit=pmin(t2, obstime),
    dead=dead & obstime <=t2),
    entry < exit)
stroke2 <- do.call(rbind, mapply(stroke.trim, c(0,0.5,2, 12), c(0.5, 2, 12, Inf), SIMPLIFY = F))
print(table(stroke$dead))
print(table(stroke2$dead))
