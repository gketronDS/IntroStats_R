library(ISwR)
library(MASS)
no.yes <- c("No", "Yes")
smoking <- gl(2,1,8,no.yes)
obesity <- gl(2,2,8,no.yes)
snoring <- gl(2,4,8, no.yes)
n.tot <- c(60,17,8,2,187,85,51,23)
n.hyp <- c(5,2,1,0,35,13,15,8)
supasmoke <- data.frame(smoking, obesity, snoring, n.tot, n.hyp)
print(supasmoke)

#R fits logistic reg in 2 ways:
#1: specify response as a matrix of column of success and one of failure
hyp.tbl <- cbind(n.hyp, n.tot-n.hyp)
print(hyp.tbl)

#then create model
hyp1 <- glm(hyp.tbl~smoking+obesity+snoring, family = binomial("logit"))

#2: give the proportion of sucesses 
prop.hyp <- n.hyp/n.tot
glm.hyp <- glm(prop.hyp~smoking+obesity+snoring, family = binomial("logit"), weights=n.tot)

print(glm.hyp)

print(summary(hyp1))

glm.hyp <- glm(prop.hyp~obesity+snoring, family = binomial("logit"), weights=n.tot)
print(summary(glm.hyp))

#analysis of deviance table

print(anova(hyp1, test="Chisq"))
hyp1 <- glm(hyp.tbl~obesity+snoring+smoking, family = binomial("logit"))
print(anova(hyp1, test="Chisq"))
#gives a deviance based test for removing smoking.
print(anova(glm.hyp, test="Chisq"))
print(drop1(glm.hyp, test="Chisq"))
#LRT = Liklihood ratio test, aka deviance change. 

#When degrees of freedom = 1 and large sample count, X^2 ~= z^2

#testing factors with more than 2 categories requires the deviance table. 
#small sample situations require special attention. 

#13.2.2 Connection to test for trends.
print(caesar.shoe)
shoe.score <- 1:6

print(summary(glm(t(caesar.shoe)~shoe.score, binomial)))
print(anova(glm(t(caesar.shoe)~shoe.score, binomial)))

#13.3 Likelihood Profiling
#Summary z tests are based on the Wald approximation, calcs what the approx std. err. of a parameter estimate
#should be if the true value of the parameters were equal to the estimates. 

#In small datasets this difference can be a lot. 

#This can mess with confidence intervals so you need to do a +-1.96x s.e. calc to do
#can do via MASS package confint which inverts the likelihood ratio test

print(confint(glm.hyp))

print(confint.default(glm.hyp))
plot(profile(glm.hyp))

print(exp(cbind(OR=coef(glm.hyp), confint(glm.hyp))))

juul$menarche <- factor(juul$menarche, labels = c("No", "Yes"))
juul$tanner <- factor(juul$tanner)
juul.girl <- subset(juul, age>8 & age<20 & complete.cases(menarche))
attach(juul.girl)
print(summary(glm(menarche~age, family = binomial)))

print(summary(glm(menarche~age+tanner, family = binomial)))

print(drop1(glm(menarche~age+tanner, binomial), test="Chisq"))

print(predict(glm.hyp))

print(predict(glm.hyp, type="response"))

plot(age, fitted(glm(menarche~age, binomial)))

glm.menarche <- glm(menarche~age, binomial)
Age <- seq(8,20,.1)
newages <- data.frame(age=Age)
predicted.probability <- predict(glm.menarche, newages, type="resp")
plot(predicted.probability ~ Age, type="l")

print(fitted(glm.hyp))
print(prop.hyp)

print(fitted(glm.hyp)*n.tot)

print(data.frame(fit=fitted(glm.hyp)*n.tot, n.hyp, n.tot))

age.group <- cut(age, c(8, 10, 12, 13, 14, 15, 16, 18, 20))
tb <- table(age.group, menarche)
print(tb)

rel.freq <- prop.table(tb,1)[,2]

print(rel.freq)

print(points(rel.freq ~ c(9,11,12.5,13.5,14.5,15.5,17,19), pch=5))

age.gr <- cut(age, c(8,12,13,14,20))
print(summary(glm(menarche~age+age.gr, binomial)))

print(anova(glm(menarche~age+age.gr, binomial)))

print(anova(glm(menarche~age+I(age^2)+I(age^3)+age.gr, binomial)))

glm.menarche <- glm(menarche~age+I(age^2)+I(age^3), binomial)

predicted.probability <- predict(glm.menarche, newages, type="resp")
plot(predicted.probability~Age, type="l")
points(rel.freq~c(9,11,12.5,13.5,14.5,15.5,17,19), pch=5)

#13.8 Exercises
#13.1 In the malaria set, analyze risk of malaria with age and log-transformed 
#antibody lvl as explanatory variables.

print(summary(glm(mal~age+log(ab), binomial, data=malaria)))

#13.2 Fit a logistic regression model to the graft.vs.host dataset, prediciting
# gvhd reponse. Use different transformations of the index variable. 
#Reduce the model using backwards elimination

attach(graft.vs.host)
type <- factor(type,labels=c("AML", "ALL", "CML"))
m1 <- glm(gvhd~rcpage+donage+type+preg+log(index), binomial)
m1a <- glm(gvhd~rcpage+donage+type+preg+index, binomial)
print(summary(m1))
print(summary(m1a))

drop1(m1, test="Chisq")
print(drop1(update(m1, ~ . -rcpage), test="Chisq"))
print(drop1(update(m1, ~ . -rcpage - type), test="Chisq"))
print(drop1(update(m1, ~ . -rcpage - type - preg), test="Chisq"))
print(summary(m2 <- glm(gvhd~donage+log(index), binomial)))

#13.3 In the analysis of the malaria and graft.vs.host data, try using the 
#confint function to find improved confidence intervals for the regression 
#coefficents.

confint(m2)
est <- coefficients(summary(m2))[,1]
se <- coefficients(summary(m2))[,2]
est + cbind(qnorm(.025)*se, qnorm(.975)*se)
print(confint.default(m2))

#13.4 Following up on Exercise 8.2 about Rocky Mountain spotted fever 
#splitting the data by age groups gives the table below. Does this confirm the 
#earlier analysis? 

counts <- c(13,40,157,40,21,61)
total <- c(108,264,375,310,181,162)
age <- gl(3,1,6)
type <- gl(2,3,6)
print(anova(glm(counts/total ~ age+type, weights=total, binomial), test="Chisq"))

#13.5 A probit regression is just like a logistic regression, 
#but uses a different link function. Try the analysis of the menarche variabl 
#in the juul dataset with this link fxn. Does the fit improve? 

juul.girl <- transform(subset(juul, age>8 & age<20 & complete.cases(menarche)), 
                                menarche=factor(menarche))

logit.menarche <- glm(menarche~age+I(age^2)+I(age^3), binomial, data=juul.girl)
probit.menarche <- glm(menarche~age+I(age^2)+I(age^3), binomial(probit), data=juul.girl)

print(summary(logit.menarche))
print(summary(probit.menarche))
Age <- seq(8,20,.1)
newages <- data.frame(age=Age)
p.logit <- predict(logit.menarche, newdata=newages, type="resp")
p.probit <- predict(probit.menarche, newdata=newages, type="resp")
matplot(Age, cbind(p.probit, p.logit), type="l")

#Questions: What is actually important to know from this chapter? 
#How is fit measured here? 