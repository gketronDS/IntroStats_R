#14 Survival Analysis
#important in bio, med, and reliability in engr. Data not normally distributed
#linear models suck for it. Also censored bc you dont know the lifetime, only that its longer than the current value.
#
#
#14.1 Essential Concepts: X = true lifetime. T = censoring time. 
# T can be random or fixed. If random, then it should be noninformative for these methods. 
# Dead from other causes is a censoring event for the given disease mortality.
#
#Survival function S(t) measures prob of being alive at a given time t. aka 1 - cumulative dist function for X. 
# S(t) = 1 - F(t) 
#hazard fxn h(t) measures the infitesimally small risk of dying with the timestep t. given the subject is alive at time t.
# if lifetime dist = desity f: h(t) = f(t)/S(t)
#this is a more fundamental quantity than mean or median of survival distribution and is the basis for further models

#14.2 Survival Objects
library(ISwR)
library(survival)

attach(melanom)
print(names(melanom))

temp <- Surv(days, status==1)
print(temp)

#kaplan-meier estimates 
#allows for computing of an estimate survival function in the presence of right-censoring. 
#aka product-limit estimator because it multiplies conditional survival curves for intervals in which there are no censored obs. 
# step function where the estimated surival is reduced by a factor 1 - 1/Rt if there is a death at time t and population of Rt still alive an uncensored at the time.

#function survfit. 
print(survfit(Surv(days, status==1)~1))
surv.all <- survfit(Surv(days, status==1)~1)

print(summary(surv.all))

plot(surv.all)

surv.bysex <- survfit(Surv(days, status==1)~sex)
plot(surv.bysex, conf.int=T, col=c("black", "grey"))

#14.4 Log-rank test
# Used to test if 2 or more surivial curves are idenitcal. 
#Look at pop at each death time and computing the expected number of deaths 
#in proportion to the number of at risk individuals. then summed over all death 
#times compared w observed deaths in chisq dist. if differece is sufficientily large, then expectation to see individuals die multipe times. 
#If observed to extinction expected values will contian all variation. 

#log rank test is formally nonparametric since the distribution of the test 
#stat depends only on assuming groups have the same surival fxn. 

#needs proportional hazards. 
print(survdiff(temp~sex))

#can also do stratified analysis
print(survdiff(temp~sex+strata(ulc)))

#14.5 Cox proportional hazards model
print(summary(coxph(temp~sex)))
print(summary(coxph(temp~sex+log(thick)+strata(ulc))))
plot(survfit(coxph(temp~log(thick)+strata(ulc)+sex)), conf.int=T, col=c("black", "grey"))

#14.6 Exercises

#1 
attach(graft.vs.host)
graftdead <- Surv(time, dead)
plot(survfit(graftdead~gvhd))
print(survdiff(graftdead~gvhd))
print(summary(coxph(graftdead~gvhd)))
print(summary(coxph(graftdead~gvhd+log(index)+donage+rcpage+preg)))
plot(survfit(graftdead~preg))

#2
attach(melanom)
cox1 <- coxph(temp~log(thick)+strata(ulc)+sex)
new <- data.frame(sex=2, thick = c(0.1, 0.2, 0.5))
svfit <- survfit(cox1, newdata=new)
plot(svfit[2], ylim=c(.985, 1))

#3 


#print(summary(coxph(Surv(obsmonths, dead)~age+sex, data=stroke)))
#print(summary(coxph(Surv(obsmonths, dead)~sex, data=stroke)))
#print(with(stroke, tapply(age,sex,mean)))

#4 
print(summary(coxph(Surv(entry, exit, dead)~age+sex, data=stroke2)))