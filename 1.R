getwd()
setwd("/Users/MSAUSI2014/Desktop/Stats-Project-1")
##Checking the sample mean, standard deviation, sample median, min and max for mother's age and mother's weight
birthwt<-read.csv(file="birthwt.csv",head = TRUE)
colMeans(birthwt)

##Checking for NA values and removing them from the data
mothagena<-is.na(birthwt$MOTHAGE)
mothage<-birthwt$MOTHAGE[!mothagena]
new_data<-subset(birthwt, !mothagena)
premna<-is.na(birthwt$PREM)
new_data<-subset(new_data, !premna)
new_data<-subset(new_data, (new_data$BIRTHWT>0))
colMeans(new_data)


##Checking the statistics for mother's age
mean(new_data$MOTHAGE)
sd(new_data$MOTHAGE)
median(new_data$MOTHAGE)
min(new_data$MOTHAGE)
max(new_data$MOTHAGE)
##Doing the same for mother's weight
mean(new_data$MOTHWT)
sd(new_data$MOTHWT)
median(new_data$MOTHWT)
min(new_data$MOTHWT)
max(new_data$MOTHWT)

##For Premature birth
mean(new_data$PREM)
sd(new_data$PREM)
median(new_data$PREM)
min(new_data$PREM)
max(new_data$PREM)

##Number of phyysician visits
mean(new_data$PHYSVIS)
sd(new_data$PHYSVIS)
median(new_data$PHYSVIS)
min(new_data$PHYSVIS)
max(new_data$PHYSVIS)

##Factor statistics
a<-length(which(new_data$RACE==1))
a
b<-length(which(new_data$RACE==2))
b
c<-length(which(new_data$RACE==3))
c
a/(a+b+c)
b/(a+b+c)
c/(a+b+c)

a<-length(which(new_data$SMOKE==0))
a
b<-length(which(new_data$SMOKE==1))
b
a/(a+b)
b/(a+b)


a<-length(which(new_data$HYPER==0))
a
b<-length(which(new_data$HYPER==1))
b
a/(a+b)
b/(a+b)

a<-length(which(new_data$URINIRR==0))
a
b<-length(which(new_data$URINIRR==1))
b
a/(a+b)
b/(a+b)



##Plotting mother's age with mother's weight
plot(new_data$MOTHWT, new_data$BIRTHWT)
reg1<-lm(new_data$BIRTHWT~new_data$MOTHWT)
abline(reg1)
##changing column names
mothage<-new_data$MOTHAGE
mothwt<-new_data$MOTHWT
race<-factor(new_data$RACE)
smoke<-factor(new_data$SMOKE)
prem<-new_data$PREM
hyper<-factor(new_data$HYPER)
urinirr<-factor(new_data$URINIRR)
physvis<-new_data$PHYSVIS
birthwt<-new_data$BIRTHWT
##Making histogram of birth weight
library(fitdistrplus)

hist(birthwt,xlab="Birth Weight of the Child",main="Histogram of the Birth Weight",col="purple",breaks = 50,prob=TRUE)
curve(dnorm(x,mean=mean(birthwt), sd=sd(birthwt)), add=TRUE,col="green")
##Scatterplots
library(car)
scatterplot.matrix(~birthwt+prem+physvis+mothage+mothwt)


##Scatterplots of continuous variables
library(gclus)
dta<-new_data[c(1,2,5,8,9)]
dta
dta.r<-abs(cor(dta))
dta.col<-dmat.color(dta.r)
dta.o<-order.single(dta.r)
cpairs(dta,dta.o,panel.colors=dta.col,gap=0.5,main="Variables ordered and colored by correlation")


par(mfrow=c(1,2))
plot(mothage, birthwt, xlab="Mother's age during pregnancy", ylab="Birth Weight of the Baby")
plot(mothwt, birthwt, xlab="Mother's weight during pregnancy", ylab="Birth Weight of the Baby")
##Make box plots
par(mfrow=c(2,3))
boxplot(birthwt~race, xlab = "Race of the mother",ylab="Birth Weight of the Baby")
boxplot(birthwt~smoke, xlab = "Mother's Smoking Status duting pregnancy",ylab="Birth Weight of the Baby")
boxplot(birthwt~prem, xlab = "Mother's history of premature labour",ylab="Birth Weight of the Baby")
boxplot(birthwt~hyper, xlab = "Mther's history of Hypertension",ylab="Birth Weight of the Baby")
boxplot(birthwt~urinirr, xlab = "Mother's history of Urinary Irritation",ylab="Birth Weight of the Baby")
boxplot(birthwt~physvis, xlab = "Number of Physician Visits",ylab="Birth Weight of the Baby")
par(mfrow=c(1,1))
##Fitting distributions
library(fitdistrplus)
library(logspline)
cols<-which(new_data$SMOKE==0)
smoke_0<-birthwt[cols]
cols<-which(new_data$SMOKE==1)
smoke_1<-birthwt[cols]
fit.smoke_0<-fitdist(smoke_0,"norm")
plot(fit.smoke_0)
fit.smoke_1<-fitdist(smoke_1,"norm")
plot(fit.smoke_1)
##We fit a normal distribution because it fairly fits the data and can be used as our sampling distribution to calculate the means
boxplot(smoke_0,smoke_1, ylab="Birth Weight of the Child", names=c("Non-Smokers","Smokers"))
mean(smoke_0)
sd(smoke_0)
mean(smoke_1)
sd(smoke_1)

t.test(smoke_0,smoke_1,alternative = "greater")
##Since we see that the p-value is 0.003715, there is a strong evidence
##that smoking among pregnant mothers causes a lower birth weight in babies
##We use the upper tail test to test this. We notice a high p-value for the lower tail
##and 2 tailed tests. So we know that the birth weight in babies
##amongst non-smokers is much higher than those in smokers.

##Linear Regression with no 
reg1<-lm(BIRTHWT~.-BIRTHWT,data=new_data)
summary(reg1)

##Treating PREM and PHYSVIS as quantitative
reg1<-lm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+PREM+factor(HYPER)+factor(URINIRR)+PHYSVIS, data=new_data)
summary(reg1)
model1<-stepAIC(reg1, direction = "both")

##treating it as categorical
reg1<-lm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+factor(PHYSVIS), data=new_data)
summary(reg1)
model1<-stepAIC(reg1, direction = "both")
##Selecting PHYSVIS as quantitative and PREM as categorical
reg1<-lm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+PHYSVIS, data=new_data)
summary(reg1)
model1<-stepAIC(reg1, direction = "both")
AIC(model1)
BIC(model1)
summary(model1)

##Linear Model with prem and physvis as non-categorical variables
pairs(new_data)

library(leaps)
model<-leaps(x = new_data[,1:8], y = new_data[,9], names = names(new_data)[1:8], method = "Cp")
library(MASS)

##Running linear regression with all two way interactions between SMOKE and everything else except BIRTHWT
fit<-lm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+PHYSVIS + factor(SMOKE)*(MOTHAGE+MOTHWT+factor(RACE)+PREM+factor(HYPER)+factor(URINIRR)+PHYSVIS), data = new_data)
model1<-stepAIC(fit, direction = "both")
extractAIC(fit, k = log(length(fit)))
model1<-lm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+ factor(SMOKE)*MOTHAGE, data = new_data)
model1<-stepAIC(model1, direction = "both")
AIC(model1)
BIC(model1)
summary(model1)
model1$anova
formula(model1)
confint(model1)
par(mfrow=c(3,3))
plot(new_data$MOTHAGE,residuals(model1),xlab = "Age of the mother",ylab="Residuals")
abline(h=0)
plot(new_data$MOTHWT,residuals(model1),xlab = "Weight of the mother",ylab="Residuals")
abline(h=0)
plot(new_data$PHYSVIS,residuals(model1),xlab = "Physician Visits by the mother",ylab="Residuals")
abline(h=0)

boxplot(residuals(model1)~new_data$PREM,xlab = "Premature Births by the mother",ylab="Residuals")
abline(h=0)
boxplot(residuals(model1)~new_data$SMOKE,xlab = "Smoker Status of the mother",ylab="Residuals")
abline(h=0)
boxplot(residuals(model1)~new_data$RACE,xlab = "Race of the mother",ylab="Residuals")
abline(h=0)
boxplot(residuals(model1)~new_data$HYPER,xlab = "Hypertension Status of the mother",ylab="Residuals")
abline(h=0)
boxplot(residuals(model1)~new_data$URINIRR,xlab = "Urinary Infection in the mother",ylab="Residuals")
abline(h=0)

plot(model1,which = 2,5)

new_data[177,]
new_data[183,]
new_data[67,]


##Logistic Regression-- Part 4
##Binarize the birthwt
new_data$BIRTHWT
new_data$BIRTHWT<-(new_data$BIRTHWT>2500)+0

##fitting the first logistic regression
fit<-glm(BIRTHWT~ MOTHAGE+MOTHWT+factor(RACE)+factor(SMOKE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+PHYSVIS + factor(SMOKE)*(MOTHAGE+MOTHWT+factor(RACE)+factor(PREM)+factor(HYPER)+factor(URINIRR)+PHYSVIS), data = new_data, family = binomial(link="logit"))
summary(fit)
model1<-stepAIC(fit, direction = "both")
extractAIC(fit, k = log(length(fit)))
AIC(model1)
BIC(model1)
AIC(fit)
BIC(fit)
new_data$BIRTHWT
summary(model1)
model1$anova
formula(model1)
require(MASS)
exp(cbind(coef(model1), confint(model1)))
exp(coefficients(fit))
confint(model1)
exp(cbind(coef(model1),confint(model1)))


##Calculate the Odds ratio and the 95% confidence interval
require(epicalc)
logistic.display(model1)
exp(cbind(OR=coef(fit),confint(fit)))
par(mfrow=c(1,1))
##Making the classification Table


prob = predict(model1,type=c("response"))
table(new_data$BIRTHWT,prob)
library(pROC)
g<-roc(BIRTHWT~ prob, data = new_data)
plot(g)
library(Deducer)
par(mfrow=c(1,1))

rocplot(model1)

