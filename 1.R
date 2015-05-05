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

a<-length(which(new_data$PREM==0))
a
b<-length(which(new_data$PREM==1))
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

a<-length(which(new_data$PHYSVIS==0))
a
b<-length(which(new_data$PHYSVIS==1))
b
c<-length(which(new_data$PHYSVIS==2))
c
d<-length(which(new_data$PHYSVIS==3))
d
e<-length(which(new_data$PHYSVIS==4))
e
f<-length(which(new_data$PHYSVIS==5))
f
g<-length(which(new_data$PHYSVIS==6))
g
a/(a+b+c+d+e+f+g)
b/(a+b+c+d+e+f+g)
c/(a+b+c+d+e+f+g)
d/(a+b+c+d+e+f+g)
e/(a+b+c+d+e+f+g)
f/(a+b+c+d+e+f+g)
g/(a+b+c+d+e+f+g)


##Plotting mother's age with mother's weight
plot(new_data$BIRTHWT, new_data$MOTHAGE)
reg1<-lm(new_data$MOTHAGE~new_data$BIRTHWT)
abline(reg1)
##changing column names
mothage<-new_data$MOTHAGE
mothwt<-new_data$MOTHWT
race<-factor(new_data$RACE)
smoke<-factor(new_data$SMOKE)
prem<-factor(new_data$PREM)
hyper<-factor(new_data$HYPER)
urinirr<-factor(new_data$URINIRR)
physvis<-factor(new_data$PHYSVIS)
birthwt<-new_data$BIRTHWT
##Making histogram of birth weight
hist(birthwt)
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

##Hypothesis testing t-test
ttest<-t.test(birthwt~smoke)
names(ttest)
ttest$statistic
ts = replicate(1000, t.test(birthwt,smoke)$statistic)
range(ts)
pts = seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='1')
lines(density(ts))
### Sample two tailed hypothesis testing
##check if sample mean of smokers is different from the sample mean of non smokers
xbar = mean(birthwt)
mu0 = median(birthwt)
sigma = sd(birthwt)
n = length(birthwt)
z = (xbar-mu0)/(sigma/sqrt(n))
z
alpha = 0.05
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha,z.half.alpha)
##We notice that the value of z is -0.609, which is in between the 95% confidence interval of -1.95 and +1.95 so we do not
##reject the null hypothesis

reg1<-lm(birthwt~new_data$BIRTHWT)

##Linear Model
fit<-lm(birthwt~mothage+mothwt+factor(race)+factor(smoke)+factor(hyper)+factor(urinirr)+factor(prem)+factor(physvis))
summary(fit)
plot(prem,birthwt)
abline(fit)
pairs(new_data)

library(leaps)
model<-leaps(x = new_data[,1:8], y = new_data[,9], names = names(new_data)[1:8], method = "Cp")
model<-update(fit, .~. - mothwt)
extractAIC(model, k = log(n))
library(MASS)
fit<-lm(birthwt~ . + smoke*(.-smoke), data = new_data)
model1<-stepAIC(fit, direction = "both")
AIC_selected<-
AIC(model1)
BIC(model1)
model1
summary(model1)
model1$anova
abline(model1)
plot(mothage, birthwt)

aic_data<-data.frame(race, smoke,hyper,urinirr, prem, birthwt)
pairs(aic_data)
