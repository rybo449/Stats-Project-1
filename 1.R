getwd()
setwd("/Users/MSAUSI2014/Desktop/Stats Project 1")
##Checking the sample mean, standard deviation, sample median, min and max for mother's age and mother's weight
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
summary(new_data$MOTHAGE)
sd(new_data$MOTHAGE)

##Doing the same for mother's weight

mean(new_data$MOTHWT)
sd(new_data$MOTHWT)
summary(new_data$MOTHWT)

##Plotting mother's age with mother's weight
plot(new_data$BIRTHWT, new_data$MOTHAGE)
reg1<-lm(new_data$MOTHAGE~new_data$BIRTHWT)
abline(reg1)
##changing column names
mothage<-new_data$MOTHAGE
mothwt<-new_data$MOTHWT
race<-new_data$RACE
smoke<-new_data$SMOKE
prem<-new_data$PREM
hyper<-new_data$HYPER
urinirr<-new_data$URINIRR
physvis<-new_data$PHYSVIS
birthwt<-new_data$BIRTHWT
##Making sample histograms
hist(race)
hist(smoke)
hist(prem)
hist(urinirr)
hist(physvis)
plot(smoke, birthwt)

### Sample two tailed hypothesis testing

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
