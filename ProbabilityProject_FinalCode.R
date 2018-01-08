##### probability project

install.packages("car")
library("car")  # to get the package
install.packages("GGally")
library("GGally")


View(Prestige)
?Prestige   # run this to understand the dataset and variales


Prestige$Occupation<-rownames(Prestige)
ggpairs(Prestige[,c(1:4)], title = "Correlation matrix")

library("dplyr")  #Creating contingency tables revealing which type of jobs fetch higher income
Prestige<-Prestige %>% mutate(income_cat=cut(income,breaks = c(0,4690,7456,25879.00),labels = c("low_in","mid_in","high_in")))
Prestige$income_cat<-as.factor(Prestige$income_cat)

Prestige<-Prestige %>% mutate(prestige_cat=cut(prestige,breaks = c(0,36.53,50.26,87.200),labels = c("low_pr","mid_pr","high_pr")))
Prestige$prestige_cat<-as.factor(Prestige$prestige_cat)


table(Prestige$type,Prestige$income_cat)
table(Prestige$type,Prestige$prestige_cat)
table(Prestige$income_cat,Prestige$prestige_cat)


Prestige <- Prestige[,1:4]  # I have decided to delete the last 2 columns to make our work easier...

dim(Prestige)
str(Prestige)



# variable prestige
######## empirical cdf of the variable Prestige
prestige.prestige.ecdf <- ecdf(Prestige$prestige)
plot(prestige.prestige.ecdf, main = "Cumulative Distribution of Prestige variable")

### confidence band
Alpha=0.05
n=length(Prestige$prestige)
Eps=sqrt(log(2/Alpha)/(2*n))
grid<-seq(0,150, length.out = 1000)
lines(grid, pmin(prestige.prestige.ecdf(grid)+Eps,1))
lines(grid, pmax(prestige.prestige.ecdf(grid)-Eps,0))

### calculating what percentage of values have a prestige between 60 and 80
estimate.prestige <- prestige.prestige.ecdf(80)-prestige.prestige.ecdf(60)
estimate.prestige
## confidence interval for the estimate
right <- estimate.prestige + qnorm(0.975)*sqrt(estimate.prestige*(1-estimate.prestige) / n)
left <- estimate.prestige- qnorm(0.975)*sqrt(estimate.prestige*(1-estimate.prestige) / n)
c(left, right)



## variable income
######## empirical cdf for the variable Income

prestige.income.ecdf <- ecdf(Prestige$income)
plot(prestige.income.ecdf, main = "Cumualtive distribution of Income variable")

### confidence band
Alpha=0.05
s=length(Prestige$income)
Eps=sqrt(log(2/Alpha)/(2*s))
grid<-seq(0,30000, length.out = 1000)
lines(grid, pmin(prestige.income.ecdf(grid)+Eps,1))
lines(grid, pmax(prestige.income.ecdf(grid)-Eps,0))

### calculating what percentage of values have a income between 5000 and 10000
estimate.income <- prestige.income.ecdf(10000)-prestige.income.ecdf(5000)
estimate.income

## confidence interval for the estimate
right <- estimate.income + qnorm(0.975)*sqrt(estimate.income*(1-estimate.income) / s)
left <- estimate.income - qnorm(0.975)*sqrt(estimate.income*(1-estimate.income) / s)
c(left, right)

par(mfrow=c(1,2))
hist(Prestige$income, breaks = seq(0,26800.00,by = 2000), col = "blue")
hist(Prestige$prestige, breaks = seq(0,88,by = 11), col = "red")

############################################### median

## median of income (AVG income in dollars in 1971)
median <- median(Prestige$income)  

## bootstrap
library(bootstrap)
B <- 2000

median.boot<-replicate(B, median(Prestige$income[sample(1:n,size=n, replace=T)]))
hist(median.boot, col = "blue", main = "Distribution of bootstrapped median incomes")

var(median.boot)
se.boot <- sd(median.boot) 

### confidence intervals
# method 1
normal.ci<-c(median-2*se.boot, median+2*se.boot)
#method 2
pivatol.ci<-c(2*median-quantile(median.boot,0.975), 2*median-quantile(median.boot,0.025))
#method 3
quantile.ci<-quantile(median.boot, c(0.025, 0.975))
normal.ci
pivatol.ci
quantile.ci
###############################################correlation
install.packages("corrplot")
library(corrplot)
M <- cor(Prestige[,1:4])
corrplot(M, method = "circle")
## correlation between prestige and income

cor.hat <- cor(Prestige$prestige, Prestige$income)
cor.hat

### confidence interaval for the correlation using bootsrap
library(bootstrap)# load the package

n <- length(Prestige$prestige)
theta <- function(x,xdata){ cor(xdata[x,2],xdata[x,4]) }

results <- bootstrap(1:n,3200,theta, Prestige)
hist(results$thetastar, col = "red", main = "Distribution of prestige and income correlation using Bootstrap")

se.boot <- sqrt(var(results$thetastar))
cor.hat <- cor(Prestige$prestige, Prestige$income)

normal.ci <- c(cor.hat-2*se.boot, cor.hat+2*se.boot)
pivatol.ci <- c(2*cor.hat-quantile(results$thetastar,0.975), 2*cor.hat-quantile(results$thetastar,0.025))
quantile.ci <- quantile(results$thetastar, c(0.025, 0.975))

normal.ci
pivatol.ci
quantile.ci

######################################women##  2017_12_06   
#Bootstrapping of Regression Coefficients
library("boot")
library("car")
#MODEL 1
fit1<-lm(income ~ education + women + prestige,data = Prestige)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)


#Education is coming to be insignificant which doesn't make sense because income is highly dependent on education level
#Possible reason is the correlation between education and prestige which is 0.8501769

#Fitting 2nd model
#Below, we are removing Education, doing log transformation of income and adding square term for prestige
#Because residual plot suggested a missing square term in the model fit1
#MODEL2: With improved residual standard error, Multiple R-squared and F statistics  
fit2<-lm(log(income) ~ women + prestige + I(prestige^2),data = Prestige)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)


#Bootstrapping 2000 times for getting 95% confidence interval for regression coefficients
boot.fun<-function(data,indices,maxit){
  data<-data[indices,]
  fit.boot<-lm(log(income) ~ women + prestige + I(prestige^2),data=data)
  coefficients(fit.boot)
}
#Boot object
Prestige.boot<-boot(Prestige,boot.fun,2000,maxit = 100)
Prestige.boot

#Distribution of bootstrap coefficients for woemn and prestige
par(mfrow = c(1,2))
hist(Prestige.boot$t[,2], main = "Distribution of bootstrapepd women coefficients", col = "blue", xlab = "women coefficient")
hist(Prestige.boot$t[,3], main = "Distribution of bootstrapepd prestige coefficients", col = "red", xlab = "prestige coefficient")

#Calculating 95% CI for coefficients of women and prestige from above done bootstrap 
women.boot.CI<-boot.ci(Prestige.boot, index=2, type=c("norm"), conf = 0.95)# women coef. 
women.boot.estimate<-mean(Prestige.boot$t[,2]) # women coef. estimate

prestige.boot.CI<-boot.ci(Prestige.boot, index=3, type=c("norm"), conf = 0.95)# prestige coef.
prestige.boot.estimate<-mean(Prestige.boot$t[,3]) # prestige coef. estimate

CI<-data.frame(rbind(women.boot.CI$normal,prestige.boot.CI$normal))
colnames(CI)<-c("Conf","2.5%_ile","97.5%_ile")
CI

#Scatter plot of coefficients of women and prestige from each bootstrap
dataEllipse(Prestige.boot$t[,2], Prestige.boot$t[,3],
xlab="women coefficient", ylab="prestige coefficient",
cex=.3, levels=c(.5, .95, .99), robust=T, main = "Scatter plot for women and prestige coefficients generated from each bootstrap")

#Above scatter plot of coefficients reveals that the true popualtion fixed coefficients beta1 and beta0
#should lie somehwere inside the cluster marked by red lines 
#where each line is a confidence level indicator


##### wald tests and permutation test

## I want to know if there is a significance difference in salary between top jobs where more than 50% are women and the jobs where less than 50% are women


median(Prestige$prestige) # even number of observations so median is (n+(n+1))/2..so in each top and bottom 50 we have 51 observations
prestige_women <- Prestige[Prestige$women > 50,] # jobs where there are more woman
prestige_man <- Prestige[Prestige$women < 50,]

## test: ho mu1 = mu 2

x1_hat <- mean(prestige_women$income )
x2_hat <- mean(prestige_man$income )

v1 <- var(prestige_women$income)/length(prestige_women$income)
v2 <- var(prestige_man$income)/length(prestige_man$income)

z <- (x1_hat-x2_hat)/sqrt(v1 + v2)
z
pnorm(z)
# which corresponds to a p-value very close to 0.... and therefore we can conclude theat there is a significance difference between the jobs where there are more women working and the jobs with more men working
View(Prestige)



### permutation test...just to make sure both tests get the same results

prestigeByWomen <- Prestige[order(Prestige$women, decreasing = TRUE),]
require(gtools)
B = 1000
perm.matrix=replicate(B, sample(length(prestigeByWomen$income)))
data.vector<-prestigeByWomen$income
perm.T<-apply(perm.matrix, 1,function(x) {abs(mean(data.vector[x[1:27]])-mean(data.vector[x[28:102]]))})
p.value=mean(perm.T>abs(mean(data.vector[1:27])-mean(data.vector[28:102])))
p.value  # same result....there is a significant difference



