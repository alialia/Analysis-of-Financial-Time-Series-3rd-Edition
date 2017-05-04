#chapter 2 example
# R packages used: fBasics, fUnitRoots, timeSeries (fSeries), TSA
# Set working directory
setwd("C:/Users/rmist/Documents/Analysis of Financial Time Series github/Analysis-of-Financial-Time-Series-3rd-Edition/R_data")
library(fBasics) # Load the package fBasics
library(ggplot2)
da=read.table("m-ibm3dx2608.txt",header=T) # Load the data.
sibm = da[,2]
libm = log(1+sibm)
Box.test(sibm,lag=5,type='Ljung') # Use Ljung-Box statistic Q(5) to test autocorrelation
Box.test(libm,lag=5,type='Ljung')
acf(sibm)
acf(libm)
vw = da[,3]
log_vw = log(1+vw)
acf(vw)
acf(log_vw)

# Example 2.1
gnp=scan(file='dgnp82.txt') # Load data
# To create a time-series object
gnp1=ts(gnp,frequency=4,start=c(1947,2))
plot(gnp1)
points(gnp1,pch='*')
m1=ar(gnp,method='mle') # Find the AR order
m1$order #An AR(3) is selected based on AIC
m2=arima(gnp,order=c(3,0,0)) # Estimation
m2
# In R, ‘‘intercept’’ denotes the mean of the series.
# Therefore, the constant term is obtained below:
(1-.348-.1793+.1423)*0.0077
sqrt(m2$sigma2) # Residual standard error
p1=c(1,-m2$coef[1:3]) # Characteristic equation
roots=polyroot(p1) # Find solutions
roots
Mod(roots) # Compute the absolute values of the solutions
# To compute average length of business cycles:
k=2*pi/acos(1.590253/1.913308)
k
gnp=scan(file='q-gnp4791.txt')
ord=ar(gnp,method='mle')
ord$aic
ord$order
vw=read.table('m-ibm3dx2608.txt',header=T)[,3]
t1=prod(vw+1)
t1
t1^(12/996)-1
m3=arima(vw,order=c(3,0,0))
m3
(1-.1158+.0187+.1042)*mean(vw) # Compute the intercept phi(0).
sqrt(m3$sigma2) # Compute standard error of residuals
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.35,9) # Compute p-value using 9 degrees of freedom
pv
# To fix the AR(2) coef to zero:
m3=arima(vw,order=c(3,0,0),fixed=c(NA,0,NA,NA))
# The subcommand ’fixed’ is used to fix parameter values,
# where NA denotes estimation and 0 means fixing the parameter to 0.
# The ordering of the parameters can be found using m3$coef.
m3
(1-.1136+.1063)*.0089 # Compute phi(0)
sqrt(m3$sigma2) # Compute residual standard error
Box.test(m3$residuals,lag=12,type='Ljung')
pv=1-pchisq(16.83,10)
pv
ew=read.table('m-ibm3dx2608.txt',header=T)[,4]
m4 = arima(ew,order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))
m4
library(urca) # library(fUnitRoots) is not available
da=read.table("q-gdp4708.txt",header=T)
gdp=log(da[,4])
m1=ar(diff(gdp),method='mle')
m1$order
ur.df(gdp, lags = 10, type = 'drift') # 'drift' corresponding to 'c' in fUnitRoots
                                      # 'trend' = 'ct'
# df is -1.6109, same as fUnitRoots
# p value is not reported, but summary  reports critical value table (1%, 5%, 10%)
# compared to that table, p-value must be larger than 10%
summary(ur.df(gdp, lags = 10, type = 'drift'))

da=read.table("d-sp55008.txt",header=T)
sp5=log(da[,7])
m2=ar(diff(sp5),method='mle')
m2$order
ur.df(sp5,lags=2,type=('trend'))
summary(ur.df(sp5,lags=2,type=('trend')))
ur.df(sp5,lags=15,type=('trend'))
summary(ur.df(sp5,lags=15,type=('trend')))
# Example 1.4
da=read.table("m-deciles08.txt",header=T)
d1=da[,2]
jan=rep(c(1,rep(0,11)),39) # Create January dummy.
m1=lm(d1~jan)
summary(m1)
m2=arima(d1,order=c(1,0,0),seasonal=list(order=c(1,0,1),period=12))
m2
# section 2.9
r1=read.table("w-gs1yr.txt",header=T)[,4]
r3=read.table("w-gs3yr.txt",header=T)[,4]
m1=lm(r3~r1)
summary(m1)
plot(m1$residuals,type='l')
acf(m1$residuals,lag=36)
c1=diff(r1)
c3=diff(r3)
m2=lm(c3 ~-1+c1)
summary(m2)
acf(m2$residuals,lag=36)
rsq=(sum(r3^2)-sum(m1$residuals^2))/sum(r3^2) # compute R^2
rsq
m3=arima(c3,order=c(0,0,1),xreg=c1,include.mean=F) # MA(1) model
m3
rsq=(sum(c3^2)-sum(m3$residuals^2))/sum(c3^2) # compute R^2
rsq
