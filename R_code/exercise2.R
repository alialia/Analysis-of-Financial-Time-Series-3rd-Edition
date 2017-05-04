# Set working directory
setwd("C:/Users/rmist/Documents/Analysis of Financial Time Series github/Analysis-of-Financial-Time-Series-3rd-Edition/R_data") 
library(fBasics) # Load the package fBasics

# 2.3
da=read.table("m-unrate.txt",header=T) # Load the data.
unrate = da[,4]
plot(unrate,type='l')
acf(unrate)
c1 = diff(unrate)
acf(c1)
m1=ar(c1,method='mle') # fit the data using ar model
m1$order # find the order as use it as the lag
ur.df(c1,lags=12,type=('trend'))# use unit root test to imply the existence of business cycles
# since all the roots are real valued, there is no bussinss cycle
