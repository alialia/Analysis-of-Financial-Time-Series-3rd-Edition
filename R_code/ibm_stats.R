# Set working directory
setwd("C:/Users/rmist/Documents/Analysis of Financial Time Series github/Analysis-of-Financial-Time-Series-3rd-Edition/R_data")
library(fBasics) # Load the package fBasics
da=read.table("d-ibm3dx7008.txt",header=T) # Load the data.
# header=T means 1st row of the data file contains
# variable names. The default is header=F, i.e., no names.
dim(da)
da[1,] # See the first row of the data
ibm=da[,2] # Obtain IBM simple returns
sibm=ibm*100 # Percentage simple returns
basicStats(sibm) # Compute the summary statistics
mean(sibm)
var(sibm)
sqrt(var(sibm)) # Standard deviation
skewness(sibm) 
kurtosis(sibm) # Excess kurtosis; To get kurtosis,use kurtosis(sibm,method='moment')
# Simple tests
s1=skewness(sibm)
t1=s1/sqrt(6/9845) # Compute test statistic
t1
pv=2*(1-pnorm(t1)) # Compute p-value.
pv
# Turn to log returns in percentages
libm=log(ibm+1)*100
t.test(libm) # Test mean being zero.
# The result shows that the hypothesis of zero expected return
# cannot be rejected at the 5# or 10# level.

normalTest(libm,method='jb') # Normality test
# The result shows the normality for log-return is rejected