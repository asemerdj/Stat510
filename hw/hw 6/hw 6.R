install.packages("permute")
library(permute)

x=rnbinom(1000,size=3,prob=.25)
mean(x);var(x)


r=3
p=0.5
x=2
n=x+r-1

a=factorial(n)/(factorial(x)*factorial(n-x))
a*((1-p)^r)*p^x

x1=rnbinom(1000,size=1,prob=.5)
dnbinom(1,size = 3,prob = .5)
dnbinom(2,size = 3,prob = .5)
