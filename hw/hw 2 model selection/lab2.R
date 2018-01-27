if(!require(faraway)){
  install.packages("faraway")
  library(faraway)
}
if(!require(leaps)){
  install.packages("leaps")
  library(leaps)
}
if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}
if(!require(AICcmodavg)){
  install.packages("AICcmodavg")
  library(AICcmodavg)
}
if(!require(car)){
  install.packages("car")
  library(car)
}


data1=read.csv("http://www.stat.cmu.edu/~cshalizi/mreg/15/lectures/03/bea-2006.csv")
data2=na.omit(data1)
attach(data2)
model1=lm(pcgmp~.-MSA,data=data2)
summary(model1)
step(model1)
