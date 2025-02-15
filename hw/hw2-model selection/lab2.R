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
data3=na.omit(data1)
attach(data3)

model1=lm(pcgmp~.-MSA,data=data3)
summary(model1)
step(model1)
model2=regsubsets(pcgmp~.-MSA,data=data3)
(rs=summary(model2))
plot(2:6,rs$adjr2,pch=16,col=2,xlab="# of parameters",ylab="adj r2")
text(2:6,rs$adjr2,labels=round(rs$adjr2,4),pos = 1,offset = 0.3,cex = 0.7)
plot(2:6,rs$cp,pch=16,col=2,xlab="# of parameters",ylab="Cp");abline(0,1,col=4)
text(2:6,rs$cp,labels=round(rs$cp,4),pos = 1,offset = 0.3,cex = 0.7)
model_1=lm(pcgmp~prof.tech,data=data3)
model_2=lm(pcgmp~pop+ict,data=data3)
model_3=lm(pcgmp~pop+finance+ict,data=data3)
model_4=lm(pcgmp~pop+finance+ict+management,data=data3)
model_5=lm(pcgmp~pop+finance+prof.tech+management,data=data3)
(myAICc=c(AICc(model_1),AICc(model_2),AICc(model_3),AICc(model_4),AICc(model_5)))
plot(2:6,myAICc,pch=16,col=2,xlab="number of parameters",ylab="AICc")
text(2:6,myAICc,labels = round(myAICc,2),pos=1,offset=0.3,cex=0.7)
boxcox(model_4)

