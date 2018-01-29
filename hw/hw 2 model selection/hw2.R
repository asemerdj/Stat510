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

pima$diastolic[pima$diastolic==0] = NA
pima$glucose[pima$glucose==0] = NA
pima$triceps[pima$triceps==0] = NA
pima$insulin[pima$insulin==0] = NA
pima$bmi[pima$bmi==0] = NA

data2 <- na.omit(pima)
attach(data2)
model1 <- lm(glucose~pregnant+diastolic+triceps+insulin+bmi+diabetes+age, data=data2)
summary(model1)
step(model1)

#library(leaps) # You may have to install it first by > install.packages("leaps")
model2 <- regsubsets(glucose~pregnant+diastolic+triceps+insulin+bmi+diabetes+age,data=data2)
(rs <- summary(model2))

plot(2:8,rs$adjr2, pch=16,col=2,xlab="# of Parameters",ylab="Adjusted R^2")
text(2:8,rs$adjr2,labels=round(rs$adjr2,4),pos=1,offset=0.3,cex=0.7)
plot(2:8,rs$cp,pch=16,col=2,xlab="# of Parameters",ylab="Cp"); abline(0,1,col=4)
text(2:8,rs$cp,labels=round(rs$cp,2),pos=3,offset=0.3,cex=0.7)
model_1 <- lm(glucose~insulin,data=data2)
model_2 <- lm(glucose~insulin+age,data=data2)
model_3 <- lm(glucose~insulin+age+diastolic,data=data2)
model_4 <- lm(glucose~insulin+age+diastolic+diabetes,data=data2)
model_5 <- lm(glucose~insulin+age+diastolic+diabetes+bmi,data=data2)
model_6 <- lm(glucose~insulin+age+diastolic+diabetes+bmi+triceps,data=data2)
model_7 <- lm(glucose~insulin+age+diastolic+diabetes+bmi+triceps+pregnant,data=data2)

#library(AICcmodavg)
(myAIC <- c(AIC(model_1),AIC(model_2),AIC(model_3),AIC(model_4),AIC(model_5),AIC(model_6),AIC(model_7)))
(myAICc <- c(AICc(model_1),AICc(model_2),AICc(model_3),AICc(model_4),AICc(model_5),AICc(model_6),AICc(model_7)))
plot(2:8,myAIC,pch=16,col=2,xlab="# of Parameters",ylab="AIC")
text(2:8,myAIC,labels=round(myAIC,2),pos=1,offset=0.3,cex=0.7)
plot(2:8,myAICc,pch=16,col=2,xlab="# of Parameters",ylab="AICc")
text(2:8,myAICc,labels=round(myAICc,2),pos=1,offset=0.3,cex=0.7)

(myBIC <- c(BIC(model_1),BIC(model_2),BIC(model_3),BIC(model_4),BIC(model_5),BIC(model_6),BIC(model_7)))
plot(2:8,myAIC,pch=16,col=2,xlab="# of Parameters",ylab="AIC")
text(2:8,myAIC,labels=round(myAIC,2),pos=3,offset=0.3,cex=0.7)
plot(2:8,myBIC,pch=16,col=2,xlab="# of Parameters",ylab="BIC")
text(2:8,myBIC,labels=round(myBIC,2),pos=3,offset=0.3,cex=0.7)

#library(MASS)
boxcox(model_3)
data2$logG=log(data2$glucose)
logmodel_3 <- lm(logG~insulin+age+diastolic,data=data2)
plot(logmodel_3)


a=logLik(model_5)
a=as.numeric(a)

ll=-1799.57
(-2*a)
+16

model_6

2*(8)*(7)
392-7-2
144-383
3615.375-239

vif(model_3)
summary(model_99)
1/(1-0.1255)

model_99=lm(age~insulin+diastolic,data=data2)
summary(model_99)$r.sq
1/(1-summary(model_99)$r.sq)
vif(model_3)


influencePlot(model_3)
#anything >2 is a positive outlier, <-2 = - outlier
sum(rstudent(model_3)>2)
sum(rstudent(model_3) < -2)
which((rstudent(model_3)) < -2)
which((rstudent(model_3)) > 2)

