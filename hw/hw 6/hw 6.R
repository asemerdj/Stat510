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


setwd("U:/Stat510/Stat510/hw")
data1=read.csv("./sampleData/gss.csv")
data2=data1[complete.cases(data1$childs),]
data3=data2[complete.cases(data2$female),]
data4=data3[complete.cases(data3$nonwhite),]
data5=data4[complete.cases(data4$educate),]
data6=data5[complete.cases(data5$income),]
search()
attach(data6)


install.packages("MASS")
library(MASS)
model3=glm(childs~female+nonwhite+educate+income,data=data6,family="poisson")
model4=glm(childs~female+nonwhite+educate+income,data=data6,family="quasipoisson")
model5=glm.nb(childs~female+nonwhite+educate+income,data=data6)

summary(model3)
summary(model4)
summary(model5)

first=read.csv("./sampleData/firstsex.csv")
end=subset(first,first$firstsex==999)

mean(first$firstsex);median(first$firstsex)

install.packages("survival")
library(survival)


hist(first$firstsex,main="histogram",xlab="months after 12th birthday")



firstsex1 <- Surv(first$firstsex, first$eversex==1)

survfit(firstsex1~1)
model1 <- survfit(firstsex1~1, data=first)
plot(model1)
legend(locator(1),c("momdad=0","momdad=1","50% survival"),lty=c(2,1,3),lwd=c(1,1,3))
clip(min(0),max(48),min(-1),max(0.5))
abline(h=0.5,lty=3,lwd=3)
abline(v=48,lty=3,lwd=3)


summary(model1)


model2 <- survfit(firstsex1~momdad, data=first)
summary(model2)
plot(model2,lty=c(2,1),xlab="months since 12th birthday",ylab="'survival'")
legend(locator(1),c("momdad=0","momdad=1","50% survival"),lty=c(2,1,3),lwd=c(1,1,3))
clip(min(0),max(48),min(-1),max(0.5))
abline(h=0.5,lty=3,lwd=3)
abline(v=48,lty=3,lwd=3)


search()
detach(data6)
attach(first)
model3=survfit(firstsex1~momdad, data=first)
survdiff(firstsex1~momdad,data=1,rho=1)

median(first$firstsex[momdad==0]);median(first$firstsex[momdad==1])


model3 <- survreg(firstsex1~famsize+pareduc+lowincom+relschol+momdad+white,data1,dist="lognormal")
model4 <- survreg(firstsex1~famsize+pareduc+lowincom+relschol+momdad+white,data1,dist="exponential")
model5 <- survreg(firstsex1~famsize+pareduc+lowincom+relschol+momdad+white,data1,dist="weibull")
model6 <- coxph(firstsex1~famsize+pareduc+lowincom+relschol+momdad+white,data1)

AIC=AIC(model3,model4,model5,model6)
AIC$dAIC = AIC$AIC - min(AIC$AIC) 
AIC=AIC[order(AIC$dAIC),]
AIC
summary(model3)


exp(coef(model3))
exp(coef(model4))
exp(coef(model5))
exp(coef(model6))
AIC(model3,model4,model5,model6)






