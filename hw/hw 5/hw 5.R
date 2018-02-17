#setwd("~/Desktop/School/Stat510/hw")
setwd("U:/Stat510/Stat510/hw")


drugtest=read.csv("./hw 4/drugtest.csv")
attach(drugtest)

model1=glm(mjuser~married,family="binomial",data=drugtest)
model2=glm(mjuser~married+age+educate,family="binomial",data=drugtest)
model3=glm(mjuser~educate,family="binomial",data=drugtest)
summary(model1)
summary(model2)

exp(coef(model1))
exp(coef(model2))
exp(coef(model3))

anova(model1,model2,test="Chisq")

tapply(drugtest$age,drugtest$married,mean)
tapply(drugtest$educate,drugtest$married,mean)

predict(model2, list(married=1,age=34.82577,educate=12.69398),type="response")
predict(model2, list(married=0,age=29.31639,educate=12.80724),type="response")


##
data1=read.csv("./sampleData/gss.csv")
data2=data1[complete.cases(data1$childs),]
data3=data2[complete.cases(data2$female),]
data4=data3[complete.cases(data3$nonwhite),]
data5=data4[complete.cases(data4$educate),]
data6=data5[complete.cases(data5$income),]

data2=data1[complete.cases(data1[c(4,15,16,18,6)]),]
dim(data2)
data99=na.omit(data1)

mean(childs); var(childs)

hist(data2$childs,freq = F,breaks=0:10,main="histogram of childs",xlab="childs")
x=0:10
prob=dpois(x,1.5973)
lines(x,prob,col=2,lty=2,lwd=2)
table1=table(data2$childs)
round(prop.table(table1),5)
round(dpois(x,1.5973),5)


model1=lm(childs~female)
summary(model1)
plot(model1)

model2=glm(childs~female,data=data2,family="poisson")
model3=glm(childs~female+nonwhite+educate+income,data=data2,family="poisson")
summary(model3)
anova(model2,model3,test="Chisq")

exp(coef(model3))

1-pchisq(3216.9-3102.0,4)
1- 0.9381607
