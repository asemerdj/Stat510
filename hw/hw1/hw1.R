install.packages("faraway")
library(faraway)
attach(pima)


dim(pima)
head(pima)
plot(pima[,1:8])
summary(pima)
table(pima$test)

pima[,2:6][pima[,2:6]==0]=NA
head(pima)

par(mfrow=c(2,4))
for(i in 1:8){
plot(density(pima[,i],na.rm = T),main=colnames(pima[i]),xlab="")
}

par(mfrow=c(1,3))
plot(density(pima$glucose,na.rm = T),main="glucose",xlab="shapiro p = 1.72e-11, w = 0.970")
plot(density(log(pima$glucose),na.rm = T),main="log(y)",xlab="shapiro p = 0.0002, w = 0.991")
plot(density(sqrt(pima$glucose),na.rm = T),main=expression(sqrt(y)),xlab="shapiro p = 1.242e-6, w = 0.986")
shapiro.test(pima$glucose)
shapiro.test(log(pima$glucose))
shapiro.test(sqrt(pima$glucose))

data2=na.omit(pima)
dim(data2)
summary(data2)
head(data2)

model1=lm(glucose~pregnant+diastolic+triceps+insulin+bmi+diabetes+age,data=pima)
summary(model1)
anova(model1)

mean(pima$glucose,na.rm = T)
mean(data2$glucose)

head(data2)
model1$fitted[1:5]
model1$residuals[1:5]




####lab1####
data1=read.csv("http://www.stat.cmu.edu/~cshalizi/mreg/15/lectures/03/bea-2006.csv")
dim(data1)
head(data1)
search()
detach(pima)
attach(data1)
plot(data1[,2:7])
summary(data1)
