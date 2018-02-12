setwd("~/Desktop/School/Stat510/hw/hw 4")

slm=function(b0,b1,x){
  fu=1/((1+exp(-(b0+(b1*x)))))
  return(fu)
}

x=seq(-10,10,0.02)
b0=c(0,0,0)
b1=c(0.3,0.6,1)

nam1=paste0("b0.",b0[1]," b1.",b1[1])
nam2=paste0("b0.",b0[2]," b1.",b1[2])
nam3=paste0("b0.",b0[3]," b1.",b1[3])

b=slm(b0[1],b1[1],x)
c=slm(b0[2],b1[2],x)
d=slm(b0[3],b1[3],x)

plot(x,b,ylab="",type="l",lty=1,xlim=c(-15,10))
lines(x,c,ylab="",type="l",lty=2,col=2)
lines(x,d,ylab="",type = "l",lty=3,col=4)
legend(locator(1),lty=c(1,2,3),col=c(1,2,4),c(nam1,nam2,nam3),cex=.75)



##2 
drugtest=read.csv("./drugtest.csv")
attach(drugtest)
table1=table(married,mjuser)
table1

margin.table(table1,1)
margin.table(table1,2)
a=prop.table(table1,1)

mar= a[2,2]/(1-a[2,2])
unmar= a[1,2]/(1-a[1,2])
mar
unmar
unmar/mar
mar/unmar
.1/.3

m.odds=table1[1,2]
table1[1,2]
?prop.table

m=lm(mjuser~married)
plot(m)


##c
a=glm(mjuser~married,data=drugtest,family="binomial")
summary(a)
predict(a,list(married=1),type="response")

exp(coefficients(a))
confint(a)
exp(confint(a))
predict(a,list(male=1),type="response")
predict(a,list(male=0),type="response")
logLik(a)
ll=as.numeric(ll)
-2*-4098.87
1-pchisq(8511.1-8197.7,1)
?pchisq
AIC(a)
