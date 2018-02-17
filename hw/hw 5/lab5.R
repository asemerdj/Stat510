#setwd("~/Desktop/School/Stat510/hw")
setwd("U:/Stat510/Stat510/hw")

depress=read.csv("./hw 4/depress.csv")
depress=na.omit(depress)
d1=glm(satlife~male,family="binomial",data=depress)
d2=glm(satlife~male+age+iq,family="binomial",data=depress)
summary(d1)
summary(d2)

exp(coef(d1))
exp(coef(d2))
