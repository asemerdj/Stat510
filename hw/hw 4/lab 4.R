depress=read.csv("./depress.csv")

b=glm(satlife~male,data=depress,family="binomial")
summary(b)
predict(b,list(male=0),type="response")


exp(coefficients(b))
confint(b)
exp(confint(b))
predict(b,list(male=1),type="response")
predict(b,list(male=0),type="response")
logLik(b)
-2*-77.81026
AIC(b)
1-pchisq(160.75-155.62,1)
