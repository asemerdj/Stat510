install.packages("pscl")
require(ggplot2)
require(pscl)

data1 <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
data1 <- within(data1, {
  livebait <- factor(livebait)
  camper <- factor(camper) })
summary(data1)
ggplot(data1, aes(count)) + geom_histogram() + scale_x_log10()
attach(data1)
plot(density(data1$count))


t1 <- table(persons,count)
a=round(prop.table(t1,1)*100, 0)
data1$persons=as.numeric(data1$persons)
t1.2=table(a[1,1],100-a[1,1])
persons.table=data.frame(a[,1],100-a[,1])
names(persons.table)[1:2]=c("0",">1")
persons.table
t2 <- table(child,count)
a=round(prop.table(t2,1)*100, 0)
children.table=data.frame(a[,1],100-a[,1])
names(children.table)[1:2]=c("0",">1")
children.table
t3 <- table(camper,count)
a=round(prop.table(t3,1)*100, 0)
camper.table=data.frame(a[,1],100-a[,1])
names(camper.table)[1:2]=c("0",">1")
camper.table
t4 <- table(livebait,count)
a=round(prop.table(t4,1)*100, 0)
livebait.table=data.frame(a[,1],100-a[,1])
names(livebait.table)[1:2]=c("0",">1")
livebait.table

par(mfrow = c(2,2),
    oma = c(3,3,0,0) + 0.1,
    mar = c(4,4,2,2) + 0.1)
plot(data1$persons,data1$count,xlab="# people",ylab="# fish")
plot(data1$child,data1$count,xlab="# children",ylab="# fish")
plot(data1$camper,data1$count,xlab="campers?",ylab="# fish")
plot(data1$livebait,data1$count,xlab="livebait?",ylab="# fish")


####
model1=zeroinfl(count~child+camper+livebait|persons,data=data1)
summary(model1)


model0=update(model1,.~1)
summary(model0)
pchisq(2*(logLik(model1)-logLik(model0)),df=4,lower.tail = F)

###
model2 = glm(count~child+camper+livebait,family="poisson",data=data1)
summary(model2)
vuong(model1,model2)

###
exp(coef(model1))
exp(confint(model1))
round((1-a[6,1])*100,1)
round((1-a[6,2])*100,1)
a[6]


model99 <- zeroinfl(count~child+camper | persons, data=data1)
#"livebait" was dropped to make a plot easy to read
newdata1 <- expand.grid(0:3, factor(0:1), 1:4, factor(0:1))
colnames(newdata1) <- c("child", "camper", "persons")
newdata1 <- subset(newdata1, subset=(child <= persons))
newdata1$phat <- predict(model99, newdata1)
ggplot(newdata1, aes(x = child, y = phat, colour = factor(persons))) +
  geom_point() + geom_line() + facet_wrap(~camper) +
  labs(x = "Number of Children", y = "Predicted Fish Caught")
dev.off()#run if you get invalid graphics state error
