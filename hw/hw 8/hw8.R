install.packages("lme4")
install.packages("nlme")
library(lme4)
library(nlme)
data("Wheat")
detach(Wheat)
attach(Wheat)
head(Wheat)
Wheat$Tray=as.factor(Wheat$Tray)
Wheat$Moisture=as.factor(Wheat$Moisture)
Wheat$fertilizer=as.factor(Wheat$fertilizer)


interaction.plot(Wheat$Moisture,Wheat$fertilizer,Wheat$DryMatter,xlab="Moisture",ylab="Dry Matter",trace.label = "Fertilizer Type")

a=lmer(DryMatter~Moisture*fertilizer+(1|Tray))                
summary(a)

a=lmer(DryMatter~Moisture*fertilizer+(1|Tray),REML=F)
b=lmer(DryMatter~Moisture+fertilizer+(1|Tray),REML=F)
anova(a,b)


install.packages("lattice")
library(lattice)

xyplot(DryMatter~fertilizer|Moisture,pch=16,col="black")

a=lmer(DryMatter~Moisture+fertilizer+(1|Tray),REML=F)
b=lmer(DryMatter~Moisture+(1|Tray),REML=F)
anova(a,b)
detach(Wheat)



data("Pastes")
head(Pastes)
attach(Pastes)

tapply(strength,list(batch,cask),mean)

install.packages("ggplot2")
library(ggplot2)

ggplot(Pastes,aes(x=batch,y=strength,fill=cask))+geom_boxplot()+xlab("batch")+scale_fill_manual(values=c("gray100","gray88","gray48"))

a=lmer(strength~(1|batch)+(1|cask))
b=lmer(strength~(1|batch)+(1|batch:cask))
summary(a)
summary(b)

interaction.plot(batch,cask,strength)

a=lmer(strength~(1|batch)+(1|batch:cask),REML=F)
b=lmer(strength~(1|batch:cask),REML=F)
anova(a,b)

a=lmer(strength~(1|batch)+(1|batch:cask),REML=F)
b=lmer(strength~(1|batch),REML=F)
anova(a,b)
