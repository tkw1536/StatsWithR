### R code from vignette source '~/Teaching/StatModelswR/Fall2016/Homework/HW2/hw2sol.Rnw'

###################################################
### code chunk number 1: hw2sol.Rnw:31-36
###################################################
library(knitr)
opts_chunk$set(
concordance=TRUE
)
options(scipen=999)


###################################################
### code chunk number 2: hw2sol.Rnw:78-81
###################################################
library(car)
library(MASS)
load("~/Data/OregonHomes.Rdata")


###################################################
### code chunk number 3: price.gar
###################################################
options(width=70)
par(mfrow=c(1,1))
homes$GarGroup <- recode(homes$Gar, "c(0,21)='Small'; else='Big'", as.factor.result=TRUE)
#homes$GarGroup <- with(homes, bin.var(Gar, bins=2, method='intervals', labels=c('Small','Big')))
boxplot(Price~GarGroup,data=homes,main="Sales price of homes", 
ylab="Sales price of homes in USD 1000's", xlab="Garage size",varwidth=TRUE)


###################################################
### code chunk number 4: hw2sol.Rnw:110-112
###################################################
housing.t<-t.test(Price~GarGroup, alternative='two.sided', conf.level=.95, 
var.equal=TRUE,  data=homes)


###################################################
### code chunk number 5: hw2sol.Rnw:120-124
###################################################
price.var<-tapply(homes$Price, homes$GarGroup, var, na.rm=TRUE)
lev.test<- leveneTest(homes$Price, homes$GarGroup, center=mean)
vartest<-var.test(Price ~ GarGroup, alternative='two.sided', conf.level=.95, 
  data=homes)


###################################################
### code chunk number 6: hw2sol.Rnw:141-143
###################################################
housing.aov <- aov(Price ~ GarGroup, data=homes)
summary(housing.aov)


###################################################
### code chunk number 7: hw2sol.Rnw:152-154
###################################################
housing.lm<-lm(Price~GarGroup, data=homes)
summary(housing.lm)


###################################################
### code chunk number 8: hw2sol.Rnw:174-178
###################################################
price.gar2 <- aov(Price~as.factor(Gar), data=homes)
summary(price.gar2)
TukeyHSD(price.gar2)
plot(TukeyHSD(price.gar2))


###################################################
### code chunk number 9: hw2sol.Rnw:190-192
###################################################
TukeyHSD(price.school)
plot(TukeyHSD(price.school))


###################################################
### code chunk number 10: hw2sol.Rnw:204-209
###################################################
numSummary(homes[,"Price"], groups=as.factor(homes$Gar), statistics=c("mean", "sd"))
homes.means <- tapply(homes$Price, homes$Gar, mean, na.rm=TRUE)
homes.sd <- tapply(homes$Price, homes$Gar, sd, na.rm=TRUE)
homes.n <- tapply(homes$Price, homes$Gar, length)
cbind(homes.means, homes.sd, homes.n)


###################################################
### code chunk number 11: hw2sol.Rnw:220-223
###################################################
housing.all <- lm(Price ~ . - GarGroup, data=homes)
summary(housing.all)
Anova(housing.all, type="II")


###################################################
### code chunk number 12: hw2sol.Rnw:266-270
###################################################
housing.base <- lm(Price ~ . - GarGroup - Year, data=homes)
#housing.best <- stepwise(housing.base, direction='backward/forward', criterion='AIC')
housing.best <- stepAIC(housing.base, scope=list(upper=housing.base,lower=~1),direction='both')
summary(housing.best)


###################################################
### code chunk number 13: housing.best
###################################################
par(mfrow=c(4,3))
crPlots(housing.best)


