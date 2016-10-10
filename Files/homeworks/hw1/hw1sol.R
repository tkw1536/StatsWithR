### R code from vignette source '~/Teaching/StatModelswR/Fall2016/Homework/HW1/hw1sol.Rnw'

###################################################
### code chunk number 1: hw1sol.Rnw:31-36
###################################################
library(knitr)
opts_chunk$set(
concordance=TRUE
)
options(scipen=999)


###################################################
### code chunk number 2: hw1sol.Rnw:78-79
###################################################
load("~/Data/OregonHomes.Rdata")


###################################################
### code chunk number 3: price.school
###################################################
options(width=70)
par(mfrow=c(1,1))
boxplot(Price~School,data=homes,main="Sales price of homes", 
ylab="Sales price of homes in USD 1000's", xlab="School District",varwidth=TRUE)


###################################################
### code chunk number 4: hw1sol.Rnw:117-121
###################################################
price.school <- aov(Price~as.factor(School), data=homes)
summary(price.school)
TukeyHSD(price.school)
plot(TukeyHSD(price.school))


###################################################
### code chunk number 5: hw1sol.Rnw:149-151
###################################################
TukeyHSD(price.school)
plot(TukeyHSD(price.school))


###################################################
### code chunk number 6: hw1sol.Rnw:172-174
###################################################
price.school.lm <- lm(Price~School, data=homes)
summary(price.school.lm)


###################################################
### code chunk number 7: hw1sol.Rnw:194-195
###################################################
anova(price.school.lm)


###################################################
### code chunk number 8: hw1sol.Rnw:214-217
###################################################
price.sfa.lm <- lm(Price~School+ Floor + Age, data=homes)
summary(price.sfa.lm)
anova(price.sfa.lm)


###################################################
### code chunk number 9: hw1sol.Rnw:235-238
###################################################
price.sfa.int <- lm(Price~School*Age +Floor, data=homes)
summary(price.sfa.int)
anova(price.sfa.int)


###################################################
### code chunk number 10: hw1sol.Rnw:257-260
###################################################
slopes <- price.sfa.int$coef[7]+c(0,price.sfa.int$coef[9:13])
names(slopes)<-levels(homes$School)
slopes


###################################################
### code chunk number 11: hw1sol.Rnw:266-269
###################################################
price.sfa.int.sq <- lm(Price~School*Age +Floor + I(Age^2), data=homes)
summary(price.sfa.int.sq)
anova(price.sfa.int.sq)


###################################################
### code chunk number 12: hw1sol.Rnw:290-294
###################################################
library(ggplot2)
p <- qplot(homes$Age,homes$Price,xlab="Age (standardized)", ylab="Price", main="House Prices inOregon")
p + geom_abline(aes(intercept=price.sfa.int$coeff[1], slope = price.sfa.int$coeff[7], col="red")) +geom_abline(aes(intercept=price.sfa.int$coeff[1] + price.sfa.int$coeff[2] , slope = price.sfa.int$coeff[7] + price.sfa.int$coeff[9], col="blue")) + geom_abline(aes(intercept=price.sfa.int$coeff[1] + price.sfa.int$coeff[3] , slope = price.sfa.int$coeff[7] + price.sfa.int$coeff[10], col="cyan")) + geom_abline(aes(intercept=price.sfa.int$coeff[1] + price.sfa.int$coeff[4] , slope = price.sfa.int$coeff[7] + price.sfa.int$coeff[11], col="brown")) + geom_abline(aes(intercept=price.sfa.int$coeff[1] + price.sfa.int$coeff[5] , slope = price.sfa.int$coeff[7] + price.sfa.int$coeff[12], col="grey")) + geom_abline(aes(intercept=price.sfa.int$coeff[1] + price.sfa.int$coeff[6] , slope = price.sfa.int$coeff[7] + price.sfa.int$coeff[13], col="black"))



