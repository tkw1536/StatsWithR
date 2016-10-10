### R code from vignette source '~/Teaching/StatModelswR/Fall2016/Homework/HW3/hw3sol.Rnw'

###################################################
### code chunk number 1: hw3sol.Rnw:31-36
###################################################
library(knitr)
opts_chunk$set(
concordance=TRUE
)
options(scipen=999)


###################################################
### code chunk number 2: hw3sol.Rnw:74-77
###################################################
library(car)
library(MASS)
data(Wells, package="effects")


###################################################
### code chunk number 3: hw3sol.Rnw:85-89
###################################################
wells.1 <- glm(switch ~ 1 , 
  family=binomial(logit), data=Wells)
summary(wells.1)
AIC(wells.1)


###################################################
### code chunk number 4: hw3sol.Rnw:97-98
###################################################
BIC(wells.1)


###################################################
### code chunk number 5: hw3sol.Rnw:106-107
###################################################
logodds <- log(mean(as.numeric(Wells$switch)-1)/(2-mean(as.numeric(Wells$switch))))


###################################################
### code chunk number 6: hw3sol.Rnw:117-120
###################################################
wells.dist <- update(wells.1, .~. + distance)
summary(wells.dist)
AIC(wells.dist)


###################################################
### code chunk number 7: hw3sol.Rnw:131-132
###################################################
Anova(wells.dist)


###################################################
### code chunk number 8: hw3sol.Rnw:178-181
###################################################
wells.main <- glm(switch ~ education + association + distance + arsenic, 
  family=binomial(logit), data=Wells)
summary(wells.main)


###################################################
### code chunk number 9: hw3sol.Rnw:192-195
###################################################
wells.best <- stepAIC(wells.main, scope=list(upper=wells.main,lower=~1),direction='both')
summary(wells.best)
AIC(wells.best)


###################################################
### code chunk number 10: wells.cr
###################################################
crPlots(wells.best)


###################################################
### code chunk number 11: hw3sol.Rnw:211-217
###################################################
wells.best.aq <- update(wells.best, .~. + I(arsenic^2), data=Wells)
Wells$larsen <- with(Wells, log(arsenic))
wells.best.ln <- glm(switch ~ education +  distance + larsen, 
  family=binomial(logit), data=Wells)
anova(wells.best.aq, wells.best.ln,  test="Chisq")
AIC(wells.best.aq,wells.best.ln)


###################################################
### code chunk number 12: hw3sol.Rnw:227-231
###################################################
pred.means <- data.frame(education = mean(Wells$education, na.rm = TRUE), 
distance = mean(Wells$distance, na.rm = TRUE), larsen = mean(Wells$larsen, 
na.rm = TRUE))
p.pred.means <- predict(wells.best.ln, pred.means, type="response")


###################################################
### code chunk number 13: hw3sol.Rnw:241-245
###################################################
pred.d100 <- data.frame(education = mean(Wells$education, na.rm = TRUE), 
distance = 100, larsen = mean(Wells$larsen, 
na.rm = TRUE))
p.pred.d100 <- predict(wells.best.ln, pred.d100, type="response")


###################################################
### code chunk number 14: hw3sol.Rnw:253-257
###################################################
pred.edu1 <- data.frame(education = (mean(Wells$education, na.rm = TRUE) + 
sd(Wells$education, na.rm=TRUE)), distance = mean(Wells$distance, 
na.rm = TRUE), larsen = mean(Wells$larsen, na.rm = TRUE))
p.pred.edu1 <- predict(wells.best.ln, pred.edu1, type="response")


###################################################
### code chunk number 15: hw3sol.Rnw:267-270
###################################################
wells.ad <- glm(switch ~ distance*arsenic, 
  family=binomial(logit), data=Wells)
summary(wells.ad)


###################################################
### code chunk number 16: hw3sol.Rnw:280-285
###################################################
Wells$arsenic.c <- with(Wells, arsenic-mean(arsenic,na.rm=TRUE))
Wells$distance.c <- with(Wells, distance-mean(distance,na.rm=TRUE))
wells.ad.c <- glm(switch ~ distance.c * arsenic.c, 
  family=binomial(logit), data=Wells)
summary(wells.ad.c)


