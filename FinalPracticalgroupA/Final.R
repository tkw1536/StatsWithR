# load all the libraries
library(car)
library(datasets)
library(effects)
library(epitools)
library(foreign)
library(ggplot2)
library(ISLR)
library(MASS)
library(mlbench)
library(nnet)
library(Rcmdr)
library(RcdmrMisc)
library(sandwich)
library(reshape2)
library(xtable)

#### final datasets
load("/home/twiesing/Projects/Jacobs/StatsWithR/Files/final/data/al4.Rdata")

### exercise 1
ex1 <- lm(cmedv ~ chas, data=BostonHousing4)
t.test(cmedv ~ chas, data = BostonHousing4, test="f")
anova(ex1)

### exercise 2
ex2 <- lm(cmedv ~ lstat + nox + rm + age, data=BostonHousing4Copy)
crPlots(ex2)

### exercise 3
ex3 <- lm(cmedv ~ lstat + nox + rm + age + I(rm^2) + I(lstat^2), data=BostonHousing4)

### exercise 4
BostonHousing4Copy <- BostonHousing4
BostonHousing4Copy$rm <- scale(BostonHousing4Copy$rm, center = TRUE, scale = FALSE)
BostonHousing4Copy$lstat <- scale(BostonHousing4Copy$lstat, center = TRUE, scale = FALSE)

ex4 <- glm(cmedv ~ lstat + nox + rm + age + I(rm^2) + I(lstat^2), family=binomial(logit), data=BostonHousing4Copy)

### exercise 4 (really)
ex5 <- glm(formula = chas ~ cmedv + dis + lstat + crim + b + indus, data=BostonHousing4, family=binomial(logit))

summary(ex5, cor=FALSE, Wald=TRUE)
nullmodel <- glm(formula = chas ~ 1, data = BostonHousing4, family=binomial(logit))
exf <- stepAIC(nullmodel, scope = list(upper = ex5,
                                            lower = ~1), direction = "both")
anova(nullmodel, exf, test = "Chisq")
