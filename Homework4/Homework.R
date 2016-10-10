# Homework 4
## Apparently wwe need all these libraries -- why?
library(car) 
library(MASS) 
library(xtable) 
library(epitools) 
library(nnet) 
library(foreign)
library(ggplot2) 
library(reshape2)


# read all the data from the internets
student <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")

### Exercise 1
ex1 <- xtabs(~ ses + prog, data=student)
ex1
## 1a) the academic program was choosen by the largest fraction with high ses
## 1b) 34.04255% of low income students selected the general program 
ex1 / rowSums(ex1, na.rm = T) 
## 1c) In the academic program, there are more students with middle social economic status. 
## 1d) The least frequent combination of variables is vocation / high ses

### Exercise 2
## 2a)
mosaicplot(ex1)
## 2b) TODO: Actually compute the odds ratio

# Make a copy of the data and add the aggregated attribute. 
studentcopy <- student
studentcopy$academicProgram <- NA
studentcopy[studentcopy$prog == "academic",]$academicProgram <- "yes"
studentcopy[studentcopy$prog != "academic",]$academicProgram <- "no"

# compute the odds ratio
ex2b <- xtabs(~ ses + academicProgram, data=studentcopy)
oddsratio(ex2b)

# low / middle = 19 / 44 = 0.43
19 / 44
# low / high = 19 /42 = 0.45
19 / 42

## Exercise 3
ex3 <- chisq.test(ex1)
## 3a) As p <0.05, the chi^2 test statistic is significant. 
## 3b) The expected frequencies are higher in low/academic, middle/general, middle/academic, high/general, high/vocation
ex3$expected > ex1

## Exercise 4
# split data into male and female
stud_female <- student[student$female == "female", ]
stud_male <- student[student$female == "male", ]

## 4a)
ex4a_f <- xtabs(~ ses + prog, data=stud_female)
ex4a_m <- xtabs(~ ses + prog, data=stud_male)

sq4a_f <- chisq.test(ex4a_f)
sq4a_f # females : not significant
sq4a_m <- chisq.test(ex4a_m)
sq4a_m # males: significant

## 4b) 
sq4a_f$expected > ex4a_f # females: low/academic, middle/general, middle/academic, high/general, high/vocation
sq4a_m$expected > ex4a_m # males: low/academic, middle/general, middle/academic, high/general, high/vocation

## 4c) 4a: yes, 4b: no
## 4d) the low/general ratio is different 
mosaicplot(ex4a_f)
mosaicplot(ex4a_m)

## Exercise 5
ex5 <- multinom(prog ~ female + ses + schtyp + read + write + math + science + honors + awards, data=student, trace=FALSE)
## 5a) the AIC is 358.8871
AIC(ex5)
## 5b) Computing the p values and test statistics
summary(ex5, cor=FALSE, Wald=TRUE)
z <- summary(ex5, cor=FALSE, Wald=TRUE)$Wald.ratios 
p <- (1 - pnorm(abs(z), 0, 1))*2
p
p < 0.05
# signigicant: academic / intercept, academic/math, academic/science, vocation/sesmiddle

## Exercise 6
ex6 <- stepAIC(ex5, direction="backward")
## 6a) ses + schtyp + read + math + science
## 6b) The BIC is 397.1684
BIC(ex6)
## 6c) -161.496
logLik(ex6)

## Exercise 7
## TODO: Do the averaging properly
## we need to predict for high/private, middle/private, low/private, high/public, middle/public, low/public
ex7data <- expand.grid(ses = c("low", "middle", "high"), schtyp=c("public","private"), read=mean(student$read), math=mean(student$math), science = mean(student$science))
ex7data
predict(ex6, ex7data, "probs")

## Exercise 8
ex8data <- expand.grid(ses = c("low", "middle", "high"), schtyp=c("public","private"), read=mean(student$read), math=mean(student$math), science = seq(30, 80, 1))
ex8data
ex8preds <- predict(ex6, ex8data, "probs")
sapply(levels(student$ses), function(s){apply(data.frame(ex8preds[ex8data$ses == s, ]), 2, mean)})
