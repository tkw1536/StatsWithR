### Homework 3
library(car)
library(MASS)

# load the data
data(Wells, package="effects")
summary(Wells)


### Exercise 1
ex1 <- glm(formula = switch ~ 1, data=Wells, family=binomial(logit))
## 1a) AIC == 4120.099
AIC(ex1)
## 1b) BIC == 4126.112
BIC(ex1)
## 1c) the log odds are 0.3029584, which is the same as the intercept in this case
log(mean(as.numeric(Wells$switch)-1)/(2-mean(as.numeric(Wells$switch))))

### Exercise 2
ex2 <- glm(formula = switch ~ 1 + distance, data=Wells, family=binomial(logit))
## 2a) AIC == 4080.238, decrease by 39.86139
AIC(ex2)
AIC(ex1) - AIC(ex2)
## 2b) Yes, it is significant at the 0.001 level. 
summary(ex2)
## 2c) We make an appropriate plot. The likelihood actually decreases with distance. 
ex2ds <- seq(0, 350, .01)
ex2pp <- predict(ex2, list(distance=ex2ds), type='response')

plot(fitted(ex2) ~ Wells$distance)
lines(ex2ds, ex2pp, col='blue')

## 2d) We predict the probability for distance 0. This is also given by the intercept. Here it is 0.6059594. 
predict(ex2, list(distance=0))

### Exercise 3
## 3a) From the table, we can see the coefficients and solve for 0.6059594 + -0.0062188*d = 0.5. 
## This gives us that the halfway point is where distance = 17.03856
hw_p <- (0.5 - 0.6059594) / (-0.0062188)
## 3b) The coefficient is beta /4 where beta is given from the summary above -0.0015547
-0.0062188 / 4

### Exercise 4
ex4 <- glm(switch ~ arsenic + distance + education + as.numeric(association), data=Wells, family=binomial(logit))
## 4a) arsenic, distance, education are significant. 
## The arsenic coefficient is positive, the higher the arscenic the higher the probability to switch. Makes sense. 
## The distance coefficient is negative, the further away, the less likely to switch. Makes sense as the less noticable the effect is. 
## The education coefficient is positive, the higher educated the higher likely to switch. Makes sense. 
summary(ex4)
## 4b) AIC score is 3917.826, arsenic, distance, education are significant
ex4b <- stepAIC(ex4, scope=list(upper=ex4,lower=~1), direction="both")
AIC(ex4b)
summary(ex4b)
## 4c) Looks like arscenic plots should have a square term included
crPlots(ex4b)

### Exercise 5
ex5a <- glm(switch ~ 1 + arsenic + distance + education + as.numeric(association) + I(arsenic^2), data = Wells, family=binomial(logit))
ex5b <- glm(switch ~ 1 + arsenic + distance + education + as.numeric(association) + log(arsenic), data = Wells, family=binomial(logit))

## Compare their BIC
BIC(ex5a)
BIC(ex5b)
## BICs ==> ex5a better

## Chi^2 tets between the model
anova(ex5a, ex5b, test="Chisq")
# The deviance is too big (> 10), so the first model is better

### Exercise 6
## Ex 6a)
predict(ex5a, list(arsenic=mean(Wells$arsenic), distance=mean(Wells$distance), education=mean(Wells$education), association=mean(as.numeric(Wells$association))))

## Ex 6b)
predict(ex5a, list(arsenic=mean(Wells$arsenic), distance=mean(Wells$distance) + 100, education=mean(Wells$education), association=mean(as.numeric(Wells$association))))
predict(ex5a, list(arsenic=mean(Wells$arsenic), distance=mean(Wells$distance), education=mean(Wells$education) + sd(Wells$education), association=mean(as.numeric(Wells$association))))

## Exercise 7
ex7 <- glm(switch ~ distance + arsenic + distance : arsenic, data=Wells, family=binomial(logit))
## distance = -0.005772 => the futher away, the less the impact
## arscenic = 0.555977 => the more arscenic the more likely to switch
## interaction terms => the bigger the distance, the fewer the impact of arscenic on the probahility
## the interpreations make sense

## Exercise 8
WellsCenter <- Wells
WellsCenter$distance = scale(WellsCenter$distance, center = TRUE, scale = FALSE)
WellsCenter$arsenic = scale(WellsCenter$arsenic, center = TRUE, scale = FALSE)

ex8 <- glm(switch ~ distance + arsenic + distance:arsenic, data=WellsCenter, family=binomial(logit))
## distance = -0.008737 => the futher away, the less the impact
## arscenic = 0.469508 => the more arscenic the more likely to switch
## interaction terms => the bigger the distance, the fewer the impact of arscenic on the probahility
## TODO: effects plot
