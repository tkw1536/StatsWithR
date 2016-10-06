### Load the dataset again
load("./OregonHomes.Rdata")
summary(homes)

## and we need this down the road
library(car)

### Exercise 1
## Create a new factor $GarGroup
homes$GarGroup <- NA # init with NA
homes$GarGroup[homes$Gar <= 1] <- "OneOrNoCar" # one group for OneOrNoCars
homes$GarGroup[homes$Gar >= 2] <- "TwoOrThreeCars" # and another group for the other cases
homes$GarGroup <- as.factor(homes$GarGroup) # make sure it is a factor

## make a boxplot
boxplot(Price~GarGroup, data=homes)

## a) Yes we expect the mean of the prizes to differ significantly as the ranges for "TwoOrThreeCars" us significantly higher. Furthermore
## the IQR is bigger for the second group. 

## b) we make a t-test
t.test(Price~GarGroup, data=homes, var.equal=TRUE) # P = 0.001547
## the p-value is very small, so we can assume there is a significant difference in house prices

## c) We compute all the variances to check if they are equal
sapply(levels(homes$GarGroup), function(g){var(homes[homes$GarGroup == g,]$Price, na.rm = TRUE)})
## this is not actually given, we see OneOrNoCar has Variance 3809, TwoOrThreeCars has 2952.802

### Exercise 2
ex2a <- aov(Price~GarGroup, data=homes)
summary(ex2a)

## 2a) There is a significant difference, as F > 1 and p = 0.00155 < 0.01
## 2b) p =0.00155 < 0.01 => it is significant
ex2b <- lm(Price~GarGroup, data=homes)
summary(ex2b)

## 2c) The p-value for linear model and anova are the same. It differed slightly for the t test. 
## all three compute a t statistic, so the value is obviously the same.

### Exercise 3
ex3 <- aov(Price ~ as.factor(Gar), data=homes)
## 3a) yes it does. p = 0.015
## 3b) only 2-0
TukeyHSD(ex3)
## 3c) There is an outlier for housses with no garage (see boxplot). 
boxplot(Price ~ as.factor(Gar), data=homes)

### Exercise 4
ex4 <- lm(Price ~ Floor + Lot + Bath + Bed + Year + Age + Gar + Status + School, data=homes)
## Ex 4.a) Floor, Lot, Bed, Gar, School are significant
anova(ex4)
## Ex 4.b) the AIC gives us the model quality which is 808.8182
AIC(ex4)
## Ex 4.c) As a numerical variable. We see this from the Df (degrees of freedom). 
aov(lm(Price ~ Floor + Lot + Bath + Bed + Year + Age + as.factor(Gar) + Status + School, data=homes))

### Exercise 5
## The variable age is linearly dependent on year (we obviously have age + year == constant). Hence during the linar model analysis, 
## the computation encounters a singularity and thus needs to pick one of the variables to ignore. 

### Exercise 6
## We go over the significant variables. 
## Floor: coefficient +72, i.e. better floor => higher costs (makes sense)
## Lot: coefficient +10, i.e. better lot => higher costs (makes sense)
## Bed: coefficient -12, i.e. more bedrooms => lower price (DOES NOT make sense)
## Gar: coefficient +7, i.e. more garage space => higher price (makes sense)
## School: different coefficients per district, only one negative. This makes sense if the Parker district is one of the worse districts. 

### Exercise 7
ex7 <- lm(Price ~ Floor + Lot + Bath + Bed + Age + Gar + Status + School, data=homes)
ex7f <- step(ex7, direction="both")
## The final model uses Floor, Lot, Bed, Status, School. 

## Exercise 8
crPlots(ex7f)
## 8a) Some square terms should be included for the Lot and Floor components
## 8b) Neither of shows a better smoothing
crPlots(ex7f, smoother.args=list(span=0.25))
crPlots(ex7f, smoother.args=list(span=0.75))
## 8c) 
ex8c <- lm(formula = Price ~ Floor + Lot + Bed + Status + School + I(Lot^2), data = homes)
AIC(ex8c)
