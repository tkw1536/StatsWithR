### Load the dataset
## Because R, this creates state and we now have a variable homes
## There is no way to restrict this -- it all ends up in the main namespace
load("./OregonHomes.Rdata")
summary(homes)


### Exercise 1 -- Make a boxplot of Price against school
boxplot(Price~School, data=homes)

### (1a) 1/2 crest >= 3/4 Adams
## Yes

### (1b) smallest IQR -- make a loop and print them all
## juding form the plot, Parker is the smallest spread
## we can also verify this: 
sapply(levels(homes$School), function(s){IQR(homes[homes$School == s,]$Price, na.rm = TRUE)})


### (1c) homescedasity -- compute all the variances
## no homescedasity does not hold, clearly the ranges are different
## we can also verify this by using the following code
sapply(levels(homes$School), function(s){var(homes[homes$School == s,]$Price, na.rm = TRUE)})


### Exercise 2 -- Run an ANOVA model
anovamodel <- aov(Price~School, data=homes)
summary(anovamodel)

### (2a) do the districts have an impact?
## as P<0.05 and F>1, they do have an impact

## (2b) P-value == Pr(>F) = 0.003040657

### (2c) F distribution
## degrees of freedom 5 and 70

### (2d) percentage of sum of squares explained
## 0.2218734
60573 / (60573 + 212434)

### Exercise 3 -- Tukey HSD post hoc test
plot(TukeyHSD(anovamodel))
## Parker-Edison, Redwood-Edison, Parker-Harris

### Exercise 4 -- Adsted P values for signifcant differences with 4 digits
## 0.0202, 0.0436, 0.0404

### Exercise 5
linearmodel <- lm(Price~School, data=homes)
summary(linearmodel)

### (5a) wich schools differ at 5% level -- not sure
## SchoolEdison, SchoolHarris

### (5b) Percentage of standard error explained == 0.7920269
(31.81 + 38.95 + 35.56 + 35.05 + 34.84 + 33.59) / (31.81 + 38.95 + 35.56 + 35.05 + 34.84 + 33.59 + 55.09)
## this model explains 4x as much percentage wise ==> it is better

### Exercise 6
anova(linearmodel)

### (6a) signifcant impact?
## Yes, since p < 0.05 and F > 1

### (6b) where is the value in 5?
## At the bottom, null hypothesis is tested

### Exercise 7
multiplelinearmodel <- lm(Price~School+Floor+Age, data=homes)

### (7a) according to anvova which have a significant influence?
anova(multiplelinearmodel)
## School, Floor, Age

### (7b) Is the model better than the model above?
## 83% > 79% explained ==> better
(60.216 + 37.985 + 34.891 + 34.643 +  34.123 + 32.692 + 29.826 + 3.102) / (60.216 + 37.985 + 34.891 + 34.643 +  34.123 + 32.692 + 29.826 + 3.102 + 52.31)

### Exercise 8
intmodel <- lm(Price ~ School + Floor + Age + Age:School, data=homes)

### (8a) according to anvova which have a significant influence?
anova(intmodel)
## School, Floor, Age, School:Age

### (8b) Which slope is estmiated for age?
# -18.70
# none, seperate values for each interaction term

### Exercise 9
## which districzts is the impact of house prices negative, which positive?
## positive for all of them

### Exercise 10 
sqmodel <-  lm(Price~School+Floor+Age+Age:School+I(Age^2), data=homes)
### (10a) Is the square term significant?
anova(sqmodel)
## not significant at the 5% level

## 10b) Q10 is a model of better quality because of a higher ajusted R^2. 

