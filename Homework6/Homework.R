library(foreign)
library(Hmisc)

## Read in the SPSS dataset
#
timss <- read.spss("./timss.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(timss) <- tolower(colnames(timss))

# the eleven columns of interest
eleven_cols <- c("bsbgday1", "bsbgday2", "bsbgday3", "bsbgday4", "bsbgday5", "bsbgday6", "bsbmday7", "bsbsday8", "bsdgday9", "bsbgclub", "bsbgpaid")
new_eleven_cols <- sapply(eleven_cols, function(c)(paste(c, ".i", sep="")))

# 1a) We count the missing names for each of the columns
sapply(eleven_cols, function(c)(sum(is.na(timss[,which(colnames(timss)==c)]))))

## 1b) Correltation matrix
ex1b <- cor(timss[,eleven_cols], use="complete")

## 1c) Replace the missing value
set.seed(13052014)
timss$bsbgday1.i<-impute(timss$bsbgday1,"random") 
timss$bsbgday2.i<-impute(timss$bsbgday2,"random")
timss$bsbgday3.i<-impute(timss$bsbgday3,"random")
timss$bsbgday4.i<-impute(timss$bsbgday4,"random")
timss$bsbgday5.i<-impute(timss$bsbgday5,"random")
timss$bsbgday6.i<-impute(timss$bsbgday6,"random")
timss$bsbmday7.i<-impute(timss$bsbmday7,"random")
timss$bsbsday8.i<-impute(timss$bsbsday8,"random")
timss$bsdgday9.i<-impute(timss$bsdgday9,"random")
timss$bsbgclub.i<-impute(timss$bsbgclub,"random")
timss$bsbgpaid.i<-impute(timss$bsbgpaid,"random")

## 1d) perform the PCA
ex1d <- princomp(~bsbgday1.i + bsbgday2.i + bsbgday3.i + bsbgday4.i + bsbgday5.i + bsbgday6.i + bsbmday7.i + bsbsday8.i + bsdgday9.i + bsbgclub.i + bsbgpaid.i, cor=TRUE, data=timss) 
summary(ex1d)

# according to Kaiser, we take the first four prinipal components with EV > 1
ex1d$sd^2
# 0.53214841 (53%) of the original variablitiy is explained

## 1e) Factor Analysis
ex1e <- factanal(~~~bsbgday1.i + bsbgday2.i + bsbgday3.i + bsbgday4.i + bsbgday5.i + bsbgday6.i + bsbmday7.i + bsbsday8.i + bsdgday9.i + bsbgclub.i + bsbgpaid.i, factors=4, rotation="varimax", scores="Bartlett", data=timss) 
summary(ex1e)
# Save and label the scores
timss$FVe1 <- ex1e$scores[,1] 
timss$FVe2 <- ex1e$scores[,2] 
timss$FVe3 <- ex1e$scores[,3] 
timss$FVe4 <- ex1e$scores[,4] 

## 1f) another factor analysis
ex1f <- factanal(~~~bsbgday1.i + bsbgday2.i + bsbgday3.i + bsbgday4.i + bsbgday5.i + bsbgday6.i + bsbmday7.i + bsbsday8.i + bsdgday9.i + bsbgclub.i + bsbgpaid.i, factors=4, rotation="promax", scores="Bartlett", data=timss) 
summary(ex1f)
# Save and label the scores
timss$FVf1 <- ex1f$scores[,1] 
timss$FVf2 <- ex1f$scores[,2] 
timss$FVf3 <- ex1f$scores[,3] 
timss$FVf4 <- ex1f$scores[,4] 

## no significant difference (both have 121 factors)


## Exercise 2
## linear regression
ex2 <- lm(bimatscr ~ FVe1 + FVe2 + FVe3 + FVe4 + bsbghome + bsbgedum + bsbgeduf + bsbgedus + bsbgsex + bsbgbrn1 + bsbglang, data = timss)


## Exercise 3
ex3 <- lm(bimatscr ~ FVf1 + FVf2 + FVf3 + FVf4 + bsbghome + bsbgedum + bsbgeduf + bsbgedus + bsbgsex + bsbgbrn1 + bsbglang, data = timss)


### Both models appear very similar with a bad R^2 of only 0.14