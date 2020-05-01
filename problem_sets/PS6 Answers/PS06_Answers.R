#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:/Users/ymcelwee/Documents/QTM200Spring2020-master (1)/QTM200Spring2020-master/problem_sets/PS6")

#####################
# Problem 1
#####################

cholesterol <- read.csv("cholesterol.csv", stringsAsFactors = F, header=T)
model1 <- lm(cholCat~sex+fat, data=cholesterol)
summary(model1)

exp(0.0082466)
exp(0.1894160 + 0.0082466)
1/(1+exp(-(-0.1303597 + 0.1894160*0 + 0.0082466*100)))
#part 2, create dummy variables for sex

cholesterol$sex_d1 <- rep(0, nrow(cholesterol))
cholesterol$sex_d2 <- rep(0, nrow(cholesterol))
cholesterol[which(cholesterol$sex==0), "sex_d1"] <- as.factor("1")
cholesterol[which(cholesterol$sex==1), "sex_d2"] <- as.factor("1")

#conduct partial f-test for interaction

cholesterol_reg1 <-lm(cholCat~sex_d1 + sex_d2 + fat, data=cholesterol)
cholesterol_reg2 <- lm(cholCat~sex_d1 + sex_d2 + fat + fat:sex_d1 + fat:sex_d2, data=cholesterol)
anova(cholesterol_reg1, cholesterol_reg2)
  #the p-value of the partial f-test is not significant, so we fail to reject the null hypothesis. The interaction between fat and sex is not significantly different from 0. Thus, there is a similar linear relationship between fat and cholCat for males and females. An interaction term is not needed.


#####################
# Problem 2
#####################

#change reference group for DV, adjust variable type
gdpChange <- read.csv("gdpChange.csv", stringsAsFactors = F, header=T)
gdpChange$GDPWdiff.f <- factor(gdpChange$GDPWdiff,  labels = c("positive", "negative", "no change"))
is.factor(gdpChange$GDPWdiff.f)
gdpChange$GDPWdiff.fr <- relevel(gdpChange$GDPWdiff.f, ref = "no change")
#adjust variable types for IVs
is.numeric(gdpChange$EDT)
gdpChange$EDT.n <- as.numeric(gdpChange$EDT)
is.numeric(gdpChange$EDT.n)

#question 2 part 1
library(nnet)
unordered_logit <- multinom(GDPWdiff.fr~REG + OIL + EDT.n, data=gdpChange)
summary(unordered_logit)
exp(coef(unordered_logit)[1:4])
exp(cbind(OR = coef(unordered_logit), confint(unordered_logit)))

#question 2 part 2
library(MASS)
ordered_logit <- polr(GDPWdiff.fr~ REG + OIL + EDT.n, data=gdpChange, Hess=T)
summary(ordered_logit)
exp(coef(ordered_logit)[1:3])
exp(cbind(OR = coef(ordered_logit), confint(ordered_logit)))
