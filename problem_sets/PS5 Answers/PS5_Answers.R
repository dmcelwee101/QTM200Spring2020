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

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

#(a) Check the constant variance assumption for the errors by plotting the residuals versus the ???tted values.
plot(model1) #first plot

#(b) Check the normality assumption with a Q-Q plot of the studentized residuals.
plot(model1) #second plot

#(c) Check for large leverage points by plotting the h values.
plot(hatvalues(model1), pch=16, cex=1, ylim=c(0,0.4))
abline(h=2*(5+1)/47, lty=2)
abline(h=3*(5+1)/47, lty=2)
identify(1:47, hatvalues(model1), row.names(gamble))

#(d) Check for outliers by running an outlierTest.
library(car)
outlierTest(model1, row.names(gamble))

#(e) Check for in???uential points by creating a "Bubble plot" with the hat-values and studentized residuals.
plot(hatvalues(model1), rstudent(model1), type="n", ylim=c(-4,8))
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty=2)
abline(v=c(2,3)*6/47, lty=2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))
influence.measures(model1)
