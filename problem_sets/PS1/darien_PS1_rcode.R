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
setwd("C:/Users/ymcelwee/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#confidence coefficient is 0.90
#using qtnorm because n is <30
library(msm)
z90 <- qtnorm((1-0.90)/2, lower.tail = FALSE)
n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)
lower_90 <- sample_mean - (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90)

#The confidence interval is (94.1, 102.7)

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t.test(y, mu = 100, alternative = "greater", df=24)

#The school counselor's students do not have a significantly higher average IQ than the average IQ of 100 of students across the country


#####################
# Problem 3
#####################

y2 <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
expenditure <- read.table("expenditure.txt", header=T)

library(ggplot2)
ggplot(expenditure, aes(x=X1, y=Y)) + geom_point()
#There is a linear, moderate, positive relationship between per capita personal income and per capita expenditure on public education
ggplot(expenditure, aes(x=X2, y=Y)) + geom_point()
#There does not appear to be a relationship between number of residents per thousand under 18 years old and per capita expenditure on public education.
ggplot(expenditure, aes(x=X3, y=Y)) + geom_point()
#There is a linear, weak, positive relationship between number of people per thousand residing in urban areas  and per capita expenditure on public education


#Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on public education?
ggplot(data=expenditure, aes(x=Region, y=Y)) + geom_bar(stat="identity")
#Region 4 (West) has the highest per capita expenditure on public education.

#Please plot the relationship between Y and X1? Describe this graph and the relationship. 
ggplot(expenditure, aes(x=X1, y=Y)) + geom_point()
#This is a scatterplot showing that there is a linear, moderate, positive relationship between per capita personal income and per capita expenditure on public education

#Reproduce the above graph including one more variable Region and display diﬀerent regions with diﬀerent types of symbols and colors.
expenditure$Region <- factor(expenditure$Region)
is.factor(expenditure$Region)
ggplot(expenditure, aes(x=X1, y=Y, col=Region, shape = Region)) + geom_point()

       