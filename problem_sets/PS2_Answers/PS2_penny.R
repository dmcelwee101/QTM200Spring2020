#question 1a: calculate fexpected for each cell
##calculate row total for upper class
14+6+7
##calculate row total for lower class
7+7+1
##calculate grand total, add both row totals
27+15
##calculate column total for not stopped 
14+7
##calculate column total bribe requested
6+7
##calculate column total for warning
7+1

##fe for not stopped, upper class
fe1 <- (27/42)*21
##fe for bribe requested, upper class
fe2 <- (27/42)*13
##fe for warning, upper class
fe3 <- (27/42)*8
##fe for not stopped, lower class
fe4 <- (15/42)*21
##fe for bribe requested, lower class
fe5 <- (15/42)*13
##fe for warning, lower class
fe6 <- (15/42)*8

#calculate the chi-squared statistic by using sum(fo - fe)^2 / fe
x <- ((14-fe1)^2/fe1) + ((6-fe2)^2/fe2) + ((7-fe3)^2/fe3) + ((7-fe4)^2/fe4) + ((7-fe5)^2/fe5) + ((1-fe6)^2/fe6)
x
##The chi-squared statistic is 3.791168.

#question 1b: calculate the p-value
##df = 2 because there are 3 columns and two rows so (3-1)(2-1) = 2
pchisq(x, df=2, lower.tail = FALSE)
#because the p-value (0.15) is not equal to or below the 0.1 threshold, we do not find sufficient evidence to reject the null hypothesis that the variables are statistically independent.

#question 1c: calculate the standardized residuals for each cell
##calculate the standard error for each cell
##calculate se for not stopped, upper class
1-(27/42)
1-(21/42)
se1 <- sqrt(fe1*0.357*0.5)
##calculate se for bribe requested, upper class
1-(27/42)
1-(13/42)
se2 <- sqrt(fe2*0.3571429*0.6904762)
##calculate se for warning, upper class
1-(27/42)
1-(8/42)
se3 <- sqrt(fe3*0.3571429*0.8095238)
##calculate se for not stopped, lower class
1-(15/42)
1-(21/42)
se4 <- sqrt(fe4*0.6428571*0.5)
##calculate se for bribe requested, lower class
1-(15/42)
1-(13/42)
se5 <- sqrt(fe5*0.6428571*0.6904762)
##calculate se for warning, lower class
1-(15/42)
1-(8/42)
se6 <- sqrt(fe6*0.6428571*0.8095238)

##calculte standard residual for not stopped, upper class
z1 <- (14-fe1)/se1
z1
###the standardized residual of not stopped, upper class is about 0.322

##calculate standard residual for bribe requested, upper class
z2 <- (6-fe2)/se2
z2
###the standardized residual of bribe requested, upper class is about -1.642

##calculate standard residual for warning, upper class
z3 <- (7-fe3)/se3
z3
###the standardized residual of warning, upper class is about 1.523

##calculate standard residual for not stopped, lower class
z4 <- (7-fe4)/se4
z4
###the standardized residual of not stopped, upper class is about -0.322

##calculate standard residual for bribe requested, lower class
z5 <- (7-fe5)/se5
z5
###the standardized residual of bribe requested, lower class is about 1.642

##calculate standard residual for warning, lower class
z6 <- (1-fe6)/se6
z6
###the standardized residual of warning, lower class is about -1.523

#question 2, read in data
women <- read.csv("~/GitHub/QTM200Spring2020-master/QTM200Spring2020-master/problem_sets/PS2/women.csv")
View(women)
#next, calculate sums and means
mean.x <- mean(women$reserved)
mean.y <- mean(women$water)
sum(women$reserved)
sum(women$water)
numerator <- sum((women$water - mean(women$water))*(women$reserved - mean(women$reserved)))
denominator <- sum((women$reserved - mean(women$reserved))^2)
#calculate regression coefficients
beta.hat <- numerator/denominator 
beta.hat
alpha.hat <- mean.y - (beta.hat*mean.x)
alpha.hat
#calculate p-value, start with sd, then se, then test statistic then
sd.y <- sd(women$water)
se.y <- sd.y/sqrt(sum((women$reserved - mean.x)^2))
TS <- (beta.hat - 0) / se.y
TS
p <- 2*pt(abs(TS), df= (length(women$water)), lower.tail=F)
#check
women.lm <- lm(water~reserved, data=women)
summary(women.lm)

#question 3.1, import dataset
fruitfly <- read.csv("~/GitHub/QTM200Spring2020-master/QTM200Spring2020-master/problem_sets/PS2/fruitfly.csv")
summary(fruitfly)
#Summary statistics for lifespan--min 16 days, 25th percentile: 46 days, median: 58 days, mean:57.44 days, 75th percentile: 70 days, max: 97 days. (Summary statistics for each variable can be found by executing above function).
##examine distribution of overall lifespan of fruitflies
hist(fruitfly$lifespan)

#question 3.2, plot lifespan vs thorax, calculate correlation coefficient
plot(lifespan~thorax, data=fruitfly)
mean.lifespan <- mean(fruitfly$lifespan)
mean.thorax <- mean(fruitfly$thorax)
sd.lifespan <- sd(fruitfly$lifespan)
sd.thorax <- sd(fruitfly$thorax)
r.fruitfly <- (1/(length(fruitfly$lifespan)-1))*sum(((fruitfly$lifespan - mean.lifespan)/sd.lifespan)*(fruitfly$thorax - mean.thorax)/sd.thorax)
r.fruitfly
#check
cor(fruitfly$lifespan, fruitfly$thorax)

#question 3.3 regress lifespan on thorax, interpret the slope of the fitted model
##step 1: calculate sums and means (already had means from the previous problem)
mean.thorax
mean.lifespan
thorax.sum <- sum(fruitfly$thorax)
lifespan.sum <- sum(fruitfly$lifespan)

big.sum <- sum((fruitfly$lifespan - mean(fruitfly$lifespan))*(fruitfly$thorax-mean(fruitfly$thorax)))
big.sum

small.sum <- sum((fruitfly$thorax-mean(fruitfly$thorax))^2)
small.sum
beta.hat.flies <- big.sum/small.sum
beta.hat.flies
alpha.hat.flies <- mean.lifespan - (beta.hat.flies*mean.thorax)
alpha.hat.flies
#check
flies.lm <- lm(lifespan~thorax, data=fruitfly)
summary(flies.lm)

##question 3.4 test for a significant linear relationship betwen lifespan and thorax
TS.flies <- (r.fruitfly*(sqrt(length(fruitfly$lifespan)-2))/(sqrt(1-(r.fruitfly)^2)))
TS.flies
p.flies <- 2*pt(TS.flies, length(fruitfly$lifespan)-2, lower.tail=F)
p.flies

##question 3.5, run a 90% confidence interval for the slope of the fitted model
confint(flies.lm, level = 0.90)

##question 3.6 Use the predict() function in R to (1) predict an individual fruit???y's lifespan when thorax=0.8 
##this is going to be a prediction interval
new_DF <- fruitfly; new_DF$thorax <- 0.8
predict(lm(new_DF$lifespan~new_DF$thorax), newdata=new_DF, interval ="prediction", level=0.95)

##question 3.6 cont.and (2) the average lifespan of fruit???ies when thorax=0.8 by the ???tted model.
##this is going to be a confidence interval
predict(lm(new_DF$lifespan~new_DF$thorax), newdata=new_DF, interval ="confidence", level=0.95)

##question 3.7
sequence <- fruitfly[30:59,]
#prediction interval
predict(lm(sequence$lifespan~sequence$thorax), newdata=sequence, interval ="prediction", level=0.95)
#confidence interval
predict(lm(sequence$lifespan~sequence$thorax), newdata=sequence, interval ="confidence", level=0.95)
#create the fitted plot
plot(sequence$thorax, sequence$lifespan, pch=19, main = "Residuals for Lifespan vs Thorax")
sequence.fit <- lm(lifespan~thorax, data=sequence)
abline(sequence.fit)
preds <- predict(sequence.fit)
segments(sequence$thorax, sequence$lifespan, sequence$thorax, preds)
