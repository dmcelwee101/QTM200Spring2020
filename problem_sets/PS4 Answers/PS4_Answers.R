install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#1a. 
Prestige$professional <- ifelse(Prestige$type=="prof", 1, 0)

#1b. 
lm1 <- lm(prestige~income+professional+income:professional, data=Prestige)
summary(lm1)

#1c.
#1d.
#1e. 
#1f.
prestige1 <- 21.1422589 + 0.0031709*(1000) + 37.7812800*(1) - 0.0023257*(1000)*(1)
prestige1

#1g.
prestige2 <- (21.1422589 +  0.0031709*(6000) + 37.7812800*(1) -0.0023257*(6000)*(1)) -(21.1422589 +  0.0031709*(6000) + 37.7812800*(0)-0.0023257*(6000)*(0)) 
prestige2

#question 2a.
p1 <-2*pt(TS1,df = 131-2-1,lower.tail=F) 
p1

#question 2b.
TS2 <- 0.042/0.013
p2 <-2*pt(TS2,df = 131-2-1,lower.tail=F) 
p2

#question 2c. 
#question 2d. 