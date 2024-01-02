#STAT 659
#Homework 4

#3.3c
library(dplyr)
alc = data.frame(absent=c(17066,14464,788,126,37), present=c(48,38,5,1,1))
#total per category
n = alc$absent + alc$present
#categories
alc$x = 0:4
fit1 = glm(present/n ~ x, family=quasi(link=identity, variance = "mu(1-mu)"), weights = n, data = alc)
summary(fit1, dispersion=1)
intercept = fit1$coefficients[1]
slope = fit1$coefficients[2]

#3.3d
#fit linear probability model without the last observation
#first reproduce original
alc = data.frame(absent=c(17066,14464,788,126,37), present=c(48,38,5,1,1))
n = alc$absent + alc$present
alc$x = c(0.0, 0.5, 1.5, 4.0, 7.0)
fit2 = glm(present/n ~ x, family=quasi(link=identity, variance = "mu(1-mu)"), weights = n, data = alc)
summary(fit2)
intercept = fit2$coefficients[1]
slope = fit2$coefficients[2]

#remove last row
alc = data.frame(absent=c(17066,14464,788,126,37), present=c(48,38,5,1,0))
n = alc$absent + alc$present
alc$x = c(0.0, 0.5, 1.5, 4.0, 7.0)
fit3 = glm(present/n ~ x, family=quasi(link=identity, variance = "mu(1-mu)"), weights = n, data = alc)
summary(fit3)
intercept = fit3$coefficients[1]
slope = fit3$coefficients[2]

#3.5
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
#part a
fit1 = glm(y ~ weight, family=quasi(link=identity, variance = "mu(1-mu)"), 
           data=crab, start = c(0.01, 0.01) )
summary(fit1)
predict(fit1, newdata = data.frame(weight=5.2)) #seems to cap at 1 which is incorrect
lmfit = lm(y ~ weight, data=crab)
summary(lmfit)
predict(lmfit, newdata = data.frame(weight=5.2)) 

#part b
logitfit = glm(y ~ weight, family = binomial(link=logit),data=crab)
summary(logitfit)
predict(logitfit, newdata = data.frame(weight=5.2), type = "response")


#3.7
heart = read.table("https://users.stat.ufl.edu/~aa/cat/data/Heart.dat", header = TRUE)
heart
library(dplyr)
#part a
heart$x1 = recode(heart$snoring, never=0, occasional=2, nearly_every_night=4, every_night=6)
heart$x2 = recode(heart$snoring, never=0, occasional=1, nearly_every_night=2, every_night=3)
heart$x3 = recode(heart$snoring, never=1, occasional=2, nearly_every_night=3, every_night=4)
n = heart$yes + heart$no
fit1 = glm(yes/n ~ x1, family=binomial(link=logit), weights=n, data=heart)
fit2 = glm(yes/n ~ x2, family=binomial(link=logit), weights=n, data=heart)
fit3 = glm(yes/n ~ x3, family=binomial(link=logit), weights=n, data=heart)
summary(fit1)
summary(fit2)
summary(fit3)
fitted(fit1)
fitted(fit2)
fitted(fit3)

#part b
heart$x4 = recode(heart$snoring, never=0, occasional=2, nearly_every_night=6, every_night=7)
fit4 = glm(yes/n ~ x4, family=binomial(link=logit), weights=n, data=heart)
summary(fit4)
fitted(fit4)

#3.9
credit = read.table("https://users.stat.ufl.edu/~aa/cat/data/Credit.dat", header = TRUE)
credit
fit = glm(cards/n ~ income, weights=n, data=credit, family=binomial(lin=logit))
summary(fit)
predict(fit, newdata = data.frame(income=33.4), type = "response")

#additional problems
#problem 1 part a
library(epitools)
epitools::oddsratio(c(512,313,89,19), method = "wald")$measure
epitools::oddsratio(c(353,207,17,8), method = "wald")$measure
epitools::oddsratio(c(120,205,202,391), method = "wald")$measure
epitools::oddsratio(c(138,279,131,244), method = "wald")$measure
epitools::oddsratio(c(53,138,94,299), method = "wald")$measure
epitools::oddsratio(c(22,351,24,317), method = "wald")$measure

#problem 1 part b
library(DescTools)
berkeley = array(c(512,89, 313, 19,353,17,207,8,120,202,205,391,138,131,279,244,53,94,138,299,22,24,351,317), dim = c(2,2,6), 
                 dimnames = list(Gender=c("Male", "Female"), Admitted=c("Yes", "No"),
                 Department = c("1","2","3","4","5","6")))
berkeley
BreslowDayTest(berkeley)
mantelhaen.test(berkeley, alternative = "two.sided", conf.level = 0.90, correct = FALSE)

#problem 1 part c
berkeley = array(c(353,17,207,8,120,202,205,391,138,131,279,244,53,94,138,299,22,24,351,317), dim = c(2,2,5), 
                 dimnames = list(Gender=c("Male", "Female"), Admitted=c("Yes", "No"),
                                 Department = c("2","3","4","5","6")))
berkeley
BreslowDayTest(berkeley)
BreslowDayTest(berkeley, OR=1)
mantelhaen.test(berkeley, alternative = "two.sided", conf.level = 0.90, correct = FALSE)

#problem 2
#part a
library(epitools)
epitools::oddsratio(c(24,9,47,12), method = "wald")$measure
epitools::oddsratio(c(10,3,45,8), method = "wald")$measure
epitools::oddsratio(c(5,4,57,9), method = "wald")$measure
epitools::oddsratio(c(16,7,54,10), method = "wald")$measure
epitools::oddsratio(c(7,4,59,12), method = "wald")$measure

#part b
black_yes = 24+10+5+16+7
black_no = 9+3+4+7+4
white_yes = 47+45+57+54+59
white_no = 12+8+9+10+12
(black_yes/black_no)/ (white_yes/white_no)

#partc
district = array(c(24,47,9,12,10,45,3,8,5,57,4,9,16,54,7,10,7,59,4,12), dim = c(2,2,5),
                 dimnames = list(race=c("black", "white"), meritpay=c("yes", "no"),
                 district = c("NC", "NE", "NW", "SE", "SW")))
district
BreslowDayTest(district)
mantelhaen.test(district, alternative = "two.sided", conf.level = 0.90, correct = FALSE)
