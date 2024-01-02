#homework 6
#4.1

#part a
li = 26
exp(-3.77714+0.14486*li) / (1+exp(-3.77714+0.14486*li))
#part b
exp(0.144486)

#predict
# table 4.5
LI <- c(8, 8, 10, 10, 12, 12, 12, 14, 14, 14, 16, 16, 16, 18, 20, 20, 20, 22, 22,
        24, 26, 28, 32, 34, 38, 38, 38)
y <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1,
       1, 0)
fit <- glm(y ~ LI, family = binomial)
summary(fit)
predict(fit, newdata = data.frame(LI=26), type="response")

#part c
LI = c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
LIu = unique(LI)
q = quantile(LIu, type=2)
q

low=q[2]
high=q[4]
low
high
pi = exp(-3.77714+0.14486*low) / (1+exp(-3.77714+0.14486*low))
pi
pi = exp(-3.77714+0.14486*high) / (1+exp(-3.77714+0.14486*high))
pi


#part d
pi = exp(-3.77714+0.14486*8) / (1+exp(-3.77714+0.14486*8))
0.144486 * pi * (1-pi)

#part e
pi_vec = exp(-3.77714+0.14486*LI) / (1+exp(-3.77714+0.14486*LI))
mean(0.144486 * pi_vec * (1-pi_vec))

#4.2
LI = c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,
          + 38,38,38)
y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)
mod1 = glm(y ~ LI, family=binomial)
summary(mod1) # LI effect: z-value 2.441 and p-value 0.01464 is significant at 0.05.
coef(mod1)
#Wald CI
exp(0.14486 - 1.96 * 0.05934)
exp(0.14486 + 1.96 * 0.05934)
exp(confint(mod1)) #(1.0434402672 1.3293190) is the profile likelihood confidence interval
library(car)
Anova(mod1) #X2 = 8.2988, p-value=0.003967 so LI effect is significant.

#4.5
flight = data.frame(
  Temperature = c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 
                  67, 53, 67, 75, 70, 81, 76, 79, 75, 76, 58),
  TD = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 
         0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1)
)
flight
#part a
fmod1 = glm(TD ~ Temperature, family = binomial, data=flight)
summary(fmod1) #p-value is 0.0320 so the effect is significant.

#part b
#linear model is 15.0429 - 0.2322 * temperature
exp(coef(fmod1)[1] - coef(fmod1)[2]* 31) / (1 + exp(coef(fmod1)[1] - coef(fmod1)[2]* 31)) #1 almost 1.

#part c
(log(0.5/0.5) - 15.0429)/ (-0.2322) # temperature = 64.78424.
0.5 * ( 1- 0.5) * (-0.2322) # rate of change of probability is -0.05805 per degree increase in temperature.

#part d
exp(coef(fmod1)[2]) # for each degree increase in temperature the odds of TD increase by a multiplicative factor of 0.7928171 .

#part e
#Wald statistic (-2.145) ^ 2 with p-value 0.0320
Anova(fmod1) #LR-7.952 and p-value = 0.004804. Effect of temperature is significant.

#4.7
library(car)
Kyphosis = read.table("https://users.stat.ufl.edu/~aa/cat/data/Kyphosis.dat", header = TRUE)
Kyphosis
#part a
kyphosismod1 = glm(y ~ x, family = binomial, data=Kyphosis)
summary(kyphosismod1) #wald = 0.734^2 and p-value=0.463 so not significant
Anova(kyphosismod1) # LR=0.54689 and p-value=0.4596 so not significant

#part b
plot(Kyphosis$y ~ Kyphosis$x, main = "Kyphosis versus age", xlab = "age", ylab = "Kyphosis")
#no idea how to plot line.

#part c
kyphosismod2 = glm(y ~ x + I(x^2), family = binomial, data=Kyphosis)
summary(kyphosismod2) #p-value for x^2 is 0.036
Anova(kyphosismod2) #p-value is 0.012237 so square term is significant

#4.9

#part a
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
fit1 = glm(y ~ as.factor(color), family = binomial, data=crab)
summary(fit1)

1/exp(-1.8608) # color1_odds = exp(alpha). color4_odds = exp(alpha) * exp(color4). So, the ratio is 1/exp(color4).

#part b
library(car)
Anova(fit1)

#part c
fit2 = glm(y ~ color, family = binomial, data=crab)
summary(fit2)
exp(-0.7147) 
library(car)
Anova(fit2)

#part e
fit3 = glm(y ~ weight + color, family = binomial, data=crab)
summary(fit3)
sd(crab$weight)
sd(crab$color)
1.6531*0.5770252
-0.5142*0.8019334

#4.10
#part a
aids = data.frame(
  race = c("white", "white", "black", "black"),
  azt = c("yes", "no", "yes", "no"),
  yes = c(14,32,11,12),
  no = c(93,81,52,43)
)
aids
aidsmod = glm(yes / (yes+no) ~ azt + race, weights = yes+no, family = binomial, data=aids)
summary(aidsmod)
#compares null model with beta1=beta2=0 and one where at least one is 0
Anova(aidsmod)
#part b
##???


#4.12
mbti = read.table("https://users.stat.ufl.edu/~aa/intro-cda/data/MBTI.dat", header = TRUE)
mbti
mbti$x1 = ifelse(mbti$EI == "e", 1,0)
mbti$x2 = ifelse(mbti$SN == "n", 1,0)
mbti$x3 = ifelse(mbti$TF == "f", 1,0)
mbti$x4 = ifelse(mbti$JP == "j", 1,0)
mbtimod = glm(drink/n ~ x1+x2+x3+x4, weights=n, data=mbti, family = binomial)
summary(mbtimod)

#it will pick whatever is first in the data
mbtimodtest = glm(drink/n ~ EI+SN+TF+JP, data=mbti, family = binomial)
summary(mbtimodtest)

#from pdf
glm(drink/n ~ factor(EI) + factor(SN) + factor(TF) + factor(JP), weights = n, family = binomial,
    data = mbti)

#4.19

#part a
fit4 = glm(y ~ width + factor(color) + width:factor(color), family = binomial,
           data=crab)
summary(fit4)

#match the book
adjColor = relevel(factor(crab$color), ref = "4")
fit5matchthebook = glm(y ~ width + adjColor + width:adjColor, family = binomial,
                       data=crab)
summary(fit5matchthebook)

#plots
y1 = exp(-1.75261 + 0.10600 * crab$width) / (1 + exp(-1.75261 + 0.10600 * crab$width))
y2 = exp(-10.03996 + 0.41887 * crab$width) / (1 + exp(-10.03996 + 0.41887 * crab$width))
y3 = exp(-21.51806 + 0.85837 * crab$width) / (1 + exp(-21.51806 + 0.85837 * crab$width))
y4 = exp(-5.85383 + 0.20043 * crab$width) / (1 + exp(-5.85383 + 0.20043 * crab$width))

i <- order(crab$width)
plot(crab$width[i], y1[i], main = "Probability versus width for different colors", xlab = "width", 
     ylab = "Probability", col="red", type = "l",  ylim = c(0.0, 1.0))
lines(crab$width[i], y2[i], col="blue")
lines(crab$width[i], y3[i], col="green")
lines(crab$width[i], y4[i], col="black")
legend(28, 0.2, legend=c("color1", "color2", "color3", "color4"),
       col=c("red", "blue", "green", "black"), lty=1:2, cex=0.8)

#part b
fit5 =  glm(y ~ width + factor(color), family = binomial,
            data=crab)
summary(fit5)
library(car)
anova(fit5, fit4, test = "Chisq")
corr1 <- cor(crab$y,fitted(fit4)) #interaction
corr2 <- cor(crab$y,fitted(fit5)) #simpler

#problem 1
berkeley.df = data.frame(
  Department= c(1,1, 2,2,3,3,4,4,5,5,6,6),
  Gender = rep(c("Male", "Female"), 6),
  Admitted_yes = c(512,89,353,17,120,202,138,131,53,94,22,24),
  Admitted_no = c(313,19,207,8,205,391,279,244,138,299,351,317)
)
berkeley.df$n = berkeley.df$Admitted_no+ berkeley.df$Admitted_yes
#part a
#individual fits
for(i in 1:6){
  print(paste('****Fitting department ', i))
  depti = berkeley.df[berkeley.df$Department == i, ]
  fitdepti = glm(Admitted_yes/n ~ Gender, 
                 data=depti, weights=n, 
                 family = binomial(link = "logit"))
  print(summary(fitdepti))
  print(exp(confint(fitdepti)))
  print(exp(coef(fitdepti)[2] + c(qnorm(0.025), qnorm(0.975)) * summary(fitdepti)$coefficients[2,2]))
}

#part b

##breslow test
#with interaction terms
fitbreslow = glm(Admitted_yes/n ~ Gender + factor(Department) + Gender : factor(Department), 
             data=berkeley.df, weights=n, family = binomial(link = "logit"))
summary(fitbreslow)
#without interaction terms
fitbasebreslow =  glm(Admitted_yes/n ~ Gender + factor(Department), 
                      data=berkeley.df, weights=n, family = binomial(link = "logit"))
summary(fitbasebreslow)
anova(fitbasebreslow, fitbreslow, test = "Chisq")

#CMH test
fitcmh = glm(Admitted_yes/n ~ Gender + factor(Department), 
           data=berkeley.df, weights=n, family = binomial(link = "logit"))
summary(fitcmh)

#part c
#remove department 1
berkeley.df.nodept1 = berkeley.df[berkeley.df$Department != 1, ]
##breslow test
#with interaction terms
fitbreslow = glm(Admitted_yes/n ~ Gender + factor(Department) + Gender : factor(Department), 
                 data=berkeley.df.nodept1, weights=n, family = binomial(link = "logit"))
summary(fitbreslow)
#without interaction terms
fitbasebreslow =  glm(Admitted_yes/n ~ Gender + factor(Department), 
                      data=berkeley.df.nodept1, weights=n, family = binomial(link = "logit"))
summary(fitbasebreslow)
anova(fitbasebreslow, fitbreslow, test = "Chisq")
#Wald CI
exp(coef(fitbasebreslow)[2] + c(qnorm(0.05), qnorm(0.95)) * summary(fitbasebreslow)$coefficients[2,2])

#CMH test
fitcmh = glm(Admitted_yes/n ~ Gender + factor(Department), 
             data=berkeley.df.nodept1, weights=n, family = binomial(link = "logit"))
summary(fitcmh)
#common odds ratio
exp(coef(fitcmh)[2])

#problem 2

district.df = data.frame(
  district = c("NC","NC", "NE", "NE", "NW", "NW", "SE", "SE", "SW", "SW"),
  race = rep(c("black", "white"), 5),
  meritpay_yes = c(24,47,10,45,5,57,16,54,7,59),
  meritpay_no = c(9,12,3,8,4,9,7,10,4,12)
)
district.df$n = district.df$meritpay_yes+district.df$meritpay_no
district.df

#breslow test
#with interaction terms
fitbreslow = glm(meritpay_yes/n ~ race + factor(district) + race : factor(district), 
                 data=district.df, weights=n, family = binomial(link = "logit"))
summary(fitbreslow)
#without interaction terms also for CMH
fitbasebreslow = glm(meritpay_yes/n ~ race + factor(district), 
                     data=district.df, weights=n, family = binomial(link = "logit"))
summary(fitbasebreslow)
anova(fitbasebreslow, fitbreslow, test = "Chisq")
