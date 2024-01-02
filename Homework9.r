#homework 9

#problem 6.2
#For the alligator food choice example (Section 6.1.2), use the model fit to estimate
#an odds ratio that describes the effect of length on primary food choice being either
#invertebrate or other.
exp(-2.4654) #0.08497485. For a cm increase in length, the odds of preferring I over O increases by 
#multiplicative factor of 0.0849.

#problem 6.3
gators2 = read.table("https://users.stat.ufl.edu/~aa/cat/data/Alligators2.dat", header = TRUE)
gators2

gators2$lake = relevel(as.factor(gators2$lake), ref = 4)
library(VGAM)
fit6.3 = vglm(cbind(y2, y3, y4, y5, y1) ~ as.factor(size) + 
                lake,
              family = multinomial, data=gators2)
summary(fit6.3)
fit6.3.multinom = multinom(cbind(y1, y2, y3, y4, y5) ~ as.factor(size) + 
                    lake, data=gators2)
summary(fit6.3.multinom)

#problem 6.4
#For the belief in an afterlife example (Section 6.1.5), describe the gender effect by
#reporting and interpreting the estimated conditional odds ratio for the (a) undecided
#and no pair of response categories, (b) yes and undecided pair. Interpret.

#part a
# we take second model, so conditional odds is exp(-0.1051)
exp(-0.1051) #0.9002345. Estimated odds of belief in afterlife undecided/no for males is 0.9 times that for female.
#given race

#Part b
exp(-0.4186+0.1051)#0.7308844. Estimated odds of belief in afterlife ye/no for males is 0.73 times that for female.
#given race

#problem 6.5
#part a

#pi_hat(y=4) = pi_hat(y<=4) - pi_hat(y<=3)
#pi_hat(y=1) = (1/(1+exp(-alpha_4+0.54x1 -0.60x2 -1.19x3))) -
               #(1/(1+exp(-alpha_3+0.54x1 -0.60x2 -1.19x3)))
#pi_hat(y=1) = (exp(-alpha_3+0.54x1 -0.60x2 -1.19x3) - exp(-alpha_4+0.54x1 -0.60x2 -1.19x3))/
             # ((1+exp(-alpha_4+0.54x1 -0.60x2 -1.19x3))*(1+exp(-alpha_3+0.54x1 -0.60x2 -1.19x3)))
# so, pi_hat(y=4) increase with x1 and decrease with x2.
#pi_hat(y=1) = pi_hat(y<=1) = (1/(1+exp(-alpha_1+0.54x1 -0.60x2 -1.19x3)))
#pi_hat(y=1) = (exp(alpha_1-0.54x1+0.60x2+1.19x3))/(1+exp(alpha_1-0.54x1+0.60x2+1.19x3))
#here the sign of x1, x2 is opposite
#part b
# we want logit(y<=4) to be the smallest so we need x1=4 and x2=x3=1

#problem 6.6


#problem 6.10
Mental = read.table("https://users.stat.ufl.edu/~aa/cat/data/Mental.dat", header = TRUE)
Mental
library(MASS)
y = factor(Mental$impair)
fit6.10 = polr(y ~ life + ses + life:ses, method="logistic", data=Mental)
summary(fit6.10)
2 * (1-pt(abs(-0.7608), 39))
predict(fit6.10, data.frame(ses=0, life=min(Mental$life)), type="probs")
predict(fit6.10, data.frame(ses=1, life=min(Mental$life)), type="probs")
predict(fit6.10, data.frame(ses=0, life=max(Mental$life)), type="probs")
predict(fit6.10, data.frame(ses=1, life=max(Mental$life)), type="probs")

predict(fit6.10, data.frame(ses=0, life=mean(Mental$life)), type="probs")
predict(fit6.10, data.frame(ses=1, life=mean(Mental$life)), type="probs")

#problem 6.14

environment.df = data.frame(
  agree = c(172,111),
  neutral = c(57,78),
  disagree = c(82,283),
  party = c("republican", "democrat")
)

environment.df



#Do it using neutral baseline
fit6.14.neutral = vglm(cbind(agree, disagree, neutral) ~ party, family=multinomial, data=environment.df)
summary(fit6.14.neutral)

#with vglm and multinom with dataframe as in the problem

#construct dataframe
environment.df = data.frame(
  agree = c(172,111),
  neutral = c(57,78),
  disagree = c(82,283),
  party = c("republican", "democrat")
)

environment.df
# agree neutral disagree      party
# 1   172      57       82 republican
# 2   111      78      283   democrat


#first try multinom
library(nnet)
fit6.14.multinom = multinom(cbind(agree, neutral, disagree) ~ as.factor(party), data=environment.df)
summary(fit6.14.multinom)
#OUTPUT
# Call:
#   multinom(formula = cbind(agree, neutral, disagree) ~ as.factor(party), 
#            data = environment.df)
# 
# Coefficients:
#   (Intercept) as.factor(party)republican
# neutral   -0.3531558                 -0.7509507
# disagree   0.9356582                 -1.6763461
# 
# Std. Errors:
#   (Intercept) as.factor(party)republican
# neutral    0.1477471                  0.2125632
# disagree   0.1119860                  0.1747867
# 
# Residual Deviance: 1507.506 
# AIC: 1515.506 


fit6.14.multinom.null =  multinom(cbind(agree, neutral, disagree) ~ 1, data=environment.df)
summary(fit6.14.multinom.null)
#OUTPUT
# Call:
#   multinom(formula = cbind(agree, neutral, disagree) ~ 1, data = environment.df)
# 
# Coefficients:
#   (Intercept)
# neutral   -0.7401722
# disagree   0.2544504
# 
# Std. Errors:
#   (Intercept)
# neutral   0.10459912
# disagree  0.07920413
# 
# Residual Deviance: 1607.794 
# AIC: 1611.794 

library(lmtest)
lrtest(fit6.14.multinom.null,fit6.14.multinom) #party has en effect

#next try vglm
library(VGAM)
fit6.14 = vglm(cbind(agree, neutral, disagree) ~ as.factor(party), family=multinomial, data=environment.df)
# Warning messages:
#   1: In vglm.fitter(x = x, y = y, w = w, offset = offset, Xm2 = Xm2,  :
#                       iterations terminated because half-step sizes are very small
#                     2: In vglm.fitter(x = x, y = y, w = w, offset = offset, Xm2 = Xm2,  :
#                                         some quantities such as z, residuals, SEs may be inaccurate due to convergence at a half-step
summary(fit6.14)
# Call:
#   vglm(formula = cbind(agree, neutral, disagree) ~ as.factor(party), 
#        family = multinomial, data = environment.df)
# 
# Coefficients: 
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1                 -0.9359     0.1120  -8.357  < 2e-16 ***
#   (Intercept):2                 -1.2887     0.1279 -10.077  < 2e-16 ***
#   as.factor(party)republican:1   1.6767     0.1748   9.593  < 2e-16 ***
#   as.factor(party)republican:2   0.9251     0.2147   4.309 1.64e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Names of linear predictors: log(mu[,1]/mu[,3]), log(mu[,2]/mu[,3])
# 
# Residual deviance: -1.215e-13 on 0 degrees of freedom
# 
# Log-likelihood: -11.8865 on 0 degrees of freedom
# 
# Number of Fisher scoring iterations: 4 
# 
# No Hauck-Donner effect found in any of the estimates
# 
# 
# Reference group is level  3  of the response

#The previous model is saturated so we need to increase the length of the dataframe
#construct dataframe
environment.df.long = data.frame(
  vote = rep(colnames(environment.df)[1:3],2),
  count = as.numeric(c(environment.df[1, 1:3], environment.df[2, 1:3])),
  party = c(rep("republican",3), rep("democrat",3))
)

environment.df.long

#note we have to put weight here because we don't use cbind
fit6.14 = vglm(vote ~ as.factor(party), family=multinomial(refLevel = 'disagree'), data=environment.df.long, weight=count)
summary(fit6.14)
# Call:
#   vglm(formula = vote ~ as.factor(party), family = multinomial(refLevel = "disagree"), 
#        data = environment.df.long, weights = count)
# 
# Coefficients: 
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1                 -0.9359     0.1120  -8.357  < 2e-16 ***
#   (Intercept):2                 -1.2887     0.1279 -10.077  < 2e-16 ***
#   as.factor(party)republican:1   1.6767     0.1748   9.593  < 2e-16 ***
#   as.factor(party)republican:2   0.9251     0.2147   4.309 1.64e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Names of linear predictors: log(mu[,1]/mu[,2]), log(mu[,3]/mu[,2])
# 
# Residual deviance: 1507.506 on 8 degrees of freedom
# 
# Log-likelihood: -753.7531 on 8 degrees of freedom
# 
# Number of Fisher scoring iterations: 4 
# 
# No Hauck-Donner effect found in any of the estimates
# 
# 
# Reference group is level  2  of the response



fit6.14.null = vglm(vote ~ 1, family=multinomial(refLevel = 'disagree'), data=environment.df.long, weight=count)
summary(fit6.14.null)
# Call:
#   vglm(formula = vote ~ 1, family = multinomial(refLevel = "disagree"), 
#        data = environment.df.long, weights = count)
# 
# Coefficients: 
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept):1  -0.2545     0.0792  -3.213  0.00132 ** 
#   (Intercept):2  -0.9946     0.1007  -9.874  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Names of linear predictors: log(mu[,1]/mu[,2]), log(mu[,3]/mu[,2])
# 
# Residual deviance: 1607.794 on 10 degrees of freedom
# 
# Log-likelihood: -803.8968 on 10 degrees of freedom
# 
# Number of Fisher scoring iterations: 4 
# 
# No Hauck-Donner effect found in any of the estimates
# 
# 
# Reference group is level  2  of the response


lrtest(fit6.14, fit6.14.null) #party has an effect
# Model 1: vote ~ as.factor(party)
# Model 2: vote ~ 1
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   8 -753.75                         
# 2  10 -803.90  2 100.29  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1