#Homework 7

#4.16
#part a
sore = read.table("https://users.stat.ufl.edu/~aa/cat/data/SoreThroat.dat", header = TRUE)
soremodel = glm(Y ~ T + D + D:T, data=sore, family=binomial)
summary(soremodel) #write two equations for t=1 and t=0 and interpret odds of Y
Anova(soremodel)# X2=1.8169  df=1  p-value=0.1776844 so interaction is not significant.
#other way is to fit a simpler model
soremodelsimple = glm(Y ~ T + D, data=sore, family=binomial)
anova(soremodelsimple, soremodel, test = "Chisq") # null hypothesis is interaction not needed which we fail to reject

#part b
cor(sore$Y, fitted(soremodel)) #0.6598764
cor(sore$Y, fitted(soremodelsimple)) #0.6528899
library(pROC)
rocplot1 = pROC::roc(Y ~ fitted(soremodel), data=sore)
pROC::auc(rocplot1)#0.8829
rocplot2 = pROC::roc(Y ~ fitted(soremodelsimple), data=sore)
pROC::auc(rocplot2)#0.8654 pretty close to the full model

#5.4
#part c
students = read.table("https://users.stat.ufl.edu/~aa/cat/data/Students.dat", header = TRUE)
stuvegmodel = glm(veg ~ ., family=binomial, data=students)
summary(stuvegmodel)
1-pchisq(stuvegmodel$null.deviance - stuvegmodel$deviance,
         stuvegmodel$df.null-stuvegmodel$df.residual) #p-value 3.257296e-05
# atleast one explanatory variable is not zero

library(RcmdrMisc)
model.f = glm(veg ~. , family=binomial, data=students) #note nothing is significant, so forward selection will be null
model.n = glm(veg ~ 1 , family=binomial, data=students)

out.aic.forward = stepwise(model.f, direction = "forward",
                           criterion = c("AIC"))
out.bic.forward = stepwise(model.f, direction = "forward",
                           criterion = c("BIC")) #veg ~ affirm + subject - so this gives us two variables




#5.6
mbti = read.table("https://users.stat.ufl.edu/~aa/intro-cda/data/MBTI.dat", header = TRUE)
#mbti$x1 = ifelse(mbti$EI == "e", 1,0)
#mbti$x2 = ifelse(mbti$SN == "n", 1,0)
#mbti$x3 = ifelse(mbti$TF == "f", 1,0)
#mbti$x4 = ifelse(mbti$JP == "j", 1,0)
mbti

#(a) Intercept-only model
out1 = glm(smoke/n ~ 1,family = binomial, weights = n, data = mbti)
summary(out1)
prob=predict(out1, type="response")
-2*logLik(out1) #too small because binomial!
#let's now match the book
perfectLkhd = -2*sum(mbti$smoke * log(prob)
                     + (mbti$n-mbti$smoke) * log(1-prob)) #1131.271 -- almost!
AIC(out1)#too small as expected
#let's match the book AIC
perfectLkhd  + 2 #1133.271 -- great!


#(b) Intercept + four main effects model
out2 = glm(smoke/n ~ factor(EI)+ factor(SN) + factor(TF) + factor(JP),
           family = binomial,weights = n, data = mbti)
summary(out2)
-2*logLik(out2) #too small because binomial!
prob=predict(out2, type="response")
#let's now match the book
perfectLkhd = -2*sum(mbti$smoke * log(prob)
                     + (mbti$n-mbti$smoke) * log(1-prob)) #1125.894 -- almost!
AIC(out2)#too small as expected
#let's match the book AIC
perfectLkhd  + 2*5 #1135.894 -- great!


#(c) Intercept + four main effects + two factor interactions model
out3 = glm(smoke/n ~ factor(EI)+ factor(SN) + factor(TF) + factor(JP) +
             factor(EI):factor(SN) + factor(EI):factor(TF) +
             factor(EI):factor(JP) + factor(SN):factor(TF) +
             factor(SN):factor(JP) + factor(TF):factor(JP),
           family = binomial,weights = n, data = mbti)
summary(out3)
-2*logLik(out3) #too small because binomial!
prob=predict(out3, type="response")
#let's now match the book
perfectLkhd = -2*sum(mbti$smoke * log(prob)
                     + (mbti$n-mbti$smoke) * log(1-prob)) #1120.856 -- almost!
AIC(out3)#too small as expected
#let's match the book AIC
perfectLkhd  + 2*11 #1142.856 -- great!

#one term
mbtimod1 = glm(smoke/n ~ 1, weights=n, data=mbti, family = binomial)
summary(mbtimod1) ##? doesn't match??

#problem 1
#part a
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
fit1 = glm(y ~ weight, family = binomial, data=crab)
summary(fit1)
predicted = as.numeric(fitted(fit1) > 0.642) #or predict(fit1, x=data.frame(weight=crab$weight), type="response")
xtabs(~crab$y + predicted)

#part b
library(pROC)
rocplot = roc(y ~ fitted(fit1), data=crab)
#plot.roc(rocplot, legacy.axes=TRUE) #this is wrong. see slides!
plot(1-rocplot$specificities, rocplot$sensitivities, type="l", lwd=2,
     xlab="1-Specificity", ylab="Sensitivity")
abline(a=0, b=1)
#pROC::auc(rocplot)
rocplot$auc

#part c
#Ho: beta neq 0
1-pchisq(fit1$deviance, fit1$df.residual) # do not prefer as weight is continuous!
library(generalhoslem)
logitgof(crab$y, fitted(fit1))

#Ho: beta eq 0
1-pchisq(225.76-195.74, 1)


#problem 2

library(aplore3)
data(icu)

icu$loc_binary = ifelse(icu$loc == "Nothing", 1, 0)
icu_small = subset(icu, select = -c(race, loc, id))

#part a
library(RcmdrMisc)
model.f = glm(sta ~. , family=binomial, data=icu_small)
model.n = glm(sta ~ 1 , family=binomial, data=icu_small)

out.aic.forward = stepwise(model.f, direction = "forward",
                       criterion = c("AIC"))
out.bic.forward = stepwise(model.f, direction = "forward",
                           criterion = c("BIC"))

out.aic.backward = stepwise(model.f, direction = "backward",
                           criterion = c("AIC"))
out.bic.backward= stepwise(model.f, direction = "backward",
                           criterion = c("BIC"))

out.aic.both = stepwise(model.f, direction = "backward/forward",
                            criterion = c("AIC"))
out.bic.both = stepwise(model.f, direction = "backward/forward",
                           criterion = c("BIC"))

#print all scores
AIC(out.aic.forward)
BIC(out.bic.forward)

AIC(out.aic.backward)
BIC(out.bic.backward)

AIC(out.aic.both)
BIC(out.bic.both)

#part b
library(bestglm)
#must move dependent variable to be the last column
icu_smaller = subset(icu_small, select = -c(ser, inf, po2, type))
icu_smaller = icu_smaller[, c(15, 2:14, 1)]
#gaussian which is default crashes
out.aic.subsets = bestglm(icu_smaller, family = binomial, IC="AIC")
out.bic.subsets = bestglm(icu_smaller, family = binomial, IC="BIC")
AIC(out.aic.subsets$BestModel)
AIC(out.bic.subsets$BestModel)

#compute auc
myauc = function(fit1){
  library(pROC)
  rocplot = roc(sta ~ fitted(fit1), data=icu_small)
  return (pROC::auc(rocplot))
}
out.aic.forward.auc = myauc(out.aic.forward)
out.bic.forward.auc = myauc(out.bic.forward)
out.aic.backward.auc = myauc(out.aic.backward)
out.bic.backward.auc = myauc(out.bic.backward)
out.aic.both.auc = myauc(out.aic.both)
out.bic.both.auc = myauc(out.bic.both)
out.subset.aic.auc = myauc(out.aic.subsets$BestModel)
out.subset.bic.auc = myauc(out.bic.subsets$BestModel)
