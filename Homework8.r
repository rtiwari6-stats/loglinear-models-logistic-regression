#Homework 8

#4.3
cancer = data.frame(
  LI = c(8,10,12,14,16,18,20,22,24,26,28,32,34,38),
  n = c(2,2,3,3,3,1,3,2,1,1,1,1,1,3),
  remissions = c(0,0,0,0,0,1,2,1,0,1,1,0,1,2)
)
summary(glm(remissions / n ~ LI, family=binomial, weights=n, data=cancer))
#MLE are same. Null and residual deviances have changed.
# Number of parameters in the saturated model is different. It is 14 for grouped data but 27 for ungrouped.

#4.24
#grouped
grouped.df = data.frame(
  x =c(0,1,2),
  n = c(4,4,4),
  success = c(1,2,4)
)
grouped.df
#ungrouped
ungrouped.df = data.frame(
  y = c(1,0,0,0,1,1,0,0,1,1,1,1),
  x = c(0,0,0,0,1,1,1,1,2,2,2,2)
)
ungrouped.df

#part a
#Likelihoods should come out to be the same?
model4.24agrouped = glm( success/n ~ x, data=grouped.df, family=binomial, weights=n)
-2 * logLik(model4.24agrouped) #  4.67215 (df=2)

model4.24agroupednull = glm( success/n ~ 1, data=grouped.df, family=binomial, weights=n)
-2 * logLik(model4.24agroupednull) #  9.944531 (df=1)


model4.24aungrouped = glm( y ~ x, data=ungrouped.df, family=binomial)
-2 * logLik(model4.24aungrouped) #11.02826 (df=2)

model4.24aungroupednull = glm( y ~ 1, data=ungrouped.df, family=binomial)
-2 * logLik(model4.24aungroupednull) #16.30064 (df=1)


#part b
summary(model4.24agrouped) #D0 = 6.2568, D1=0.9844 
summary(model4.24aungrouped) #D0=16.301, D1=11.028
#deviances are different and depend on form of data entry because the number of observations and hence saturated model is different

#part c
#wheren you take the difference of the deviances, the saturated likelihood cancels our and only L0 and L1 remain.

#5.8
dpenalty.df = data.frame(
  victim_race = c("white", "white", "black", "black"),
  defendant_race = c("white", "black", "white", "black"),
  yes = c(53,11,0,4),
  no = c(414,37,16,139)
)
dpenalty.df$n = dpenalty.df$yes + dpenalty.df$no

dpenalty.df

fit1 = glm(yes / n ~ victim_race + defendant_race, family = binomial, weights=n, data=dpenalty.df)
summary(fit1)
1-pchisq(fit1$deviance,fit1$df.residual)

#with explicit indicators
dpenalty.df$victim_race_i = ifelse(dpenalty.df$victim_race == "white", 1 ,0)
dpenalty.df$defendant_race_i = ifelse(dpenalty.df$defendant_race == "white", 1, 0)

fit1i = glm(yes / n ~ victim_race_i + defendant_race_i , family = binomial, weights=n, data=dpenalty.df)
summary(fit1i) #does not change the results

rstandard(fit1, type = "pearson")

#5.9

berkeley.df = data.frame(
  Department= c(1,1, 2,2,3,3,4,4,5,5,6,6),
  Gender = rep(c("Male", "Female"), 6),
  Admitted_yes = c(512,89,353,17,120,202,138,131,53,94,22,24),
  Admitted_no = c(313,19,207,8,205,391,279,244,138,299,351,317)
)
berkeley.df$n = berkeley.df$Admitted_no+ berkeley.df$Admitted_yes

#part a
# see here if we do not use factor, it assumes department is continuous
fit2 = glm(Admitted_yes / n ~ factor(Department), weights=n, family = binomial, 
           data=berkeley.df)
summary(fit2)
1-pchisq(fit2$deviance, fit2$df.residual)
rstandard(fit2, type = "pearson")

#part b
#try to get the odds ratio - we will just do the simple model
fit3 = glm(Admitted_yes / n ~ Gender + factor(Department), weights=n, family = binomial, 
           data=berkeley.df)
summary(fit3)
exp(coef(fit3)[2]) #common odds ratio

#5.10
lungs = read.table("https://users.stat.ufl.edu/~aa/cat/data/Lungs.dat", header = TRUE)
lungs
lungs$n = lungs$Yes + lungs$No
#fit base glm
lungsglmbase = glm(Yes/n ~ factor(City) + Smoking, data=lungs, weights=n, family = binomial)
summary(lungsglmbase) #smoking is significant and so is Taiyuan's effect
1-pchisq(lungsglmbase$deviance, lungsglmbase$df.residual) #p-value = 0.6360822 so a good fit. Note null hypothesis is a good fit.

#fit glm with interaction
lungsglminteraction = glm(Yes/n ~ factor(City) + Smoking + factor(City):Smoking, 
                          data=lungs, weights=n, family = binomial)
summary(lungsglminteraction) #interactions seem useless?

#Breslow's
anova(lungsglmbase, lungsglminteraction, test="Chisq") #p-value 0.6361 which means interactions not needed

#Use CMH to get common odds ratio
exp(coef(lungsglmbase)[9]) #2.175072 common odds ratio of cancer from smoking=yes to smoking=no.

#use rstandard to describe lack of fit
rstandard(lungsglmbase, type = "pearson") #all within the range so model is adequate.

#5.11
#
students = read.table("https://users.stat.ufl.edu/~aa/cat/data/Students.dat", header = TRUE)
#initial main effects models
#potential explanatory variables are ideol,relig, news, hsgpa, and gender
glm1 =  glm(abor ~ ideol, family = binomial, data = students)
glm2 =  glm(abor ~ relig, family = binomial, data = students)
glm3 =  glm(abor ~ news, family = binomial, data = students)
glm4 =  glm(abor ~ hsgpa, family = binomial, data = students)
glm5 =  glm(abor ~ gender, family = binomial, data = students)
summary(glm1) #ideol significant
summary(glm2) #relig significant
summary(glm3) # news significant
summary(glm4) #hsgpa not significant
summary(glm5) # gender not significant

#now create combined models
glm6 = glm(abor ~ ideol + relig + news, family = binomial, data = students) #relig not significant
glm7 = glm(abor ~ ideol + relig, family = binomial, data = students) # relig not significant
glm8 = glm(abor ~ ideol + news, family = binomial, data = students) # all good
summary(glm6)
summary(glm7)
summary(glm8)

# check with other predictors
# we are removing relig
glm9 = glm(abor ~ ideol + news + hsgpa, family = binomial, data = students)
glm10 = glm(abor ~ ideol + news + gender, family = binomial, data = students)
summary(glm9) #hsgpa not significant
summary(glm10) # gender not significant

#add interaction terms
glm11 = glm(abor ~ ideol + news + ideol * news, family = binomial, data=students)
summary(glm11) #interaction not significant so we go with ideol+news.

finalglm = glm(abor ~ ideol + news, family = binomial, data = students)
summary(finalglm)
1 - pchisq(finalglm$deviance, finalglm$df.residual)  #p-value 0.9969725 which is large so no lack of fit. Shouldn't this only make sense for grouped data?
rstandard(finalglm) # data isn't grouped so not useful.

#5.12
# part a let's check
#Suppose y = 0 at x = 0, 10, 20, 30 and y = 1 at x = 70, 80, 90, 100.
x = c(0,10,20,30,70,80,90,100)
y = c(0,0,0,0,1,1,1,1)
fit5.12 = glm(y ~ x, family = binomial)
summary(fit5.12) # huge SE 2368.996, z-value is zero p-value=1. Wald test is useless.
# When x <= 40, pihat = 0 so logit = -inf. When x > 40, pihat=1 and logit=inf. So the MLE of beta is inf because logit increases from -inf to inf.

#part b
#Add two observations at x = 50, y = 1 for one and y = 0 for the other
x = c(0,10,20,30,50, 50, 70,80,90,100)
y = c(0,0,0,0,0,1,1,1,1,1)
fit5.12b1 = glm(y ~ x, family = binomial)
summary(fit5.12b1) #beta-hat = 1.011, SE=528.572 so SE gets reduced, z-value=0.002 and p-value=0.998 so slight improvement but this is quasi-complete separation.
# in fit5.12b1, the algorithm to find the MLE is not converging. 

#What happens if you replace the two observations by y = 1 at x = 49.9 and y = 0 at x = 50.1?
x = c(0,10,20,30, 50.1, 49.9, 70,80,90,100)
y = c(0,0,0,0,0,1,1,1,1,1)
fit5.12b2 = glm(y ~ x, family = binomial)
summary(fit5.12b2) # we removed the complete separation as the groups overlap. beta-hat=0.3025 and SE=0.6879.p-value=0.66

#5.14
osteosarcoma.df = data.frame(
  Lymphocytic_Infiltration = c(rep("High", 4), rep("Low",4)),
  Sex = c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male"),
  Osteoblastic_Pathology = c("No", "Yes", "No", "Yes", "No", "Yes","No", "Yes"),
  yes = c(3,2,4,1,5,3,5,6),
  no = c(0,0,0,0,0,2,4,11)
)
osteosarcoma.df$n = osteosarcoma.df$yes + osteosarcoma.df$no
osteosarcoma.df

#part a
fit5.14a1 = glm(yes / n ~ Lymphocytic_Infiltration, family = binomial, 
                data=osteosarcoma.df, weights=n)
summary(fit5.14a1) #insignificant?
fit5.14a2 = glm(yes / n ~ Sex, family = binomial, 
                data=osteosarcoma.df,  weights=n)
summary(fit5.14a2) #significant
fit5.14a3 = glm(yes / n ~ Osteoblastic_Pathology, family = binomial, 
                data=osteosarcoma.df,  weights=n)
summary(fit5.14a3) #significant

#all predictos
fit5.14a4 = glm(yes / n ~ Lymphocytic_Infiltration + Sex +
                  Osteoblastic_Pathology, family = binomial, 
                data=osteosarcoma.df,  weights=n)
summary(fit5.14a4) #Lymphocytic_Infiltration has large SE, large p-value.
# There are some with 0 dinsease-free while some with not. This is quasi-complete separation.

#part b
#TBD

#5.15
drug.df = data.frame(
  center = c(1,1, 2,2,3,3,4,4,5,5),
  treatment = c("Active Drug", "Placebo", "Active Drug", "Placebo", "Active Drug", "Placebo", "Active Drug", "Placebo", "Active Drug", "Placebo"),
  success = c(0,0,1,0,0,0,6,2,5,2),
  failure = c(5,9,12,10,7,5,3,6,9,12)
)
drug.df$n = drug.df$failure + drug.df$success
drug.df$treatment = relevel(factor(drug.df$treatment, levels = c("Active Drug", "Placebo")), ref = "Placebo")
#part a
fit4 = glm(success/n ~ treatment 
           + factor(center), weights=n, data=drug.df, family=binomial)
summary(fit4) # We see almost zero z-value, almost 1 p-value and large SE for the center effects.

confint(fit4)

#part b
fit5 = glm(success / n ~ factor(center)  + treatment - 1, weights=n, data=drug.df, family=binomial)
summary(fit5)
confint(fit5)

# part c
fit6 = glm(success / n ~ treatment + factor(center), weights=n, data=drug.df, family = binomial)
summary(fit6)

drug.df.new = data.frame(
  center = c(2,2,4,4,5,5),
  treatment = c("Active Drug", "Placebo", "Active Drug", "Placebo", "Active Drug", "Placebo"),
  success = c(1,0,6,2,5,2),
  failure = c(12,10,3,6,9,12)
)
drug.df.new$n = drug.df.new$failure + drug.df.new$success
drug.df.new$treatment = relevel(factor(drug.df.new$treatment, levels = c("Active Drug", "Placebo")), ref = "Placebo")
fit7 = glm(success / n ~ treatment + factor(center), weights=n, data=drug.df.new, family = binomial)
summary(fit7)