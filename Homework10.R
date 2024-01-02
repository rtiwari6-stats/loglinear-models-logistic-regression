#Homework 10
#problem 7.1

1-pchisq(0.82,1)
exp(1.4165)
exp(-1.4165) 
exp (0.1368)

#part d
postlife = read.table("https://users.stat.ufl.edu/~aa/cat/data/Postlife.dat", header = TRUE)
postlife
fit7.1d = glm(count ~ race + postlife, data=postlife, family=poisson)
summary(fit7.1d)
exp (1.49846)

#problem 7.2
1-pchisq(0.37984,1)

#part c
DeathPenalty = read.table("https://users.stat.ufl.edu/~aa/cat/data/DeathPenalty.dat", header = TRUE)
DeathPenalty

DeathPenalty.df = data.frame(
  V =  c("white", "white", "black", "black"),
  D = c("white", "black", "white", "black"),
  Yes = c(53, 11, 0, 4),
  No = c(414, 37, 16, 139)
)
DeathPenalty.df$n = DeathPenalty.df$Yes + DeathPenalty.df$No

fit7.2d = glm(Yes/n ~ V + D, data=DeathPenalty.df, family=binomial, weights=n)
summary(fit7.2d)

#problem 7.3
data7.3.df = data.frame(
  Safety = c(rep("Seat Belt", 4), rep("None", 4)),
  Ejected = rep(c("Yes", "Yes", "No", "No"), 2),
  Injury = rep(c("Non Fatal", "Fatal"), 4),
  Count = c(1105, 14, 411111, 483, 4624, 497, 157342, 1008)
)
data7.3.df

#part a
fit7.3a = glm(Count ~ Safety + Ejected + Injury + Safety:Ejected + Safety:Injury + Ejected:Injury, data=data7.3.df, family=poisson)
summary(fit7.3a)

#estimated  conditional odds ratio of ejected when wearing seat belt to none 
exp(-2.39964 ) #0.09075062

#estimated conditional odds of non fatal injury when wearing seat belt to none
exp(1.71732) #5.569582

#estimated conditional odds of non fatal injury when ejected to not ejected
exp(-2.79779 ) #0.0609446

#part b dissimilarity index
sum(abs(data7.3.df$Count - fitted(fit7.3a)))/ (2 * sum(data7.3.df$Count)) #4.767967e-05

#From edition 2
#Treating whether killed as the response variable, fit an equivalent logistic
#model. Interpret the effects on the response.
data7.3.df.logit = data.frame(
  Safety = c(rep("Seat Belt", 2), rep("None", 2)),
  Ejected = c("Yes", "No", "Yes", "No"),
  Injury_fatal = c(14,483,497,1008),
  Injury_nonfatal = c(1105, 411111,4624,157342)
)
data7.3.df.logit
data7.3.df.logit$n = data7.3.df.logit$Injury_fatal+data7.3.df.logit$Injury_nonfatal

#we take non fatal to match our loglinear
fit7.3b = glm(Injury_nonfatal/n ~ Safety+Ejected, weights=n, data=data7.3.df.logit,
              family = binomial)
summary(fit7.3b)

#problem 7.4
mbti = read.table("https://users.stat.ufl.edu/~aa/cat/data/MBTI.dat", header = TRUE)
mbti

fit7.4 = glm(n ~ EI + SN + TF + JP + EI:SN + EI:TF + EI:JP +
                SN:TF + SN:JP  + TF:JP, family=poisson, data=mbti)
summary(fit7.4) #check coefficients and p-values.

#problem 7.5
Accidents2 = read.table("https://users.stat.ufl.edu/~aa/cat/data/Accidents2.dat", header = TRUE)
Accidents2
#part a
fit7.5a = glm(count ~ gender+location+seatbelt+injury+
                      gender:location + gender:seatbelt+
                location:seatbelt+gender:injury+
                location:injury+seatbelt:injury, family=poisson,
              data=Accidents2)
summary(fit7.5a)
#odds of injury in males
exp(-0.54053 ) #0.5824395

#odds of injury in urban location
exp(-0.75503) #0.4699965

#odds of injury with seat belt use
exp(-0.81400) #0.4430822

#part b (check solutions)

library(dplyr)
library(tidyr)

Accidents2_partb1 = Accidents2[, !(names(Accidents2) %in% "injury")]  %>% 
  group_by(gender, location, seatbelt) %>% 
  summarise(count_seatbelt=sum(count), .groups='drop') %>%
  pivot_wider(names_from=seatbelt, values_from=count_seatbelt)

fit7.5b1 = glm( yes/(no+yes) ~ 
                  gender + location, family=binomial, data=Accidents2_partb1,
                weights=(no+yes))
summary(fit7.5b1) #GS and LS

Accidents2_partb2 = Accidents2 %>% 
  pivot_wider(names_from=injury, values_from=count)
#pivot narrower and see
Accidents2_partb2 %>% pivot_longer(cols = c("no", "yes"), names_to = "injury", values_to = "count")

fit7.5b2 = glm( yes/(no+yes) ~ 
                 gender + location + seatbelt, family=binomial, data=Accidents2_partb2,
               weights=(no+yes))
summary(fit7.5b2) #GI, LI, SI

#problem 7.6
data7.6.df = data.frame(
  political = factor(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3)),
  premarital = factor(c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2)),
  religious = factor(c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2)),
  birth = factor(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)),
  count = c(99, 15, 73, 25, 8, 4, 24, 22, 73, 20, 87, 37, 20, 13, 50, 60, 51, 19, 51, 36, 6, 12, 33, 88)
)
data7.6.df

#partc
fit7.6partc = glm(count ~ political+premarital + birth+religious+
                     political:premarital+political:birth+ political:religious+
                     premarital:birth+premarital:religious+
                     birth:religious+ political:birth:religious, data=data7.6.df,
                   family=poisson)
summary(fit7.6partc)
1-pchisq(fit7.6partc$deviance, fit7.6partc$df.residual) #p-value=0.5648268

#smaller model
fit7.6partc1 = glm(count ~ political+premarital + birth+religious+
                    political:premarital+
                    premarital:birth+premarital:religious
                    , data=data7.6.df,
                  family=poisson)
summary(fit7.6partc1)

#problem 7.7
spending = read.table("https://users.stat.ufl.edu/~aa/cat/data/Spending.dat", header = TRUE)
spending

spending$e = as.factor(spending$e)
spending$h = as.factor(spending$h)
spending$c = as.factor(spending$c)
spending$l = as.factor(spending$l)

fit7.7.2factor = glm(count ~ e+h+c+l+e:h+e:c+e:l+h:c+h:l+c:l, family=poisson,
                     data=spending)
summary(fit7.7.2factor)

fit7.7.3factor = glm(count ~ e+h+c+l+e:h+e:c+e:l+h:c+h:l+c:l
                     +e:h:c+e:h:l+e:c:l+h:c:l
                     , family=poisson,
                     data=spending)
summary(fit7.7.3factor)
anova(fit7.7.2factor, fit7.7.3factor, test="Chisq") #0.8737
#so we can go with the simpler model
#compute odd ratios for 3-3 categories of each
#can drop EL and CH based on small odd ratios
fit7.7.2factor.simple = glm(count ~ e+h+c+l+e:h+e:c+h:l+c:l, family=poisson,
                     data=spending)
summary(fit7.7.2factor.simple)#39.411  on 56  degrees of freedom
  
#problem 7.13

data7.13.df = data.frame(
  Attendance = c(rep("Never", 4),
                      rep("Less Than Once a Year", 4),
                      rep("Once or Twice a Year", 4),
                      rep("Several Times a Year", 4),
                      rep("About Once a Month", 4),
                      rep("2-3 Times a Month", 4),
                      rep("Nearly Every Week", 4),
                      rep("Every Week", 4),
                      rep("Several Times a Week", 4)),
  BirthControl = rep(c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree")),
  Count = c(49, 49, 19, 9, 31, 27, 11, 11, 46, 55, 25, 8, 34, 37, 19, 7,
            21, 22, 14, 16, 26, 36, 16, 16, 8, 16, 15, 11, 32, 65, 57, 61,
            4, 17, 16, 20)
)
data7.13.df #36 cell counts



#part a
fit7.13a = glm(Count ~ Attendance + BirthControl, family = poisson,
               data = data7.13.df)
summary(fit7.13a)

1-pchisq(fit7.13a$deviance, fit7.13a$df.residual)

#part b
data7.13.df$Rscore = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4),rep(5,4),rep(6,4),rep(7,4)
           , rep(8,4), rep(9,4))
data7.13.df$Cscore = c(rep(c(1,2,3,4), nrow(data7.13.df)/4))
fit7.13b = glm(Count ~ Attendance + BirthControl + Rscore:Cscore, 
          family = poisson, data = data7.13.df)
summary(fit7.13b)
1-pchisq(fit7.13b$deviance, fit7.13b$df.residual)
anova(fit7.13a, fit7.13b, test = "Chisq")

#partc 
data7.13.df$Cscore_c = c(rep(c(1,2,4,5), nrow(data7.13.df)/4))
fit7.13c = glm(Count ~ Attendance + BirthControl + Rscore:Cscore_c, 
               family = poisson, data = data7.13.df)
summary(fit7.13c)
1-pchisq(fit7.13c$deviance, fit7.13c$df.residual)

#part d
#saturated
fit7.13d.sat = glm(Count ~ Attendance + BirthControl + Attendance:BirthControl, family = poisson,
               data = data7.13.df)
summary(fit7.13d.sat)
1-pchisq(fit7.13d.sat$deviance, fit7.13d.sat$df.residual)

#row
fit7.13d.row = glm(Count ~ Attendance + BirthControl + Attendance:Cscore, family = poisson,
                   data = data7.13.df)
summary(fit7.13d.row)
1-pchisq(fit7.13d.row$deviance, fit7.13d.row$df.residual)

#col
fit7.13d.col = glm(Count ~ Attendance + BirthControl + Rscore:BirthControl, family = poisson,
                   data = data7.13.df)
summary(fit7.13d.col)
1-pchisq(fit7.13d.col$deviance, fit7.13d.col$df.residual)

#both
fit7.13d.both = glm(Count ~ Attendance + BirthControl + Rscore:BirthControl 
                    + Attendance:Cscore, family = poisson,
                   data = data7.13.df)
summary(fit7.13d.both)
1-pchisq(fit7.13d.both$deviance, fit7.13d.both$df.residual)
