#STAT 659
#Homework5
#3.11
imperfections = c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4, 9, 9, 8, 14, 8, 13, 11, 5, 7, 6)
x = c(rep(0,10), rep(1,10))
fit1 = glm(imperfections ~ x, family = poisson(link=log))
summary(fit1)
library(car)
Anova(fit1)
exp(confint(fit1)) #must exponentiate
#mu_a
predict(fit1, type = "response", newdata = data.frame(x=0))

#3.13
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
fit2 = glm(sat ~ weight, family = poisson(link=log), data=crab)
summary(fit2)
#Wald CI
wald = c(0.58930 - 1.96 * 0.06502, 0.58930 + 1.96 * 0.06502)
confint(fit2)
exp(c(0.4597002,  0.71449835))
library(car)
Anova(fit2)

#7.15
#Part a
cancer = read.table("https://users.stat.ufl.edu/~aa/cat/data/Cancer.dat", header = TRUE)
cancer
logrisktime = log(cancer$risktime)
fit3 = glm(count ~ factor(histology) + factor(stage) + factor(time) + factor(stage) * factor(time),
           data=cancer, family=poisson, offset=logrisktime)
fit = glm(count ~ factor(histology) + factor(stage) + factor(time),
           data=cancer, family=poisson, offset=logrisktime)
summary(fit3)
summary(fit)
anova(fit, fit3, test = "Chisq")

#part b
fit4 = glm(count ~  factor(stage) + factor(time),
          data=cancer, family=poisson, offset=logrisktime)
summary(fit4)
anova(fit4, fit, test = "Chisq")

#7.16
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
fit5 = glm(sat ~ weight, family=poisson, data=crab)
summary(fit5)
library(MASS)
fit6 = glm.nb(sat ~ weight, data=crab)
summary(fit6)
confint(fit5)
confint(fit6)

#Additional Problems
#problem 1
#mydata=read.csv("agresti_crab.csv")
mydata=data.frame(crab)
mydata$color=as.factor(mydata$color)
mydata$spine=as.factor(mydata$spine)
head(mydata)
regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),
                      c(TRUE,FALSE), c(TRUE,FALSE))
#regMat <- regMat[-(dim(regMat)[1]),]
names(regMat) <- c("color", "spine", "width", "weight")
regressors <- c("color", "spine", "width", "weight")
allModelsList <- apply(regMat, 1, function(x) as.formula(
  paste(c("sat ~ 1", regressors[x]),
        collapse=" + ")) )
allModelsResults <- lapply(allModelsList,
                           function(x) glm(x, family=poisson,
                                           data=mydata))
ourAIC <- unlist(lapply(allModelsResults, function(x)
  AIC(x)))
ourBIC <- unlist(lapply(allModelsResults, function(x)
  BIC(x)))
ourResid.Dev <- unlist(lapply(allModelsResults, function(x)
  x$deviance))
ourResid.Dev.df <- unlist(lapply(allModelsResults, function(x)
  x$df.residual))
ourloglik <- unlist(lapply(allModelsResults, function(x)
  as.numeric(logLik(x)) ))
results <- data.frame( model = as.character(allModelsList),
                       loglik=ourloglik,
                       aic=ourAIC,
                       bic=ourBIC,
                       Residual.Deviance=ourResid.Dev.df,
                       Residual.df=ourResid.Dev.df
)
print(results)

min(results$aic)
min(results$bic)

#problem 2
crab = read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = TRUE)
crab
fit5 = glm(sat ~ weight, family=poisson, data=crab)
AIC(fit5)
fit6 = glm.nb(sat ~ weight, data=crab)
AIC(fit6)
library(pscl)
fit7 = zeroinfl(sat ~ weight | weight, data=crab)
AIC(fit7)
fit8 = zeroinfl(sat ~ weight | 1, data=crab)
AIC(fit8)
fit9 = zeroinfl(sat ~ weight | weight, data=crab, dist = "negbin")
AIC(fit9)
fit10 = zeroinfl(sat ~ weight | 1, data=crab, dist = "negbin")
AIC(fit10)

#problem 3
soccer = data.frame(attendance = c(404, 286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148),
                    arrests =    c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19))
soccer$team = c("Aston Villa",  "Bradford City",  "Leeds United", "Bournemouth",  
                "West Brom", 
                "Hudderfield",  
                "Middlesbro", 
                "Birmingham", 
                "Ipswich Town",  
                "Leicester City", 
                "Blackburn",   
                "Crystal Palace", "Shrewsbury", "Swindon Town", "Sheffield Utd", "Stoke City",
                "Barnsley", "Millwall", "Hull City", "Manchester City", "Plymouth", "Reading",
                "Oldham")
plot(soccer$attendance, soccer$arrests, main = "attendance v/s arrests", xlab = "attendance", ylab = "arrests")
fit11 = glm(arrests ~ offset(log(attendance)), family = poisson, data=soccer)
summary(fit11)
i <- order(soccer$attendance)
lines(soccer$attendance[i],fit11$fitted[i],type="l")
text(soccer$attendance,soccer$arrests,soccer$team)
soccer$res = rstandard(fit11,type="pearson")
soccer
fit12 = glm.nb(arrests ~ offset(log(attendance)), data=soccer)
summary(fit12)
#Wald CI
1 / (fit12$theta + qnorm(c(0.025, 0.975)) * fit12$SE.theta)

#problem 4
data = data.frame(Age = c("35-44", "45-54", "55-64", "65-74", "75-84"),
                  PersonYrsNS = c(18793,10673,5710,2585, 1462),
                  PersonYrsS = c(52407,43248, 28612, 12663,5317),
                  CoronaryDeathsNS = c(2,12,28,28,31),
                  CoronaryDeathsS = c(32,104,206,186,102))
data
data$DeathRateNS = data$CoronaryDeathsNS/data$PersonYrsNS * 1000
data$DeathRateS = data$CoronaryDeathsS/data$PersonYrsS * 1000
data$relativerisk = data$DeathRateS/data$DeathRateNS
data

#restructure the data
data1 = data.frame(Age = c("35-44", "35-44", "45-54", "45-54", "55-64", "55-64", "65-74", "65-74", "75-84", "75-84"),
                   yrs = c(18793, 52407, 10673, 43248, 5710, 28612
                             ,2585, 12663, 1462, 5317),
                   deaths = c(2, 32, 12, 104, 28, 206, 28, 186, 31, 102),
                 isSmoker = as.factor(c(0,1,0,1,0,1,0,1,0,1)),
                 age_scores = c(1,1,2,2,3,3,4,4,5,5))
data1$x1 = ifelse(data1$Age=="35-44",1,0)
data1$x2 = ifelse(data1$Age=="45-54",1,0)
data1$x3 = ifelse(data1$Age=="55-64",1,0)
data1$x4 = ifelse(data1$Age=="65-74",1,0)
#first model
fit13 = glm(deaths ~ x1+x2+x3+x4+isSmoker+offset(log(yrs)), family = poisson(link=log), 
            data=data1)
summary(fit13)
fit14 = glm(deaths ~ x1+x2+x3+x4+isSmoker + isSmoker*age_scores+offset(log(yrs)), 
            family = poisson(link=log), data=data1)
summary(fit14)
anova(fit13, fit14, test = "Chisq")
