#STAT 659
#Homework2

#2.3a
a = 62.4*10^-6 * (1-62.4*10^-6)/10^6
b = 1.3*10^-6 * (1-1.3*10^-6)/10^6
se = sqrt(a+b)

#wald CI
(62.4-1.3)*10^-6 + 1.96 * se
(62.4-1.3)*10^-6 - 1.96 * se

#can also use
prop.test(c(62.4, 1.3), c(1000000, 1000000), correct = FALSE)

#2.6
a = c(179793, 65502, 24624, 17401, 5266, 10)
sum(a)
b = a/sum(a)
b

(0.81*(1-0.22))/(0.22*(1-0.81))

#2.8
(0.847*(1-0.906))/(0.906*(1-0.847))

#2.10
se = sqrt(1/433+1/8049+1/570+1/554883)
log(52.36879)
3.958311 - 1.96*0.06472941
3.958311 + 1.96*0.06472941

exp(3.958311 - 1.96*0.06472941)
exp(3.958311 + 1.96*0.06472941)

library(epitools)
out=array(c(433, 570,8049,554883), c(2,2),
          dimnames=list(RestraintsUse=c('N', 'Y'),
                        Injury=c('Fatal', 'NonFatal')))
out
out2 = oddsratio(out, method = "wald")
out2$measure
#2.11
se = sqrt(1/802+1/53+1/34+1/494)
se
5.392992 -1.96*0.2270482
5.392992 +1.96*0.2270482

exp(5.392992 -1.96*0.2270482)
exp(5.392992 +1.96*0.2270482)
library(epitools)
out=array(c(802, 34,53,494), c(2,2),
          dimnames=list(Vote2008=c('Obama', 'McCain'),
                        Vote2012=c('Obama', 'Romney')))
out
out2 = oddsratio(out, method = "wald")
out2$measure

#additional
#problem 1
a = c(15,18,8,22,10,19,14,14,18,12)
b = (a-15)^2/15
sum(b)

qchisq(0.95, 9)

#trying with chisq.test
obs.freq = c(15,18,8,22,10,19,14,14,18,12)
prob.vec = rep(1/10,10)
chisq.test(x=obs.freq, p=prob.vec) #this works

#problem 2
obs.freq = c(773,221,238,69)
prob.vec = c(9/16, 3/16, 3/16, 1/16)
chisq.test(x=obs.freq, p=prob.vec)
qchisq(0.95, 3)

#problem 3

f = expression(1997 * log(0.25*(2+x)) + 906*log(0.25*(1-x)) + 904*log(0.25*(1-x)) + 32*log(0.25*x))
D(f,"x") #derivative
#write derivative as a function
d = function(x) {1997 * (0.25/(0.25 * (2 + x))) - 906 * (0.25/(0.25 * (1 - x))) - 
    904 * (0.25/(0.25 * (1 - x))) + 32 * (0.25/(0.25 * x))}
uniroot(d, c(0.0,1.0)) #find root

#with chisq, but df will be wrong but the x-squared statistic is right
obs.freq = c(1997, 906, 904, 32)
prob.vec  = c(0.25*(2+0.03570979), 0.25*(1-0.03570979), 0.25*(1-0.03570979), 0.25*0.03570979)
chisq.test(x=obs.freq, p=prob.vec)

#problem 4
log(0.0171/0.0094) - 1.96 * sqrt((1-0.0171)/(11034*0.0171) + (1-0.0094)/(11037*0.0094))
log(0.0171/0.0094) + 1.96 * sqrt((1-0.0171)/(11034*0.0171) + (1-0.0094)/(11037*0.0094))

exp(log(0.0171/0.0094) - 1.96 * sqrt((1-0.0171)/(11034*0.0171) + (1-0.0094)/(11037*0.0094)))
exp(log(0.0171/0.0094) + 1.96 * sqrt((1-0.0171)/(11034*0.0171) + (1-0.0094)/(11037*0.0094)))
#CI for odds ratio
library(PropCIs)
riskscoreci(189,11034,104,11037, conf.level = 0.95)

#score intervals
library(binom)
binom.confint(189, 11034, method = "wilson")
binom.confint(104, 11037, method = "wilson")

(0.0171-0.0094)- 1.96 *sqrt((0.01487052*(1-0.01487052))/11034+(0.01140372*(1-0.01140372))/11037)
(0.0171-0.0094) + 1.96 *sqrt((0.01972333*(1-0.01972333))/11034+(0.007783358*(1-0.007783358))/11037)

#AC
(0.0172163827-0.0095117311)  - 1.96 *sqrt((0.0172163827*(1-0.0172163827))/11036+(0.0095117311*(1-0.0095117311))/11039)
(0.0172163827-0.0095117311)  + 1.96 *sqrt((0.0172163827*(1-0.0172163827))/11036+(0.0095117311*(1-0.0095117311))/11039)

library(DescTools)
#agresti-caffo
BinomDiffCI(189, 11034, 104, 11037, conf.level = 0.95, sides = "two.sided",
            method ="ac")

library(PropCIs)
#ac ci
diffscoreci(189, 11034, 104, 11037, conf.level = 0.95)

#Newcombe
BinomDiffCI(189, 11034, 104, 11037, conf.level = 0.95, sides = "two.sided",
            method ="scorecc")
