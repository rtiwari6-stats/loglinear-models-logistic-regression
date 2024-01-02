# STAT 659
# Homework1

library(binom)

# 1.3
# number of correct answers - can go up till 100
dbinom(0, 100, 0.25)
dbinom(1, 100, 0.25)
dbinom(2, 100, 0.25)
dbinom(3, 100, 0.25)
dbinom(4, 100, 0.25)

# 1.4
# part a
dbinom(0, 2, 0.5)
dbinom(1, 2, 0.5)
dbinom(2, 2, 0.5)

curve(2 * x * (1 - x), from = 0.0, to = 1.0, xlab = expression(pi), ylab = "likelihood")

# 1.8a
binom.confint(486, 1374, conf.level = 0.99, methods = "asymptotic")
prop.test(486, 1374, alternative = "two.sided", correct = FALSE)



# 1.9a
2*(1 - pnorm(2))
library(exactci)
prop.test(60,100, p=0.50, alternative="two.sided", correct=FALSE) #p-value 0.0455
prop.test(60,100, p=0.50, alternative="two.sided", correct=TRUE) #Too conservative! p-value 0.057
binom.test(c(60,40), p=0.5, alternative = "two.sided") # exact test that will give a different result! p-value=0.056
binom.exact(60,100,0.50, alternative = "two.sided", midp = TRUE) # exact test not too liberal, p-value=0.046

#problem with exact test?
2*(1-pbinom(59,100,0.5)) #note we are adding back a dbinom for 60 because we want P(X>=60 | Ho is true) 
#we want this to be 0.05

#when we have mid p-value, we get: 
2*(1-pbinom(59,100,0.5) - dbinom(60,100,0.5)/2) #this is  0.05, p-value 0.46 same as the binom.exact function

# 1.9b
binom.confint(60, 100, methods = "asymptotic")

# 1.9c
# Wilson
pihat <- 0.6
z_alpha_2 <- 1.96^2
n <- 100

(pihat + (z_alpha_2 / (2 * n) -
  1.96 * sqrt((pihat * (1 - pihat) / n) + (z_alpha_2 / (4 * n^2))))) / (1 + z_alpha_2 / n)
(pihat + (z_alpha_2 / (2 * n) +
  1.96 * sqrt((pihat * (1 - pihat) / n) + (z_alpha_2 / (4 * n^2))))) / (1 + z_alpha_2 / n)
binom.confint(60, n, methods = "ac")
binom.confint(60, n, methods = "wilson")
# 1.12d
binom.confint(0, 25, methods = "wilson")

# 1.12c
2 * (1 - pnorm(5))

# 1.14a
binom.test(8, 10, 0.5, alternative = "greater")
prop.test(x = 8, n = 10, p = 0.5, correct = TRUE, alternative = "greater") # should not use this because we need binomial test which is exact test. The prop.test is a normal approximation.
1 - pbinom(7, 10, 0.5) # we need to add probabilities for 8,9, 10.

binom.test(8, 10, 0.5, alternative = "less")
pbinom(8, 10, 0.5) # add probabilites from x=0 to 8.

# 1.14b
library(exactci)
exactci::binom.exact(8, 10, 0.5, alternative = "greater", midp = TRUE)
exactci::binom.exact(8, 10, 0.5, alternative = "less", midp = TRUE)

# 1.14d
library(PropCIs)
midPci(8, 10, 0.95)
exactci::binom.exact(8, 10, 0.5, alternative = "two.sided", midp = TRUE)

# additional problems
# problem1
binom.confint(8, 13, methods = "asymptotic")
binom.confint(8, 13, methods = "wilson")
binom.confint(8, 13, methods = "ac")

# problem2
# part a
A <- c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4)
B <- c(9, 9, 8, 14, 8, 13, 11, 5, 7, 6)
mean(A)
mean(B)
# wald
mean(A) - 1.96 * sqrt(mean(A) / length(A))
mean(A) + 1.96 * sqrt(mean(A) / length(A))
mean(B) - 1.96 * sqrt(mean(B) / length(B))
mean(B) + 1.96 * sqrt(mean(B) / length(B))

# score
5 + (1.96^2 / 20) - (1.96 / sqrt(10)) * sqrt(5 + (1.96^2 / 40))
5 + (1.96^2 / 20) + (1.96 / sqrt(10)) * sqrt(5 + (1.96^2 / 40))
9 + (1.96^2 / 20) - (1.96 / sqrt(10)) * sqrt(9 + (1.96^2 / 40))
9 + (1.96^2 / 20) + (1.96 / sqrt(10)) * sqrt(9 + (1.96^2 / 40))

library(DescTools)
PoissonCI(x=5*10, n=10, method = c("wald", "score"))
PoissonCI(x=9*10, n=10, method = c("wald", "score"))


# partb
mean(A)
var(A)
mean(B)
var(B)
(4.22 / 5 - 1) * sqrt(9 / 2)
(8.44 / 9 - 1) * sqrt(9 / 2)
2*(1-pnorm(abs(-0.1319933)))
2*(1-pnorm(abs(-0.330926)))

# rejection region
qnorm(1 - 0.05, 2)
