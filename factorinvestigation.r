#factor test

set.seed(50)
#Y is binomial response
Y =  rbinom(10, 2, 0.3)
#X is continuous
X = rnorm(10)
#Z is categorical
Z = rpois(10, 1)

#Note we don't code Z as a factor
model1 = glm(Y ~ X + Z, family=binomial)
summary(model1)
#Result of model1:
# Call:
#   glm(formula = Y ~ X + Z, family = binomial)
#
# Deviance Residuals:
#   Min        1Q    Median        3Q       Max
# -0.91352  -0.80238  -0.30438   0.00011   1.84514
#
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)   20.1679  5920.9644   0.003    0.997
# X              0.9807     1.6194   0.606    0.545
# Z            -21.1799  5920.9646  -0.004    0.997
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 13.8629  on 9  degrees of freedom
# Residual deviance:  7.9915  on 7  degrees of freedom
# AIC: 13.992
#
# Number of Fisher Scoring iterations: 18

#Now let's code Z  as a factor
model2 = glm(Y ~ X + as.factor(Z), family=binomial)
summary(model2)

#Result of model2:
# Call:
#   glm(formula = Y ~ X + as.factor(Z), family = binomial)
#
# Deviance Residuals:
#   Min        1Q    Median        3Q       Max
# -0.91352  -0.80238  -0.30438   0.00011   1.84514
#
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)     20.1679  5920.9644   0.003    0.997
# X                0.9807     1.6194   0.606    0.545
# as.factor(Z)1  -21.1799  5920.9646  -0.004    0.997
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 13.8629  on 9  degrees of freedom
# Residual deviance:  7.9915  on 7  degrees of freedom
# AIC: 13.992
#
# Number of Fisher Scoring iterations: 18

#both identical

#Now let's increase the levels of Z
Z = sample(20, 10)

#No factor
model3 = glm(Y ~ X + Z, family=binomial)
summary(model3)
#
# Call:
#   glm(formula = Y ~ X + Z, family = binomial)
#
# Deviance Residuals:
#   Min        1Q    Median        3Q       Max
# -1.62207  -1.05521   0.05933   0.99201   1.53465
#
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -1.4538     1.6168  -0.899    0.369
# X            -0.9586     1.2749  -0.752    0.452
# Z             0.1891     0.1909   0.990    0.322
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 13.863  on 9  degrees of freedom
# Residual deviance: 12.724  on 7  degrees of freedom
# AIC: 18.724
#
# Number of Fisher Scoring iterations: 4


#Using factor
model3 = glm(Y ~ X + as.factor(Z), family=binomial)
summary(model3)

# Call:
#   glm(formula = Y ~ X + as.factor(Z), family = binomial)
#
# Deviance Residuals:
#   [1]  0  0  0  0  0  0  0  0  0  0
#
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -8.927  98565.325       0        1
# X                  34.331 129462.010       0        1
# as.factor(Z)2      43.032 174914.077       0        1
# as.factor(Z)3     -28.027 160984.512       0        1
# as.factor(Z)5      83.127 272970.588       0        1
# as.factor(Z)6      50.612 188128.801       0        1
# as.factor(Z)8     -22.359 160670.412       0        1
# as.factor(Z)9     -25.774 160519.314       0        1
# as.factor(Z)10      4.647 194632.543       0        1
# as.factor(Z)13     14.448 164929.028       0        1
# as.factor(Z)15         NA         NA      NA       NA
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 1.3863e+01  on 9  degrees of freedom
# Residual deviance: 4.2867e-10  on 0  degrees of freedom
# AIC: 20
#
# Number of Fisher Scoring iterations: 23
