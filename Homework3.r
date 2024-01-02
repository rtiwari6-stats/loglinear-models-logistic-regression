#STAT 659 homework3

#2.16
qchisq(0.95, 4)
1-pchisq(73.4, 4)

#linear trend test
data = matrix(c(21,159,110,53,372,221,94,242,83), nrow = 3, ncol = 3, byrow = TRUE)
rownames(data) = c("Above Average", "Average", "Below Average")
colnames(data) = c("Not Too Happy", "Pretty Happy", "Very Happy")
data
#linear trend test
library(vcdExtra)
rscores = c(3,2,1)
cscores = c(1,2,3)
CMHtest(data, rscores = rscores, cscores = cscores)
source('~/tamu/MS-STAT-2022/659/2023/HW3/linear_trend_test.R')
linear.trend(data, nrow(data), ncol(data), rscores, cscores)
#mosaic plot
mosaicplot(data, main="", col=c("maroon","skyblue", "gray"), xlab = "Income", ylab = "Happiness")

#2.17a
#chi-square test of independence
political = matrix(c(871,821,336,347,42,83), ncol = 3, byrow = TRUE)
rownames(political) = c("White", "Black")
colnames(political) = c("Dem", "Rep", "Ind")
political
chisq.test(political)
#note we need standard residuals
chisq.test(political)$stdres

#2.18
data = matrix(c(60,40,75,25), ncol=2, byrow = TRUE)
rownames(data) = c("Men", "Women")
colnames(data) = c("A_Yes", "A_No")
data
chisq.test(data)

#2.19
1-pchisq(69.2,4)

#2.19b
education = matrix(c(178,138,108,570,648,442,138,252,252), ncol = 3, byrow = TRUE)
rownames(education) = c("Less than high school", "high school or junior college", "bachelor or graduate")
colnames(education) = c("fundamentalist", "moderate", "liberal")
education
library(vcdExtra)
rscores = c(1,2,3)
cscores = c(1,2,3)
CMHtest(education, rscores = rscores, cscores = cscores)
mosaicplot(education, main="", col=c("maroon","skyblue", "gray"))
#professor's / agresti's code
source('~/tamu/MS-STAT-2022/659/2023/HW3/linear_trend_test.R')
linear.trend(education,3,3, rscores, cscores)

#2.21a
data = matrix(c(2,4,13,3,2,6,22,4,0,1,15,8,0,3,13,8), ncol = 4, byrow = TRUE)
rownames(data) = c("<5", "5-15", "15-25", ">25")
colnames(data) = c("very dissatisfied", "a little satisfied", "moderately satisfied", "very satisfied")
data
chisq.test(data)
chisq.test(data)$stdres

#2.21b
rscores = c(3, 10, 20, 35)
cscores = c(1, 3, 4, 5)
CMHtest(data, rscores = rscores, cscores = cscores)
linear.trend(t(data), nrow(data), ncol(data), rscores, cscores)

#2.22
data = matrix(c(7,8,0,15), ncol = 2, byrow = TRUE)
data
fisher.test(data, alternative = "greater") # greater because we have been asked to test for this case

#2.23b
library(epitools)
#mid p-values for testing independence (gives both one-sided and two-sided)
ormidp.test(21,2,15,3, or=1)
#the CI for mid p-value
#row wise
or.midp(c(21,2,15,3), conf.level = 0.95)$conf.int
epitools::oddsratio(c(21,2,15,3), conf.level = 0.95, method = "midp")$measure
epitools::oddsratio(c(21,2,15,3), conf.level = 0.95, method = "fisher")$measure