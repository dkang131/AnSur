###################################################################
#
# @filename : coxph.R
# @brief    : R code for topic 5: the Cox PH models
# @date     : 20230402
# 
###################################################################
getwd()
setwd("C:/Users/darre/Desktop/SEM 7/Survival/Material")

## Installing required packages
#install.packages(c("survival"))
library(survival)

# Load leukemia data
leu <- read.csv("leu2.csv", fileEncoding = 'UTF-8-BOM')
head(leu)
dim(leu)
factor(leu$Treatment)
str(leu)

# Defining survival outcome
y1 <- Surv(leu$Time,leu$Censor)

# Model 1: h(t|X) = h_0(t) x exp(b1 x Treatment)
model11 <- coxph(y1 ~ leu$Treatment)
summary(model11)

# Model 2: h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC)
model21 <- coxph(y1 ~ leu$Treatment + leu$logWBC)
summary(model21)

# Model 3: h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC + b3 x Treatment*logWBC)
model31 <- coxph(y1 ~ leu$Treatment + leu$logWBC + leu$Treatment*leu$logWBC)
summary(model31)

lrt <- -2*model21$loglik[2] - (-2*model31$loglik[2])
pval <- pchisq(lrt,1, lower.tail = F);pval
chi_crit <- qchisq(0.05,1,lower.tail = F)

if(lrt > chi_crit){
  print('Reject H0')
}else{
  print('Fail to Reject H0')
}

lrt_confounding <- -2*model11$loglik[2] - (-2*model21$loglik[2])
pval <- pchisq(lrt_confounding,1, lower.tail = F);pval
if(lrt_confounding > chi_crit){
  print('Reject H0')
}else{
  print('Fail to Reject H0')
}

HR <- exp(-1.3861*1 + 1.6909*3)/exp(-1.3861*0 + 1.6909*2.6);HR
var_cov_2 <- model21$var

var_HR <- var_cov_2[1,1] + var_cov_2[2,2] + (0.4^2)*var_cov_2[1,2]

CI <- c(HR*exp(1.96*sqrt(var_HR)),HR*exp(-1.96*sqrt(var_HR)))
CI
