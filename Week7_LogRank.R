###################################################################
#
# @filename : logrank.R
# @brief    : R code to do the log-rank tests
# @date     : 11/10/23
# @author   : Darren
#
###################################################################


#getwd()
## Navigate the working directory to your favorit path
setwd("C:/Users/darre/Desktop/SEM 7/Survival/Pre Midterm")

## Installing required packages
#install.packages(c("survival", "ggsurvfit", "tidycmprsk"))
library(survival)
library(ggsurvfit)
library(tidycmprsk)

## Comparing 2 groups
# Load leukemia data
leu <- read.csv("leu.csv")
head(leu)
dim(leu)
factor(leu$Treatment)

# define survival time
y <- Surv(leu$Time,leu$Censor)

# Log-rank test
lr1 <- survdiff(y ~ leu$Treatment)
lr1

# KM curves
# Plot survival curves for each treatment group
plot(survfit(y ~ leu$Treatment), lty = c("solid", "dashed"), xlab = "Time", ylab = "Survival Probabilities")
legend("topright", c("Placebo", "6-MP"), lty = c("solid", "dashed"))

# 95% confidence intervals
kmfit1 <- survfit(y ~ leu$Treatment)
summary(kmfit1) # Get the 95% CIs for curve, for each group
kmfit1 # Get the 95% CIs for median, for each group

# Plotting with 95% CI for survival curves
kmfit1 %>%
  ggsurvfit() +
  labs(
    x = "Weeks",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

## Comparing p groups (in this case, p = 3)
# Load veteran data
vet <- read.csv("veteran.csv")
head(vet)
dim(vet)
factor(vet$treatment)
factor(vet$karnofsky)

# Categorizing Karnofsky score
vet$karnofsky_group <- cut(vet$karnofsky,
                           breaks = c(0, 59, 74, 100),
                           labels = c("1", "2", "3"))
head(vet)

# Define survival time as survival data
y1 <- Surv(vet$tdays,vet$dead)

# KM curves
# Plot survival curves for each treatment group
plot(survfit(y1 ~ vet$karnofsky_group), lty = c("solid", "dashed", "dotted"), xlab = "Days", ylab = "Survival Probabilities")
legend("topright", c("Group 1", "Group 2", "Group 3"), lty = c("solid", "dashed", "dotted"))

# The log-rank test for 3 groups
lr2 <- survdiff(y1 ~ vet$karnofsky_group)
lr2

