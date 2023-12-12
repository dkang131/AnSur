setwd("C:/Users/darre/Desktop/SEM 7/Survival/Pre Midterm")
library(survival)

data <- read.csv('leu.csv', header = T, sep = ',')
head(data)
factor(data$Treatment)

#define survival time
y <- Surv(data$Time, data$Censor)

leuexp <- survreg(y ~ Treatment, data = data, dist = "exponential")
summary(leuexp)
