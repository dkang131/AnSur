setwd('C:/Users/darre/Desktop/SEM 7/Survival/Material')
library(survival)

data <- read.csv('data_midterm.csv', header =T, sep = ";")
data
factor(data$Treatment)
y <- Surv(data$Time,data$Censor)
plot(survfit(y ~ data$Treatment),
     xlab = "Time",
     ylab = "Survival Probabilities")

plot(survfit(y ~ data$Treatment), 
     lty = c("solid", "dashed"), 
     xlab = "Time", 
     ylab = "Survival Probabilities")

legend("bottomleft", c("After", "Before"), lty = c("solid", "dashed"))
