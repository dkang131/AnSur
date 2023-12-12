setwd('C:/Users/darre/Desktop/SEM 7/Survival/Pre Midterm')
library(survival)

data <- read.csv('leu.csv', header = T, sep = ',')
head(data)
factor(data$Treatment)

#define survival time
y <- Surv(data$Time, data$Censor)

# KM curves
# Plot survival curves for each treatment group
plot(survfit(y ~ data$Treatment),
     xlab = "Time",
     ylab = "Survival Probabilities")

plot(survfit(y ~ data$Treatment), 
     lty = c("solid", "dashed"), 
     xlab = "Time", 
     ylab = "Survival Probabilities")

legend("topright", c("Placebo", "6-MP"), lty = c("solid", "dashed"))

# Log-rank test
LR <- survdiff(y ~ data$Treatment)

??survdiff

#decision
if (LR$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}
