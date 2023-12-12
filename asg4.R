setwd('C:/Users/darre/Desktop/SEM 7/Survival/Material')

#A function for -log-log
minusloglog <- function(p){
  return(-log(-log(p)))}

library(survival)

# Load leukemia data
leu <- read.csv("leu3.csv")
head(leu)
dim(leu)
factor(leu$Treatment)

# Defining survival outcome
y <- Surv(leu$Time,leu$Censor)

#### Number 1 ####
# subset
treatment = subset(leu, Treatment == 1)
treatment = treatment[,-6]
summary(treatment$logWBC)
treatment$logWBC_cat <-cut(treatment$logWBC, breaks = c(0, 2.2, 3, 6), labels = c("Low_Trt", "Medium_Trt", "High_Trt"))

placebo = subset(leu, Treatment != 1)
placebo = placebo[,-6]
summary(placebo$logWBC)
placebo$logWBC_cat <- cut(placebo$logWBC, breaks = c(0, 2.4, 4, 6), labels = c("Low_Plc", "Medium_Plc", "High_Plc"))

df <- rbind(treatment,placebo)
kmfit <- survfit(y ~ df$logWBC_cat)
win.graph()
plot(kmfit, fun = minusloglog, xlab ="Time", ylab = "-log-log S", col = c("black", "red", "blue", "green", "purple","orange"))
legend("topright", c("Low_Trt", "Medium_Trt", "High_Trt", "Low_Plc", "Medium_Plc","High_Plc"), lty=c("solid"), 
       col = c("black", "red", "blue","green","purple","orange", cex = 0.25))


#### Number 2 ####
add <- read.csv('addicts.csv')
names(add) <- c("ID", "Clinic", "Status", "Time", "Prison", "Dose")
head(add)

y_add <- Surv(add$Time, add$Status)

# clinic
kmfit <- survfit(y_add ~ add$Clinic)
plot(kmfit, fun = minusloglog, xlab = 'Time', ylab = '-loh-log S', col = c('red', 'blue'))
legend('topright', c('1','2'), lty = c('solid'), col = c('red','blue'))

# prison
kmfit_prison <- survfit(y_add ~ add$Prison)
plot(kmfit_prison, fun = minusloglog, xlab = 'Time', ylab = '-loh-log S', col = c('red', 'blue'))
legend('topright', c('none','any'), lty = c('solid'), col = c('red','blue'))

# dose
summary(add$Dose)
add$Dose_cat <- cut(add$Dose, breaks = c(15, 50, 70, 150), labels = c('Low','Medium','High'))
head(add)
kmfit_dose <- survfit(y_add ~ add$Dose_cat)
plot(kmfit_dose, fun = minusloglog, xlab='Time', ylab = '-log-log S', col = c('black','red','blue'))
legend('topright', c('Low','Medium', 'High'), lty = c('solid'), col = c('black','red','blue'))

# gof
model <- coxph(y_add ~ Clinic + Prison + Dose, data = add)
checkph <- cox.zph(model, transform = rank)
checkph$table

model2 <- coxph(y_add ~ Clinic + Prison + Dose_cat, data = add)
checkph2 <- cox.zph(model, transform = rank)
checkph2$table

