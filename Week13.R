setwd('C:/Users/darre/Desktop/SEM 7/Survival/Material')

library(survival)

leu <- read.csv('leu3.csv')
head(leu)

y <- Surv(leu$Time, leu$Censor)
# stratified -  no-interaction
model5 <- coxph(y ~ Treatment + logWBC + strata(Sex), data = leu)
summary(model5)

model6 <- coxph(y ~ logWBC + strata(Sex), data = leu)
summary(model6)

pchisq(-2*model6$loglik[2]-(-2*model5$loglik[2]),1,lower.tail = F)

# No-strat
model5.female <- coxph(y ~ Treatment + logWBC, subset=(Sex == 1), data = leu)
summary(model5.female)
model5.male <- coxph(y ~ Treatment + logWBC, subset=(Sex == 0), data = leu)
summary(model5.male)

# log-likelihood
LL.strat <- model5$loglik[2]
LL.nostrat <- model5.female$loglik[2] + model5.male$loglik[2]

# check PH assumption
chisq_stat <- -2*(LL.strat - LL.nostrat)
ngroups <- length(unique(leu$Sex))
ncoef <- length(model5$coefficients)
pval <- 1 - pchisq(chisq_stat, df = (ngroups-1)*ncoef)
pval


model7 <- coxph(y ~  Treatment + logWBC + strata(Sex)*Treatment + 
                  strata(Sex)*logWBC, data = leu) # p = 4
summary(model7)

LL.noint <- model5$loglik[2]
LL.int <- model7$loglik[2]
chisq_stat <- -2*(LL.noint - LL.int)
ncoef.dif <- length(model7$coefficients) - length(model5$coefficients) # 2
pvalue <- 1 - pchisq(chisq_stat, df = ncoef.dif)
pvalue

### veteran data ####

minusloglog <- function(p){
  return(-log(-log(p)))
}

vet <- read.csv('veteran.csv')
head(vet)
str(vet)

y_vet <- Surv(vet$tdays, vet$dead)
win.graph()
plot(survfit(y_vet ~ vet$treatment), fun = minusloglog, lty = c('solid','dashed'), xlab = 'time', ylab = '-log-log S')
legend('topright', c('standard','test'), lty = c('solid','dashed'))

plot(survfit(y_vet ~ vet$celltype), fun = minusloglog, col = c('blue','red','pink','purple'), xlab = 'time', ylab = '-log-log S')
legend('topright', c('1','2','3','4'), lty = c('solid'), col = c('blue','red','pink','purple'))

summary(vet$karnofsky)
vet$karnofsky_cat <- cut(vet$karnofsky, breaks = c(0,40,75,100), labels = c('low_kar','med_kar', 'high_kar'))
plot(survfit(y_vet ~ vet$karnofsky_cat), fun = minusloglog, col = c('blue','red','purple'), xlab = 'time', ylab = '-log-log S')
legend('topright', c('low_kar','med_kar','high_kar'), lty = c('solid'), col = c('blue','red','purple'))

summary(vet$tprior)
vet$tprior_cat <- cut(vet$tprior, breaks=c(0,20,40,100), labels = c('low_the','med_the','high_the'))
summary(vet$age)
vet$age_cat <- cut(vet$age, breaks = c(30,51,66,100), labels = c('low_age','med_age','high_age'))

model1 <- coxph(y_vet ~ treatment + celltype + therapy + karnofsky + tprior + age, data = vet)
summary(model1)
check_ph <- cox.zph(model1, transform = rank)
check_ph$table
# karfnosky and cell type reject h0

model2 <- coxph(y_vet ~ treatment + therapy + karnofsky + tprior + age, data = vet)
summary(model2)
