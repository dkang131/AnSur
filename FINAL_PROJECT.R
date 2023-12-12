library(KMsurv)
library(dplyr)
library(survival)
library(tidyverse)

data('pneumon')
str(pneumon)
sapply(pneumon, function(x) sum(is.na(x)))
# statdes
pneumon %>% summarise(across(where(is.numeric), .fns = 
                                 list(min = min,
                                      median = median,
                                      mean = mean,
                                      stdev = sd,
                                      q25 = ~quantile(., 0.25),
                                      q75 = ~quantile(., 0.75),
                                      max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('variable', '.value'))
#### data cleaning & preprocessing ####
pneumon$mthage <- as.numeric(pneumon$mthage)
pneumon$urban <- as.factor(pneumon$urban)
pneumon$alcohol <- as.factor(pneumon$alcohol)
pneumon$smoke <- as.factor(pneumon$smoke)
pneumon$region <- as.factor(pneumon$region)
pneumon$poverty <- as.factor(pneumon$poverty)
pneumon$bweight <- as.factor(pneumon$bweight)
pneumon$race <- as.factor(pneumon$race)
pneumon$education <- as.numeric(pneumon$education)
pneumon$nsibs <- as.numeric(pneumon$nsibs)
pneumon$wmonth <- as.numeric(pneumon$wmonth)
pneumon$sfmonth <- as.numeric(pneumon$sfmonth)


str(pneumon)

# table(pneumon$smoke)
# table(pneumon$alcohol)
# win.graph()
# par(mfrow = c(2,1))
# hist(table(pneumon$smoke))
# hist(table(pneumon$alcohol))

pneumon$smoke <- factor(pneumon$smoke, levels = c(0,1,2), labels = c(0,1,1))
str(pneumon$smoke)
table(pneumon$smoke)

pneumon$alcohol <- factor(pneumon$alcohol, levels = c(0,1,2,3,4), labels = c(0,1,1,1,1))
str(pneumon$alcohol)
table(pneumon$alcohol)

pneumon$education_cat <- cut(pneumon$education, breaks = c(0,10,12,20), labels =c("Low_Edu", "Med_Edu", "High_Edu"))
pneumon$wmonth_cat <- cut(pneumon$wmonth, breaks = c(-1,0,30), labels = c("zero_month", "morethan_zero"))
pneumon$sfmonth_cat <- cut(pneumon$sfmonth, breaks = c(-1,0,30), labels = c("zero_month", "morethan_zero"))
remove_cols <- c('education','wmonth','sfmonth')
pneumon = subset(pneumon, select = !(names(pneumon) %in% remove_cols))

#### Define Y survival time ####
y <- Surv(pneumon$agepn, pneumon$hospital)

#### KM Curve ####
# child age
kmfit1 <- survfit(y ~ chldage, data = pneumon)
plot(kmfit1, col = c(unique(pneumon$chldage)),xlab="survival time in months", ylab="survival probabilities")
legend("bottomleft", legend = c(unique(pneumon$chldage)), col=c(unique(pneumon$chldage)), pch = 16, 
       cex = 0.5, xjust = 1)

# mthage
kmfit2 <- survfit(y ~ mthage, data = pneumon)
plot(kmfit2, col = c(unique(pneumon$mthage)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve mthage")
legend("bottomleft", legend = c(unique(pneumon$mthage)), col=c(unique(pneumon$mthage)), pch = 16, 
       cex = 0.5, xjust = 1)

# urban
kmfit3 <- survfit(y ~ urban, data = pneumon)
plot(kmfit3, col = c(unique(pneumon$urban)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Urban")
legend("bottomleft", legend = c(unique(pneumon$urban)), col=c(unique(pneumon$urban)), pch = 16, 
       cex = 0.5, xjust = 1)

# alchohol
kmfit4 <- survfit(y ~ alcohol, data = pneumon)
plot(kmfit4, col = c(unique(pneumon$alcohol)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Alcohol")
legend("bottomleft", legend = c(unique(pneumon$alcohol)), col=c(unique(pneumon$alcohol)), pch = 16, 
       cex = 0.5, xjust = 1)

# smoke
kmfit5 <- survfit(y ~ smoke, data = pneumon)
plot(kmfit5, col = c(unique(pneumon$smoke)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Smoke")
legend("bottomleft", legend = c(unique(pneumon$smoke)), col=c(unique(pneumon$smoke)), pch = 16, 
       cex = 0.5, xjust = 1)

# region
kmfit6 <- survfit(y ~ region, data = pneumon)
plot(kmfit6, col = c(unique(pneumon$region)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Region")
legend("bottomleft", legend = c(unique(pneumon$region)), col=c(unique(pneumon$region)), pch = 16, 
       cex = 0.5, xjust = 1)

# poverty
kmfit7 <- survfit(y ~ poverty, data = pneumon)
plot(kmfit7, col = c(unique(pneumon$poverty)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Poverty")
legend("bottomleft", legend = c(unique(pneumon$poverty)), col=c(unique(pneumon$poverty)), pch = 16, 
       cex = 0.5, xjust = 1)

# bweight
kmfit8 <- survfit(y ~ bweight, data = pneumon)
plot(kmfit8, col = c(unique(pneumon$bweight)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Bweight")
legend("bottomleft", legend = c(unique(pneumon$bweight)), col=c(unique(pneumon$bweight)), pch = 16, 
       cex = 0.5, xjust = 1)

# race
kmfit9 <- survfit(y ~ race, data = pneumon)
plot(kmfit9, col = c(unique(pneumon$race)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Race")
legend("bottomleft", legend = c(unique(pneumon$race)), col=c(unique(pneumon$race)), pch = 16, 
       cex = 0.5, xjust = 1)

# education
kmfit10 <- survfit(y ~ education_cat, data = pneumon)
plot(kmfit10, col = c(unique(pneumon$education_cat)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve Education")
legend("bottomleft", legend = c(unique(pneumon$education_cat)), col=c(unique(pneumon$education_cat)), pch = 16, 
       cex = 0.5, xjust = 1)

# nsibs
kmfit11 <- survfit(y ~ nsibs, data = pneumon)
plot(kmfit11, col = c(unique(pneumon$nsibs)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.5,1))
title(main = "KM Curve nsibs")
legend("bottomleft", legend = c(unique(pneumon$nsibs)), col=c(unique(pneumon$nsibs)), pch = 16, 
       cex = 0.5, xjust = 1)

# wmonth
kmfit12 <- survfit(y ~ wmonth_cat, data = pneumon)
plot(kmfit12, col = c(unique(pneumon$wmonth_cat)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve wmonth")
legend("bottomleft", legend = c(unique(pneumon$wmonth_cat)), col=c(unique(pneumon$wmonth_cat)), pch = 16, 
       cex = 0.5, xjust = 1)

# sfmonth
kmfit13 <- survfit(y ~ sfmonth_cat, data = pneumon)
plot(kmfit13, col = c(unique(pneumon$sfmonth_cat)),xlab="survival time in months", ylab="survival probabilities",
     ylim = c(0.9,1))
title(main = "KM Curve sfmonth")
legend("bottomleft", legend = c(unique(pneumon$sfmonth_cat)), col=c(unique(pneumon$sfmonth_cat)), pch = 16, 
       cex = 0.5, xjust = 1)


#### Log Rank Test ####
# chldage
LR1 <- survdiff(y ~ pneumon$chldage)
LR1$pvalue
if (LR1$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# mthage
LR2 <- survdiff(y ~ pneumon$mthage)
LR2$pvalue
if (LR2$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# urban
LR3 <- survdiff(y ~ pneumon$urban)
LR3$pvalue
if (LR3$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# alcohol
LR4 <- survdiff(y ~ pneumon$alcohol)
LR4$pvalue
if (LR4$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# smoke
LR5 <- survdiff(y ~ pneumon$smoke)
LR5$pvalue
if (LR5$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# region
LR6 <- survdiff(y ~ pneumon$region)
LR6$pvalue
if (LR6$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# poverty
LR7 <- survdiff(y ~ pneumon$poverty)
LR7$pvalue
if (LR7$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# bweight
LR8 <- survdiff(y ~ pneumon$bweight)
LR8$pvalue
if (LR8$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# race
LR9 <- survdiff(y ~ pneumon$race)
LR9$pvalue
if (LR9$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# education
LR10 <- survdiff(y ~ pneumon$education_cat)
LR10$pvalue
if (LR10$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# nsibs
LR11 <- survdiff(y ~ pneumon$nsibs)
LR11$pvalue
if (LR11$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# wmonth
LR12 <- survdiff(y ~ pneumon$wmonth_cat)
LR12$pvalue
if (LR12$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# sfmont
LR13 <- survdiff(y ~ pneumon$sfmonth_cat)
LR13$pvalue
if (LR13$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}


#### Building Cox Model ####
model1 <- coxph(y~. -agepn -hospital, data = pneumon)
summary(model1)

check_ph <- cox.zph(model1, transform = rank)
check_ph$table

#### log log plot ####
minusloglog <- function(p){
  return(-log(-log(p)))
}

# child age
win.graph()
plot(kmfit1, fun = minusloglog, col = c(unique(pneumon$chldage)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot child age")
legend("bottomleft", legend = c(unique(pneumon$chldage)), col=c(unique(pneumon$chldage)), pch = 16, 
       cex = 0.8, xjust = 1)

# mthage
plot(kmfit2, fun = minusloglog, col = c(unique(pneumon$mthage)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot mthage")
legend("bottomleft", legend = c(unique(pneumon$mthage)), col=c(unique(pneumon$mthage)), pch = 16, 
       cex = 0.8, xjust = 1)

# urban
plot(kmfit3, fun = minusloglog, col = c(unique(pneumon$urban)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Urban")
legend("bottomleft", legend = c(unique(pneumon$urban)), col=c(unique(pneumon$urban)), pch = 16, 
       cex = 0.8, xjust = 1)

# alchohol
plot(kmfit4, fun = minusloglog, col = c(unique(pneumon$alcohol)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Alcohol")
legend("bottomleft", legend = c(unique(pneumon$alcohol)), col=c(unique(pneumon$alcohol)), pch = 16, 
       cex = 0.8, xjust = 1)

# smoke
plot(kmfit5, fun = minusloglog, col = c(unique(pneumon$smoke)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Smoke")
legend("bottomleft", legend = c(unique(pneumon$smoke)), col=c(unique(pneumon$smoke)), pch = 16, 
       cex = 0.8, xjust = 1)

# region
plot(kmfit6, fun = minusloglog, col = c(unique(pneumon$region)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Region")
legend("bottomleft", legend = c(unique(pneumon$region)), col=c(unique(pneumon$region)), pch = 16, 
       cex = 0.8, xjust = 1)

# poverty
plot(kmfit7, fun = minusloglog, col = c(unique(pneumon$poverty)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Poverty")
legend("bottomleft", legend = c(unique(pneumon$poverty)), col=c(unique(pneumon$poverty)), pch = 16, 
       cex = 0.8, xjust = 1)

# bweight
plot(kmfit8, fun = minusloglog, col = c(unique(pneumon$bweight)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Bweight")
legend("bottomleft", legend = c(unique(pneumon$bweight)), col=c(unique(pneumon$bweight)), pch = 16, 
       cex = 0.8, xjust = 1)

# race
plot(kmfit9, fun = minusloglog, col = c(unique(pneumon$race)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Race")
legend("bottomleft", legend = c(unique(pneumon$race)), col=c(unique(pneumon$race)), pch = 16, 
       cex = 0.8, xjust = 1)

# education
plot(kmfit10, fun = minusloglog, col = c(unique(pneumon$education_cat)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot Education")
legend("bottomleft", legend = c(unique(pneumon$education_cat)), col=c(unique(pneumon$education_cat)), pch = 16, 
       cex = 0.8, xjust = 1)

# nsibs
plot(kmfit11, fun = minusloglog, col = c(unique(pneumon$nsibs)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot nsibs")
legend("bottomleft", legend = c(unique(pneumon$nsibs)), col=c(unique(pneumon$nsibs)), pch = 16, 
       cex = 0.8, xjust = 1)

# wmonth
plot(kmfit12, fun = minusloglog, col = c(unique(pneumon$wmonth_cat)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot wmonth")
legend("bottomleft", legend = c(unique(pneumon$wmonth_cat)), col=c(unique(pneumon$wmonth_cat)), pch = 16, 
       cex = 0.8, xjust = 1)

# sfmonth
plot(kmfit13, fun = minusloglog, col = c(unique(pneumon$sfmonth_cat)),xlab="survival time in months", ylab="-log -log S")
title(main = "Log Log Plot sfmonth")
legend("bottomleft", legend = c(unique(pneumon$sfmonth_cat)), col=c(unique(pneumon$sfmonth_cat)), pch = 16, 
       cex = 0.8, xjust = 1)
