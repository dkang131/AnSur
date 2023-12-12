library(survival)
data=read.csv("C:/MATERI KULIAH SEMESTER 5/SURVIVAL ANALYSIS/gbsg.csv")
head(data)
str(data)

data$meno=as.factor(data$meno)
data$grade=as.factor(data$grade)
data$hormon=as.factor(data$hormon)

str(data)

#Mendefinisikan respon
Y=Surv(data$rfstime,data$status==1)

#Deskriptif data
kmfit1=survfit(Y~1)
kmfit1

#Plot Kurva KM untuk Survival Time
plot(kmfit1,xlab="survival time in days", ylab="survival probabilities")
summary(kmfit1)

#Plot Kurva KM untuk Survival Time hormonal treatment
kmfit2=survfit(Y ~ hormon, data=data)
kmfit2
summary(kmfit2)

plot(kmfit2, lty=c("solid","dashed"), col=c("red", "blue"), xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("other therapy", "hormonal therapy"), lty=c("solid","dashed"), col=c("red", "blue"))

plot(kmfit2, fun="cloglog", xlab="time in month using log scale", ylab="log-log survival")

#log rank test
survdiff(Y~hormon, data=data)
#p-value<alpha terdapat perbedaan

#cox ph model1
model1=coxph(Y~hormon+meno+size+nodes+er, data=data)
summary(model1) 
anova(model1)
#goodness of fit (GOF) model 1
cox.zph(model1,transform=rank)

#stratifikasi pada variabel meno

#model 1 no interaction stratified
model1strat=coxph(Y~hormon+strata(meno)+size+nodes+er, data=data)
summary(model1strat)
#goodness of fit (GOF) model 1 stratified
cox.zph(model1strat,transform=rank)

#model 2 stratified with interaction
model2<-coxph(Y~hormon+strata(meno)+size+nodes+er+(meno*hormon)+
                (meno*size)+(meno*nodes)+(meno*er), data=data)
summary(model2)
#goodness of fit (GOF) model 2 
cox.zph(model2,transform=rank)

#H0: no-interaction model is acceptable
LRT=(-2)*(model1strat$loglik[2]-model2$loglik[2])
LRT
Pvalue = 1 - pchisq(LRT,1)
Pvalue
#reject H0 when p-value < 0.05
#if reject
#no difference between no interaction model and interaction model, 


#corat coret
#Model no interaction is acceptable
LRT1=(-2)*(model1$loglik[2]-model1strat$loglik[2])
LRT1
Pvalue1 = 1 - pchisq(LRT1,1)
Pvalue1
model1

#chisq_stat <- -2*(LL.strat - LL.nostrat)
LRT1=(-2)*(model1strat$loglik[2]-model1$loglik[2])
LRT1

pval <- 1 - pchisq(LRT1,1)
pval

ncoef.dif <- length(model1strat$coefficients) - length(model1$coefficients) # 2
