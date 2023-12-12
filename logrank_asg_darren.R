###################################################################
#
# @filename : logrank_asg2.R
# @brief    : R code to do assignment 2 for Survival Analysis (Q)
# @date     : 20230227
# @author   : S. Andari
#
###################################################################

# @Student        : Darren Kang Wan Chee
# @Student Number : 5003201184

# Klein2003 datasets are pooled in KMsurv package
# Reading materials: https://rdrr.io/cran/KMsurv/

#install.packages("KMsurv") # Installing KMsurv. You need to remove '#' up front to run this line.
library(KMsurv)
## Loading other required packages
library(survival)
library(ggsurvfit)
library(tidycmprsk)


# R code for question (1) and (2)
## Load your data
## (write your code here)
data("kidney")
head(kidney)
View(kidney)
factor(kidney$type)

## Define your survival time
## (complete the following code)
y <- Surv(kidney$time,kidney$delta)

## Plotting KM curves with 95% confidence bands
## (complete the following code)
kmfit_cat <- survfit(y ~ kidney$type)
kmfit_cat %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probabilities"
  ) + 
  add_confidence_interval()
summary(kmfit_cat)
kmfit_cat

## Evaluate the difference between surgical vs percutaneous catheterization
## (complete the following code)
LR <- survdiff(y ~ kidney$type)

if (LR$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}


# R code for question (3): time-to-infection on burn patients
## Read more about the data description here: https://rdrr.io/cran/KMsurv/man/burn.html
## Our variable of interest:
### Z1: cleansing method (0=routine bathing 1=body cleansing)
### T3: Time to straphylocous aureaus infection or on study time
### D3: Straphylocous aureaus infection: 1=yes 0=no

data(burn) # loading data called burn
dim(burn) # getting the dimension of burn data (154 rows by 18 columns)
head(burn) 

burn <- subset(burn, select = -c(Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, T1, D1, T2, D2))
head(burn)
factor(burn$Z1)

y_burn <- Surv(burn$T3,burn$D3)

kmfit_burn <- survfit(y_burn ~ burn$Z1)
kmfit_burn %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probabilities"
  ) + 
  add_confidence_interval()
summary(kmfit_burn)
kmfit_burn

LR_burn <- survdiff(y_burn ~ burn$Z1)
LR_burn

if (LR_burn$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

# R code for question (4)
## Load your data
## (write your code here)
data(bnct)
head(bnct)
## Define your survival time
## (complete the following code)
y_bnct <- Surv(bnct$time,bnct$death)

## Evaluate the difference in survival curves among the three groups
## (complete the following code)
LR_bnct <- survdiff(y_bnct ~ bnct$trt)

if (LR_bnct$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}
LR_bnct
kmfit_bnct <- survfit(y_bnct ~ bnct$trt)
kmfit_bnct %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probabilities"
  ) + 
  add_confidence_interval()
summary(kmfit_bnct)
kmfit_bnct

# R code for question (5): kidney transplant
## Read more about the data description here: https://rdrr.io/cran/KMsurv/man/kidtran.html
## Our variable of interest:
### time: Time to death or on-study time
### delta: Death indicator (0=alive, 1=dead)
### gender: 1=male, 2=female
### race: 1=white, 2=black

data(kidtran) # loading the kidney transplatation data
dim(kidtran)
head(kidtran)

# Re-group data based on gender x race combination
# group 1: male-white (gender=1, race=1)
# group 2: male-black (gender=1, race=2)
# group 3: female-white (gender=2, race=1)
# group 4: female-black (gender=2, race=2)
# Create a new column called 'group'
kidtran$group <- ifelse(kidtran$gender==1 & kidtran$race==1, 1,
                        ifelse(kidtran$gender==1 & kidtran$race==2, 2,
                               ifelse(kidtran$gender==2 & kidtran$race==1, 3,4 )))

## You may want to check your updated kidtran dataframe
head(kidtran)

## Define your survival time
## (complete the following code)
y_kidtran <- Surv(kidtran$time,kidtran$delta)

## Evaluate the difference in survival curves among the three groups
## (complete the following code)
LR_kidtran <- survdiff(y_kidtran ~ kidtran$group)
LR_kidtran
if (LR_kidtran$pvalue < 0.05){
  cat('Reject H0')
}else{
  cat('Fail to Reject H0')
}

## Summary of 95% confidence interval for curves and medians
## (complete the following code)
kmfit_kidtran <- survfit(y_kidtran ~ kidtran$group)
kmfit_kidtran %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probabilities"
  ) + 
  add_confidence_interval()
summary(kmfit_kidtran) # Get the 95% CIs for curve, for each group
kmfit_kidtran # Get the 95% CIs for median, for each group
