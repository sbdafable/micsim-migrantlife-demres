####################################################################################
####################################################################################
## 1. DEFINITION OF MICROSIMULATION PARAMETERS & MODEL                            ##
##                                                                                ##
## Date: July 2019                                                                ##
## Author: Sabine Zinn                                                            ##
## Email: szinn@diw.de                                                            ##
##                                                                                ##
####################################################################################
####################################################################################

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# A. LOAD RESOURCES
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# Empty main storage
rm(list=ls())
# Load R packages
library(chron)
# Load auxiliary functions to define microsimulation input
path <- "C:/Users/Biene/Documents/MicCoreInR/R/" # TODO: CHANGE TO PERSONAL SETTINGS
source(paste(path,"auxFctInMicSim.r",sep=""))

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# B. SIMULATION SPECIFICATION PARAMETERS
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# Here you have to specifiy your microsimulation parameteres and model.
# How this is done is identical to the way you do it when using the MicSim package (in R).

# Definition of simulation horizon
simHorizon <- setSimHorizon(startDate="01/01/2000", endDate="31/12/2010")

# ----------------------------------------------------------------------------------------------------------------------
# Seed for random number generator
set.seed(234)

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# C. MODEL PARAMETERS
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# Definition of maximal age
maxAge <- 100

# ----------------------------------------------------------------------------------------------------------------------
# Defintion of state space  & absorbent states
sex <- c("male","fem")
fert <- c("noChild","firstChild","secondChild","thirdChild","fourthChild")
marital <- c("NM","M","D","W")
edu <- c("noEdu","lowEdu","medEdu","highEdu")
stateSpace <- expand.grid(sex=sex,fert=fert,marital=marital,edu=edu)
absStates <- c("dead","rest")

# ----------------------------------------------------------------------------------------------------------------------
# Definition of initial states for newborns by related probabilities
initStatesMale <- c("male","noChild","NM","noEdu")
initStatesFem <- c("fem","noChild","NM","noEdu")
initStatesProbFem <- 1
initStatesProbMale <- 1
sexRatio <- 0.485

# ----------------------------------------------------------------------------------------------------------------------
# Definition of initial population (for the moment create random population)
N = 200
initBirthDatesRange <- chron(dates=c("31/12/1930","01/01/2000"),format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))
birthDates <- dates(initBirthDatesRange[1] + runif(N, min=0, max=diff(initBirthDatesRange)))
getRandInitState <- function(birthDate){
  age <- trunc(as.numeric(simHorizon[1] - birthDate)/365.25)
  s1 <- sample(sex,1)
  s2 <- ifelse(age<=18, fert[1], ifelse(age<=22, sample(fert[1:3],1), sample(fert,1)))
  s3 <- ifelse(age<=18, marital[1], ifelse(age<=22, sample(marital[1:3],1), sample(marital,1)))
  s4 <- ifelse(age<=7, edu[1], ifelse(age<=18, edu[2], ifelse(age<=23, sample(edu[2:3],1), sample(edu[-1],1))))
  initState <- paste(c(s1,s2,s3,s4), collapse="/")
  return(initState)
}
initPop <- data.frame(ID=1:N, birthDate=birthDates, initState=sapply(birthDates,getRandInitState),stringsAsFactors=FALSE)

# ----------------------------------------------------------------------------------------------------------------------
# Definition of immigrants entering the population (for the moment create immigrants randomly)
M = 50
immigrDatesRange <- as.numeric(simHorizon)
immigrDates <- dates(chron(immigrDatesRange[1] + runif(M, min=0, max=diff(immigrDatesRange)),
 format=c(dates="d/m/Y", times="h:m:s"),out.format=c(dates="d/m/year", times="h:m:s")))
immigrAges <- runif(M, min=15*365.25, max=70*365.25)
immigrBirthDates <- dates(chron(as.numeric(immigrDates) - immigrAges,
  format=c(dates="d/m/Y", times="h:m:s"),out.format=c(dates="d/m/year", times="h:m:s")))
IDmig <- max(as.numeric(initPop[,"ID"]))+(1:M)
immigrPop <- data.frame(ID = IDmig, immigrDate = immigrDates, birthDate=immigrBirthDates,
  immigrInitState=sapply(immigrBirthDates,getRandInitState),stringsAsFactors=FALSE)

# ----------------------------------------------------------------------------------------------------------------------
# Definition of transition rates (that vary along age, calendar time, and duration)
# BEWARE: Each function describing transition rates has to feature at least `age' (in years) as input parameter.
# Furthermore, `calTime' (in years) and `duration' (in years) might enter the function.
# (1) Fertility rates (Hadwiger model)
fert1Rates <- function(age, calTime=1990, duration=0){  # parity 1
 b <- ifelse(calTime<=1950, 3.9, 3.3)
 c <- ifelse(calTime<=1950, 28, 29)
 rate <-  (b/c)*(c/age)^(3/2)*exp(-b^2*(c/age+age/c-2))
 rate[age<=15 | age>=45] <- 0
 return(rate)
}
fert2Rates <- function(age, calTime=1990, duration=0){  # partiy 2+
 b <- ifelse(calTime<=1950, 3.2, 2.8)
 c <- ifelse(calTime<=1950, 32, 33)
 rate <-  (b/c)*(c/age)^(3/2)*exp(-b^2*(c/age+age/c-2))
 rate[age<=15 | age>=45 | duration<1] <- 0
 return(rate)
}
# (2) Rates for first marriage (normal density)
marriage1Rates <- function(age, calTime=1990, duration=0){
 m <- ifelse(calTime<=1950, 25, 30)
 s <- ifelse(calTime<=1950, 3, 3)
 rate <- dnorm(age, mean=m, sd=s)
 rate[age<=16] <- 0
 return(rate)
}
# (3) Remariage rates (logistic model)
marriage2Rates <- function(age, calTime=1990, duration=0){
  b <- ifelse(calTime<=1950, 0.07, 0.10)
  p <- ifelse(calTime<=1950, 2.7,2.7)
  lambda <- ifelse(calTime<=1950, 0.04, 0.03)
  rate <- b*p*(lambda*age)^(p-1)/(1+(lambda*age)^p)
  rate[age<=18] <- 0
  return(rate)
}
# (4) Divorce rates (normal density)
divorceRates <- function(age, calTime=1990, duration=0){
  m <- ifelse(calTime<=1950, 40, 40)
  s <- ifelse(calTime<=1950, 7, 6)
  rate <- dnorm(age,mean=m,sd=s)
  rate[age<=18] <- 0
  return(rate)
}
# (5) Widowhood rates (gamma cdf)
widowhoodRates <- function(age, calTime=1990, duration=0){
 rate <- ifelse(age<=30, 0, pgamma(age-30, shape=6, rate=0.06))
 return(rate)
}
# (6) Rates to change educational attainment
noToLowEduRates <- function(age, calTime=1990, duration=0){
 rate <- ifelse(age==7,Inf,0) # Put it Inf to make it deterministic.
 return(rate)
}
lowToMedEduRates <- function(age, calTime=1990, duration=0){
 rate <- dnorm(age,mean=16,sd=1)
 rate[age<=15 | age>=25] <- 0
 return(rate)
}
medToHighEduRates <- function(age, calTime=1990, duration=0){
 rate <- dnorm(age,mean=20,sd=3)
 rate[age<=18 | age>=35] <- 0
 return(rate)
}
# (7) Mortality rates (Gompertz model)
mortRates <- function(age, calTime=1990, duration=0){
  a <- ifelse(calTime<=1950, 0.00003, 0.00003)
  b <- ifelse(calTime<=1950, 0.1, 0.097)
  rate <- a*exp(b*age)
  return(rate)
}
# (8) Emigration rates
emigrRates <- function(age, calTime=1990, duration=0){
  rate <- ifelse(age<=18,0,0.0025)
  return(rate)
}

# ----------------------------------------------------------------------------------------------------------------------
# Transition pattern and assignment of functions specifying transition rates
fertTrMatrix <- cbind(c("noChild->firstChild","firstChild->secondChild","secondChild->thirdChild","thirdChild->fourthChild"),
                c("fert1Rates", "fert2Rates", "fert2Rates","fert2Rates"))
maritalTrMatrix <- cbind(c("NM->M","M->D","M->W","D->M","W->M"),
                   c("marriage1Rates","divorceRates","widowhoodRates","marriage2Rates","marriage2Rates"))
eduTrMatrix <- cbind(c("noEdu->lowEdu","lowEdu->medEdu","medEdu->highEdu"),
               c("noToLowEduRates","lowToMedEduRates","medToHighEduRates"))
allTransitions <- rbind(fertTrMatrix, maritalTrMatrix, eduTrMatrix)
absTransitions <- rbind(c("dead","mortRates"),c("rest","emigrRates"))

# This is an aux function helping you to put everything in a format that can be written for MicCore. 
transitionMatrix <- buildTransitionMatrix(allTransitions,absTransitions,stateSpace) 

# ----------------------------------------------------------------------------------------------------------------------
# Write input files for MicCore
pathOut <- "C:\\Users\\Biene\\Documents\\MicCoreInR\\MicCoreIn\\"
writeInputMicCore(pathOut,mig=T)



