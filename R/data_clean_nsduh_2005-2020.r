# Set directory where data will be saved
mainDir <- "C:/Users/JT936/Dropbox/Analysis/NSDUH"
setwd(file.path(mainDir))

# library(dplyr)
# library(tidyselect)
# library(plyr)

# Load NSDUH 2002-2018 data (may take some time as this is a large file)
# load("NSDUH_2002_2018.RData")
# nsduh <- CONCATPUF_0218_010320
# 
# nsduh<- nsduh %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol")),contains(c("preg","K6","SPD","smi","ami","mde","race","edu")))

## Harmonize variables and reduce size of dataset
# nsduh <- nsduh[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","ajamdelt","ajamdeyr", "cigyr","CIG100LF","amdetxrx","ahltmde", "cigmon","ircigrc)] # Keep only the variables needed
# load("C:/Users/JT936/Dropbox/Analysis/NSDUH/nsduhvars_2005-2018.rda")
# nsduh0518 <- subset(nsduh, year>=2005 & CATAG6 >1) # only keep data after 2005 for adults ages 18+
# save(nsduh, file="nsduhvars_2005-2018.rda")

# Due to questionnaire changes in 2008, the variables AMDELT and AMDEYR are not comparable pre-2008 vs. 2008-2019. 
# table(nsduh$ajamdeyr, nsduh$year) 
# table(nsduh$amdeyr, nsduh$year)
# nsduh$amdeyr <- ifelse(nsduh$year<2008,nsduh$ajamdeyr,nsduh$amdeyr) # Adjusted variables AJAMDELT and AJAMDEYR were developed to allow for comparisons of adult MDE data from 2005-2019
# table(nsduh$amdeyr, nsduh$year)
# 
# table(nsduh$ajamdelt, nsduh$year)
# table(nsduh$amdelt, nsduh$year)
# nsduh$amdelt <- ifelse(nsduh$year<2008,nsduh$ajamdelt,nsduh$amdelt) # Adjusted variables AJAMDELT and AJAMDEYR were developed to allow for comparisons of adult MDE data from 2005-2019
# table(nsduh$amdelt, nsduh$year)
# 
# table(nsduh$ahltmde, nsduh$year) # 2010-2018 only
# nsduh[nsduh$ahltmde==-9] <-NA
# table(nsduh$amdetxrx, nsduh$year) # 2008-2018 only
# 
# save(nsduh, file="nsduhvars_2002-2018.rda")

# Add NSDUH 2019
# load("C:/Users/JT936/Dropbox/Analysis/NSDUH/NSDUH_2019.RData")
# nsduh19 <- PUF2019_100920
# nsduh19$year <- 2019
# nsduh19$ANALWC1 <- nsduh19$ANALWT_C
# nsduh19<- nsduh19 %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol")),contains(c("preg","K6","SPD","smi","ami","mde","race","edu")))
# Add NSDUH 2020
# load("C:/Users/JT936/Dropbox/Analysis/NSDUH/NSDUH_2020.Rdata")
# nsduh20 <- NSDUH_2020
# nsduh20$year <- 2020
# nsduh20$vestr <- nsduh20$VESTRQ1Q4_C
# nsduh20$ANALWC1 <- nsduh20$ANALWTQ1Q4_C
# nsduh20<- nsduh20 %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol")),contains(c("vap", "preg","K6","SPD","smi","ami","mde","race","edu")))

# nsduh0220 <- rbind.fill(nsduh20, nsduh19)
# nsduh0220 <- rbind.fill(nsduh0220, nsduh)
# save(nsduh0220, file="nsduhvars_2002-2020.rda")

##
load("nsduhvars_2002-2020.rda")

nsduh0520<-subset(nsduh0220,year>=2005&CATAG6>1)
nsduh0520<-nsduh0520[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","cigyr","CIG100LF","amdetxrx","ahltmde","cigmon","ircigrc",
"vapnicevr","vapnicrec","vapanyrec","vapanyevr","vapanyflag","vapanyyr","vapanymon","vapnicflag","vapnicyr","vapnicmon")] # Keep only the variables needed

## ECIG USE - Vaping nicotine or tobacco
nsduh0520$nevervap[nsduh0520$vapnicevr==2 | nsduh0520$vapnicevr==91] <-1
nsduh0520$nevervap[nsduh0520$vapnicevr==1 ] <-0

nsduh0520$evervap[nsduh0520$vapnicevr==2 | nsduh0520$vapnicevr==91] <-0
nsduh0520$evervap[nsduh0520$vapnicevr==1 ] <-1

# vaped nicotine or tobacco, but not in past 30 days
nsduh0520$formervap[nsduh0520$evervap==1 &  (nsduh0520$vapnicrec==2 | nsduh0520$vapnicrec==3 | nsduh0520$vapnicrec==9 | nsduh0520$vapnicrec==19)] <-1
nsduh0520$formervap[nsduh0520$evervap==1 &  nsduh0520$vapnicrec==1] <- 0
nsduh0520$formervap[nsduh0520$evervap==0] <- 0

# vaped nicotine or tobacco in past 30 days
nsduh0520$currentvap[nsduh0520$evervap==1 & nsduh0520$vapnicrec==1] <- 1 
nsduh0520$currentvap[nsduh0520$formervap==1] <- 0 
nsduh0520$currentvap[nsduh0520$evervap==0] <- 0

# cigmon = cigarette smoked within the past 30 days

# dual cig and e-cig(nicotine) within past 30 days
nsduh0520$dual[nsduh0520$cigmon==1 & nsduh0520$currentvap==1] <- 1 
nsduh0520$dual[nsduh0520$cigmon==0 & nsduh0520$currentvap==1] <- 0 
nsduh0520$dual[nsduh0520$cigmon==1 & nsduh0520$currentvap==0] <- 0 
nsduh0520$dual[nsduh0520$cigmon==0 & nsduh0520$currentvap==0] <- 0 

# exclusive cig past 30 days, no e-cig nicotine vaping in past 30 days
nsduh0520$exclcig[nsduh0520$cigmon==1 & nsduh0520$currentvap==1] <- 0
nsduh0520$exclcig[nsduh0520$cigmon==0 & nsduh0520$currentvap==1] <- 0 
nsduh0520$exclcig[nsduh0520$cigmon==1 & nsduh0520$currentvap==0] <- 1 
nsduh0520$exclcig[nsduh0520$cigmon==0 & nsduh0520$currentvap==0] <- 0 

# exclusive vape (nicotine) past 30 days, no cig smoked in past 30 days
nsduh0520$exclvap[nsduh0520$cigmon==1 & nsduh0520$currentvap==1] <- 0
nsduh0520$exclvap[nsduh0520$cigmon==0 & nsduh0520$currentvap==1] <- 1 
nsduh0520$exclvap[nsduh0520$cigmon==1 & nsduh0520$currentvap==0] <- 0 
nsduh0520$exclvap[nsduh0520$cigmon==0 & nsduh0520$currentvap==0] <- 0

# no use of vape (nicotine) OR cig smoked in past 30 days
nsduh0520$neither[nsduh0520$cigmon==1 & nsduh0520$currentvap==1] <- 0
nsduh0520$neither[nsduh0520$cigmon==0 & nsduh0520$currentvap==1] <- 0 
nsduh0520$neither[nsduh0520$cigmon==1 & nsduh0520$currentvap==0] <- 0 
nsduh0520$neither[nsduh0520$cigmon==0 & nsduh0520$currentvap==0] <- 1

# tobacco product use status for vaping nicotine and smoking cigarettes in past 30 days
nsduh0520$tobstat[nsduh0520$cigmon==1 & nsduh0520$currentvap==1] <- 3 # dual use
nsduh0520$tobstat[nsduh0520$cigmon==0 & nsduh0520$currentvap==1] <- 1 # vaping only
nsduh0520$tobstat[nsduh0520$cigmon==1 & nsduh0520$currentvap==0] <- 2 # smoking only
nsduh0520$tobstat[nsduh0520$cigmon==0 & nsduh0520$currentvap==0] <- 0 # neither

## Assign MD status based on past year and lifetime reports of MD

# Current MD = past year MDE
nsduh0520$dep[nsduh0520$amdeyr==1] <- 1
nsduh0520$dep[nsduh0520$amdeyr==2] <- 0

# Former MD = no past year MDE, but lifetime MDE
nsduh0520$fdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==1] <- 1
nsduh0520$fdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==2] <- 0
nsduh0520$fdep[nsduh0520$amdeyr==1] <- 0

# Never MD = no reported lifetime MDE (but may be subject to recall error)
nsduh0520$nevdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==2] <- 1 
nsduh0520$nevdep[nsduh0520$amdeyr==1& nsduh0520$amdelt==1] <- 0
nsduh0520$nevdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==1] <- 0

# Ever MD = reported lifetime MDE
nsduh0520$everdep[nsduh0520$nevdep==1] <- 0 
nsduh0520$everdep[nsduh0520$nevdep==0] <- 1

# smoked within the past 12 months and at least 100 cigs in lifetime
# nsduh0520$currentsmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-1
# nsduh0520$currentsmoker[nsduh0520$cigyr==0] <-0
# nsduh0520$currentsmoker[nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91] <-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0520$currentsmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-1
nsduh0520$currentsmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0520$currentsmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)] <-0
nsduh0520$currentsmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-0

# did not smoke within the past 12 months but at least 100 cigs in lifetime
# nsduh0520$formersmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-1
# nsduh0520$formersmoker[nsduh0520$cigyr==1] <-0
# nsduh0520$formersmoker[nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91] <-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0520$formersmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-0
nsduh0520$formersmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0520$formersmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)] <-1
nsduh0520$formersmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-0

#never smoked 100 cigarettes in lifetime or never smoked at all
# nsduh0520$neversmoker[nsduh0520$CIG100LF==2 |nsduh0520$CIG100LF==91] <- 1
# nsduh0520$neversmoker[nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5] <- 0
nsduh0520$neversmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-0
nsduh0520$neversmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-1 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0520$neversmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)] <-0
nsduh0520$neversmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==2 | nsduh0520$CIG100LF==91)]<-1

# ever smoked 100 cigarettes in lifetime
# nsduh0520$eversmoker[nsduh0520$CIG100LF==2 |nsduh0520$CIG100LF==91] <- 0
# nsduh0520$eversmoker[nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5] <- 1

save(nsduh0520, file="nsduh2005-2020clean.Rda")
load("nsduh2005-2020clean.Rda")
# Load survey packages
library(survey)
options(survey.lonely.psu="adjust")
nsduh0520 <- nsduh0520[order(nsduh0520$vestr, nsduh0520$verep),] # re-order survey design variables

## Generate age group-specific prevalences and confidence intervals across adult population
getprevsbyage <- function(groupvar, subpop){
  byagegroup = NULL
  alladults = NULL
  for (y in 2005:2020){
    svy <-svydesign(id=~verep, strata=~vestr, nest=TRUE, weights=~ANALWC1, data=subset(subpop,year==y))

    prev <-svymean(as.formula(paste("~",groupvar)),design=svy,na.rm=TRUE) 
    alladults  <- rbind(alladults, data.frame(y,18.99,groupvar, deparse(substitute(subpop)), prev[1],SE(prev), confint(prev)[1,1], confint(prev)[1,2]))          
    
    agegroupnames <- c(18.25, 26.34,35.49, 50.64, 65.99)
    for (k in 2:6){
      prev <-svymean(as.formula(paste("~",groupvar)),design=subset(svy,CATAG6==k),na.rm=TRUE) # 
      byagegroup <- rbind(byagegroup, data.frame(y,agegroupnames[k-1],groupvar, deparse(substitute(subpop)), prev[1],SE(prev),confint(prev)[1,1], confint(prev)[1,2]))          
    }

  }
  names(alladults) <- names(byagegroup)
  byagegroup <- rbind(alladults, byagegroup)
  colnames(byagegroup) <- c("survey_year","age","status","subpopulation","prev","se", "prev_lowCI","prev_highCI")
  return(byagegroup)
}

get2020prevs<- function(groupvar, subpop){
  byagegroup = NULL
  alladults = NULL
  for (y in 2020:2020){
    svy <-svydesign(id=~verep, strata=~vestr, nest=TRUE, weights=~ANALWC1, data=subset(subpop,year==y))
    
    prev <-svymean(as.formula(paste("~",groupvar)),design=svy,na.rm=TRUE) 
    alladults  <- rbind(alladults, data.frame(y,"total",groupvar, deparse(substitute(subpop)), prev[1],SE(prev), confint(prev)[1,1], confint(prev)[1,2]))          
    
    agegroupnames <- c(18.25, 26.34,35.49, 50.64, 65.99)
    for (k in 2:6){
      prev <-svymean(as.formula(paste("~",groupvar)),design=subset(svy,CATAG6==k),na.rm=TRUE) # 
      byagegroup <- rbind(byagegroup, data.frame(y,agegroupnames[k-1],groupvar, deparse(substitute(subpop)), prev[1],SE(prev),confint(prev)[1,1], confint(prev)[1,2]))          
    }
    
  }
  names(alladults) <- names(byagegroup)
  byagegroup <- rbind(alladults, byagegroup)
  colnames(byagegroup) <- c("survey_year","age","status","subpopulation","prev","se", "prev_lowCI","prev_highCI")
  return(byagegroup)
}

## Create new dataframe with NSDUH prevalences for model fitting
mdseprevsB <- NULL
totalpop <- nsduh0520
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("neither",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclvap",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclcig",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("currentvap",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("dual",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",totalpop)))

deppop <- subset(nsduh0520, nsduh0520$dep==1)
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("neither",deppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclvap",deppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclcig",deppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("currentvap",deppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("dual",deppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",deppop)))

nevdeppop <- subset(nsduh0520, nsduh0520$nevdep==1)
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("neither",nevdeppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclvap",nevdeppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("exclcig",nevdeppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("currentvap",nevdeppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",get2020prevs("dual",nevdeppop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",nevdeppop)))
mdseprevsB <- rbind(mdseprevsB, cbind("both", getprevsbyage("dep",totalpop)))

colnames(mdseprevsB)[1] <- "gender"
mdseprevsB$prev=as.numeric(mdseprevsB$prev)
mdseprevsB$se=as.numeric(mdseprevsB$se)
mdseprevsB$prev_highCI=as.numeric(mdseprevsB$prev_highCI)
mdseprevsB$prev_lowCI=as.numeric(mdseprevsB$prev_lowCI)

mdseprevs <- NULL

gender <- c("Men", "Women")
for (x in 1:2){# irsex: 1 = males, 2 = females
  totalpop <- subset(nsduh0520, nsduh0520$irsex==x)
  
  currentvapers <- subset(nsduh0520, nsduh0520$currentvap==1)
  dualusers <- subset(nsduh0520, nsduh0520$dual==1)
  exclvapers <- subset(nsduh0520, nsduh0520$exclvap==1)
  exclsmokers<- subset(nsduh0520, nsduh0520$exclcig==1)
  neitherpop <- subset(nsduh0520, nsduh0520$neitherpop==1)
   
  currentsmokers <- subset(nsduh0520, nsduh0520$currentsmoker==1 & nsduh0520$irsex==x) # PREVALENCE ESTIMATES ARE LOWER COMPARED TO depsmkprevs_by_year. NEED TO CHECK DATA.
  formersmokers <- subset(nsduh0520, nsduh0520$formersmoker==1 & nsduh0520$irsex==x)
  neversmokers <- subset(nsduh0520, nsduh0520$neversmoker==1 & nsduh0520$irsex==x)
  
  deppop <- subset(nsduh0520, nsduh0520$dep==1 & nsduh0520$irsex==x)
  fdeppop <- subset(nsduh0520, nsduh0520$fdep==1 & nsduh0520$irsex==x)
  nevdeppop <- subset(nsduh0520, nsduh0520$nevdep==1 & nsduh0520$irsex==x)
  everdeppop <- subset(nsduh0520, nsduh0520$everdep==1 & nsduh0520$irsex==x)
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",deppop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",nevdeppop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dep",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("fdep",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("nevdep",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("everdep",totalpop)))
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("currentsmoker",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("formersmoker",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neversmoker",totalpop)))
   
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dep",neversmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("fdep",neversmokers)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("nevdep",neversmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("everdep",neversmokers)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dep",currentsmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("notdep",currentsmokers)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("nevdep",currentsmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("everdep",currentsmokers)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dep",formersmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("fdep",formersmokers)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("nevdep",formersmokers)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("everdep",formersmokers)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("currentsmoker",deppop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("formersmoker",deppop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neversmoker",deppop)))

  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("currentsmoker",fdeppop)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("formersmoker",fdeppop)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neversmoker",fdeppop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("currentsmoker",nevdeppop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("formersmoker",nevdeppop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neversmoker",nevdeppop)))

  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("currentsmoker",everdeppop)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("formersmoker",everdeppop)))
  # mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neversmoker",everdeppop)))
  
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("neither",totalpop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclvap",totalpop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclcig",totalpop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("currentvap",totalpop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("dual",totalpop)))
  
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("neither",deppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclvap",deppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclcig",deppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("currentvap",deppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("dual",deppop)))
  
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("neither",nevdeppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclvap",nevdeppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("exclcig",nevdeppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("currentvap",nevdeppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("dual",nevdeppop)))
  mdseprevs<-rbind(mdseprevs,cbind(gender[x],get2020prevs("cigmon",nevdeppop)))
  
} 

colnames(mdseprevs)[1] <- "gender"
mdseprevs$prev=as.numeric(mdseprevs$prev)
mdseprevs$se=as.numeric(mdseprevs$se)
mdseprevs$prev_highCI=as.numeric(mdseprevs$prev_highCI)
mdseprevs$prev_lowCI=as.numeric(mdseprevs$prev_lowCI)

mdseprevs <- rbind(mdseprevsB, mdseprevs)
mdseprevs$group <- paste0(mdseprevs$gender,"_",mdseprevs$status,"_",mdseprevs$subpopulation)

mdseprevs$sex[mdseprevs$gender=="Women"] <-"females"
mdseprevs$sex[mdseprevs$gender=="Men"] <-"males"
mdseprevs$sex[mdseprevs$gender=="both"] <-"both"
save(mdseprevs, file="mdseprevs0520.rda")
# tab <- subset(mdseprevs, age=="total" & survey_year==2020& status=="dep")
# tab$prev <- round(tab$prev,3)
# tab$prev_lowCI <- round(tab$prev,3)
# tab$prev_highCI <- round(tab$prev,3)

load("mdseprevs0520.rda")

# levels(depsmkprevs_by_year$gender)<-c("Men","Women")
# levels(depsmkprevs_by_year$status)[1:7] <-c("Current MD", "Former MD", "Never MD","Ever MD", "Current smoker", "Former smoker",  "Never smoker"  )
# levels(depsmkprevs_by_year$subpopulation)<-c("Total","Never smokers","Current smokers","Former smokers","Current MD", "Former MD","Never MD","Ever MD")
# 
# load("mdseprevs2020.rda")


# Data visualization and results figures -------------------------------------------------------------------
library(ggplot2)
library(reshape)
library(grid)
library(gridBase)
library(gridExtra)
library(plyr)

xaxisbreaks = seq(2005,2020,1) # specify the ticks on the x-axis of your results plots
date = "SRNT2023"

theme_set( theme_light(base_size = 17))

# Combine plots with shared legend
grid_arrange_shared_legend <- function(plots,columns,titletext) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(arrangeGrob(grobs= lapply(plots, function(x)
    x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),ncol=columns),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    top=textGrob(titletext,just="top", vjust=1,check.overlap=TRUE,gp=gpar(fontsize=12, fontface="bold"))
  )
}

mdseprevs$subpopulation[mdseprevs$subpopulation=="deppop"] <- "Current MDE"
mdseprevs$subpopulation[mdseprevs$subpopulation=="nevdeppop"] <- "Never MDE"
mdseprevs$subpopulation[mdseprevs$subpopulation=="totalpop"] <- "Total"
smkprevbyMDF <- ggplot()+
  geom_pointrange(data= subset(mdseprevs,age=="18to25"&gender=="Women"&status=="currentsmoker"&(subpopulation=="Total"|subpopulation=="Current MDE")), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,65),breaks=seq(0,65,5)) +
  labs(title="Women, ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 17),legend.position = c(0.95, 0.02),   legend.justification = c("right", "bottom"),
        legend.box.just = "right",  legend.margin = margin(6, 6, 6, 6),legend.box.background = element_rect(color="black", size=1),)
smkprevbyMDM <- ggplot()+
  geom_pointrange(data= subset(mdseprevs,age=="18to25"&gender=="Men"&status=="currentsmoker"&(subpopulation=="Total"|subpopulation=="Current MDE")), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,65),breaks=seq(0,65,5)) +
  labs(title="Men, ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 17),legend.position = c(0.95, 0.02),   legend.justification = c("right", "bottom"),
        legend.box.just = "right",  legend.margin = margin(6, 6, 6, 6),legend.box.background = element_rect(color="black", size=1),)

jpeg(filename = paste0("smkprevbyMD","_" ,date, ".jpg"),width=10, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(smkprevbyMDF,smkprevbyMDM),2,"Current smoking prevalence, NSDUH 2005-2020")
dev.off()

# Current MD (Past year MD) Prevalence 1997-2020
MDprev<- ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age=="total"&subpopulation=="Total"&status=="Current MD"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,26),breaks=seq(0,26,2)) +
  labs(title="Current MD, Ages 18+")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev18.25 <-ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="18to25"&subpopulation=="Total"&status=="Current MD"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,26),breaks=seq(0,26,2)) +
  labs(title="Current MD, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 14))
MDprev18.25cs <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age=="18to25"&subpopulation=="currentsmokers"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MDE prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
  labs(title="Current smoking, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))
MDprev18.25fs <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age=="18to25"&subpopulation=="formersmokers"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MDE prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
  labs(title="Former smoking, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))
MDprev18.25ns <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age=="18to25"&subpopulation=="neversmokers"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MDE prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
  labs(title="Never smoked, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

jpeg(filename = paste0("MDprev","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(MDprev,MDprev18.25),2,"")
dev.off()
jpeg(filename = paste0("MDEprevbysmk","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(MDprev18.25ns,MDprev18.25cs,MDprev18.25fs),3,"Current MDE, ages 18-25")
dev.off()

# Current MDE Prevalence 2005-2020
MDprev_cspop<- ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="total"&subpopulation=="Current smokers"&status=="Current MD"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Current smokers")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev_fspop<- ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="total"&subpopulation=="Former smokers"&status=="Current MD"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Former smokers")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev_nspop<- ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="total"&subpopulation=="Never smokers"&status=="Current MD"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Never smokers")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

jpeg(filename = paste0("MDprev_smkstat","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(MDprev_nspop,MDprev_cspop, MDprev_fspop),3,"")
dev.off()

# smkdist<- subset(depsmkprevs_by_year,status=="currentsmoker"|status=="formersmoker"|status=="neversmoker")
# smkdist_deppopF<- ggplot() + 
#   geom_pointrange(data= subset(smkdist,age=="total"&subpopulation=="deppop"&gender=="females"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=status))+
#   scale_y_continuous(name="Smoking distribution (%)",limits=c(0,100),breaks=seq(0,100,5)) +
#   scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
#   labs(title="Women with Current MD, Ages 18+")+
#   theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(),text = element_text(size = 20))
# 
# smkdist_deppopM<- ggplot() + 
#   geom_pointrange(data= subset(smkdist,age=="total"&subpopulation=="deppop"&gender=="males"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=status))+
#   scale_y_continuous(name="Smoking distribution (%)",limits=c(0,100),breaks=seq(0,100,5)) +
#   scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
#   labs(title="Men with Current MD, Ages 18+")+
#   theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(),text = element_text(size = 20))
# 
# smkdist_totalpopF<- ggplot() + 
#   geom_pointrange(data= subset(smkdist,age=="total"&subpopulation=="totalpop"&gender=="females"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=status))+
#   scale_y_continuous(name="Smoking distribution (%)",limits=c(0,100),breaks=seq(0,100,5)) +
#   scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
#   labs(title="Women, Ages 18+")+
#   theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(),text = element_text(size = 20))
# 
# smkdist_totalpopM<- ggplot() + 
#   geom_pointrange(data= subset(smkdist,age=="total"&subpopulation=="totalpop"&gender=="males"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=status))+
#   scale_y_continuous(name="Smoking distribution (%)",limits=c(0,100),breaks=seq(0,100,5)) +
#   scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
#   labs(title="Men, Ages 18+")+
#   theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(),text = element_text(size = 20))

smkprevF<- ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="total"&
                                 (subpopulation=="Current MD"|subpopulation=="Never MD")&status=="Current smoker"&gender=="Women"), 
                  aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,55),breaks=seq(0,55,5)) +
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  labs(title="Smoking prevalence (Past Year), Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

smkprevM <- ggplot() + 
  geom_pointrange(data= subset(depsmkprevs_by_year,age=="total"&
                                 (subpopulation=="Current MD"|subpopulation=="Never MD")&status=="Current smoker"&gender=="Men"), 
                  aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,55),breaks=seq(0,55,5)) +
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  labs(title="Smoking prevalence (Past Year), Men")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

jpeg(filename = paste0("smkprevbyMD","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(smkprevF,smkprevM),2,"")
dev.off()

# Past 30 day tobacco use by MD status, 2020
bardata <- subset(mdseprevs, age=="total"& (status=="dual" | status=="exclcig" | status=="exclvap" | status=="neither") & (subpopulation=="deppop" | subpopulation=="nevdeppop" | subpopulation=="totalpop"))
bardata$subpopulation=as.factor(bardata$subpopulation)
levels(bardata$subpopulation) <- c("Current MDE","Never MDE", "Total")
bardata$status=as.factor(bardata$status)
levels(bardata$status) <- c("Dual use","Exclusive cig", "Exclusive e-cig", "Neither")
bardata$status = factor(bardata$status, levels=c("Neither","Exclusive e-cig","Exclusive cig","Dual use")) # reorder factor levels
group.colors <- c("Dual use" = "#0000FF", "Exclusive cig" = "#000000", "Exclusive e-cig" ="#00CCFF", "Neither" = "#CCCCCC")
bardata$prev = round(bardata$prev*100,1)

# tobaccostatM <-ggplot(data=subset(bardata,gender=="Men")) + geom_col(aes(x=subpopulation,y=prev,fill=status))+
#   scale_fill_manual(values=group.colors)+
#   scale_y_continuous(name="Prevalence (%)",limits=c(0,101),breaks=seq(0,100,5)) +
#   labs(title="Past 30 day tobacco use, Men")+xlab("Population")+
#   theme(axis.title.x = element_blank(),legend.title = element_blank(), text = element_text(size = 15))+
#   geom_label(position=position_stack(vjust=0.5), aes(x=subpopulation,y=prev, group=status, label=prev))

tobaccostat<-ggplot(data=subset(bardata,gender=="both")) + geom_col(aes(x=subpopulation,y=prev,fill=status))+
  scale_fill_manual(values=group.colors)+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,101),breaks=seq(0,100,5)) +
  labs(title="Past 30 day tobacco use, NSDUH 2020")+xlab("Population")+
  theme(axis.title.x = element_blank(),legend.title = element_blank(), text = element_text(size = 15))+
  geom_label(position=position_stack(vjust=0.5), aes(x=subpopulation,y=prev, group=status, label=prev))

jpeg(filename = paste0("tobstatbyMDE","_" ,"SRNT2023", ".jpg"),width=6, height=6, units ="in", res=1000)
tobaccostat
dev.off()       