# Set directory where data will be saved
# mainDir <- "C:/Users/jamietam/Dropbox/Analysis/R/NSDUH"
mainDir <- "C:/Users/JT936/Dropbox/Analysis/R/NSDUH"
setwd(file.path(mainDir))

# Load NSDUH 2002-2018 data (may take some time as this is a large file)
# load("NSDUH_2002_2018.RData")
# load("C:/Users/jamietam/Dropbox/Analysis/R/NSDUH/NSDUH_2002_2018.RData")
# nsduh0518 <- CONCATPUF_0218_010320

## Harmonize variables and reduce size of dataset
# nsduh <- nsduh[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","ajamdelt","ajamdeyr", "cigyr","CIG100LF","amdetxrx","ahltmde")] # Keep only the variables needed
# load("C:/Users/JT936/Dropbox/Analysis/NSDUH/nsduhvars_2005-2018.rda")
# nsduh0518 <- subset(nsduh, year>=2005 & CATAG6 >1) # only keep data after 2005 for adults ages 18+
# save(nsduh, file="nsduhvars_2005-2018.rda")
# Due to questionnaire changes in 2008, the variables AMDELT and AMDEYR are not comparable pre-2008 vs. 2008-2019. 
table(nsduh$ajamdeyr, nsduh$year) 
table(nsduh$amdeyr, nsduh$year)
nsduh$amdeyr <- ifelse(nsduh$year<2008,nsduh$ajamdeyr,nsduh$amdeyr) # Adjusted variables AJAMDELT and AJAMDEYR were developed to allow for comparisons of adult MDE data from 2005-2019
table(nsduh$amdeyr, nsduh$year)

table(nsduh$ajamdelt, nsduh$year)
table(nsduh$amdelt, nsduh$year)
nsduh$amdelt <- ifelse(nsduh$year<2008,nsduh$ajamdelt,nsduh$amdelt) # Adjusted variables AJAMDELT and AJAMDEYR were developed to allow for comparisons of adult MDE data from 2005-2019
table(nsduh$amdelt, nsduh$year)

table(nsduh$ahltmde, nsduh$year) # 2010-2018 only
nsduh[nsduh$ahltmde==-9] <-NA
table(nsduh$amdetxrx, nsduh$year) # 2008-2018 only

nsduh <- nsduh[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","cigyr","CIG100LF","amdetxrx","ahltmde")] # Keep only the variables needed

# Add NSDUH 2020
load("C:/Users/JT936/Dropbox/Analysis/NSDUH/NSDUH_2020.Rdata")
nsduh20 <- subset(NSDUH_2020, CATAG6 >1)
nsduh20$year <- 2020
nsduh20$vestr <- nsduh20$VESTRQ1Q4_C
nsduh20$ANALWC1 <- nsduh20$ANALWTQ1Q4_C
nsduh20<-nsduh20[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","cigyr","CIG100LF","amdetxrx","ahltmde")] # Keep only the variables needed

# Add NSDUH 2019
load("C:/Users/JT936/Dropbox/Analysis/NSDUH/NSDUH_2019.RData")
nsduh19 <- subset(PUF2019_100920, CATAG6 >1)
nsduh19$year <- 2019
nsduh19$ANALWC1 <- nsduh19$ANALWT_C
nsduh19<-nsduh19[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","cigyr","CIG100LF","amdetxrx","ahltmde")] # Keep only the variables needed

nsduh0520 <- rbind(nsduh20, nsduh19)
nsduh0520 <- rbind(nsduh0520, nsduh)

save(nsduh0520, file="nsduhvars_2005-2020.rda")
## Assign MD status based on past year and lifetime reports of MDE

# Current MD = past year MDE
nsduh0520$dep[nsduh0520$amdeyr==1] <- 1
nsduh0520$dep[nsduh0520$amdeyr==2] <- 0

# Former MD = no past year MDE, but lifetime MDE
nsduh0520$notdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==1] <- 1
nsduh0520$notdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==2] <- 0
nsduh0520$notdep[nsduh0520$amdeyr==1] <- 0

# Never MD = no reported lifetime MDE (but may be subject to recall error)
nsduh0520$nevdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==2] <- 1 
nsduh0520$nevdep[nsduh0520$amdeyr==1& nsduh0520$amdelt==1] <- 0
nsduh0520$nevdep[nsduh0520$amdeyr==2 & nsduh0520$amdelt==1] <- 0

# Ever MD = reported lifetime MDE
nsduh0520$everdep[nsduh0520$nevdep==1] <- 0 
nsduh0520$everdep[nsduh0520$nevdep==0] <- 1

# smoked within the past 12 months and at least 100 cigs in lifetime
nsduh0520$currentsmoker[nsduh0520$cigyr==1 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-1
nsduh0520$currentsmoker[nsduh0520$cigyr==0] <-0

# did not smoke within the past 12 months but at least 100 cigs in lifetime
nsduh0520$formersmoker[nsduh0520$cigyr==0 & (nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5)]<-1
nsduh0520$formersmoker[nsduh0520$cigyr==1] <-0
nsduh0520$formersmoker[nsduh0520$CIG100LF==2] <-0

#never smoked 100 cigarettes in lifetime or never smoked at all
nsduh0520$neversmoker[nsduh0520$CIG100LF==2 |nsduh0520$CIG100LF==91] <- 1
nsduh0520$neversmoker[nsduh0520$CIG100LF==1 | nsduh0520$CIG100LF==3 |nsduh0520$CIG100LF==5] <- 0


# SMKDEP categories
nsduh0520$ns_dep[nsduh0520$dep==1 & nsduh0520$neversmoker==1] <- 1
nsduh0520$ns_dep[nsduh0520$dep==1 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_dep[nsduh0520$dep==0 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_dep[nsduh0520$dep==0 & nsduh0520$neversmoker==1] <- 0

nsduh0520$ns_notdep[nsduh0520$notdep==1 & nsduh0520$neversmoker==1] <- 1
nsduh0520$ns_notdep[nsduh0520$notdep==1 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_notdep[nsduh0520$notdep==0 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_notdep[nsduh0520$notdep==0 & nsduh0520$neversmoker==1] <- 0

nsduh0520$ns_nevdep[nsduh0520$nevdep==1 & nsduh0520$neversmoker==1] <- 1
nsduh0520$ns_nevdep[nsduh0520$nevdep==1 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_nevdep[nsduh0520$nevdep==0 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_nevdep[nsduh0520$nevdep==0 & nsduh0520$neversmoker==1] <- 0

nsduh0520$ns_everdep[nsduh0520$everdep==1 & nsduh0520$neversmoker==1] <- 1
nsduh0520$ns_everdep[nsduh0520$everdep==1 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_everdep[nsduh0520$everdep==0 & nsduh0520$neversmoker==0] <- 0
nsduh0520$ns_everdep[nsduh0520$everdep==0 & nsduh0520$neversmoker==1] <- 0

nsduh0520$cs_dep[nsduh0520$dep==1 & nsduh0520$currentsmoker==1] <- 1
nsduh0520$cs_dep[nsduh0520$dep==1 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_dep[nsduh0520$dep==0 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_dep[nsduh0520$dep==0 & nsduh0520$currentsmoker==1] <- 0

nsduh0520$cs_notdep[nsduh0520$notdep==1 & nsduh0520$currentsmoker==1] <- 1
nsduh0520$cs_notdep[nsduh0520$notdep==1 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_notdep[nsduh0520$notdep==0 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_notdep[nsduh0520$notdep==0 & nsduh0520$currentsmoker==1] <- 0

nsduh0520$cs_nevdep[nsduh0520$nevdep==1 & nsduh0520$currentsmoker==1] <- 1
nsduh0520$cs_nevdep[nsduh0520$nevdep==1 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_nevdep[nsduh0520$nevdep==0 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_nevdep[nsduh0520$nevdep==0 & nsduh0520$currentsmoker==1] <- 0

nsduh0520$cs_everdep[nsduh0520$everdep==1 & nsduh0520$currentsmoker==1] <- 1
nsduh0520$cs_everdep[nsduh0520$everdep==1 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_everdep[nsduh0520$everdep==0 & nsduh0520$currentsmoker==0] <- 0
nsduh0520$cs_everdep[nsduh0520$everdep==0 & nsduh0520$currentsmoker==1] <- 0

nsduh0520$fs_dep[nsduh0520$dep==1 & nsduh0520$formersmoker==1] <- 1
nsduh0520$fs_dep[nsduh0520$dep==1 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_dep[nsduh0520$dep==0 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_dep[nsduh0520$dep==0 & nsduh0520$formersmoker==1] <- 0

nsduh0520$fs_notdep[nsduh0520$notdep==1 & nsduh0520$formersmoker==1] <- 1
nsduh0520$fs_notdep[nsduh0520$notdep==1 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_notdep[nsduh0520$notdep==0 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_notdep[nsduh0520$notdep==0 & nsduh0520$formersmoker==1] <- 0

nsduh0520$fs_nevdep[nsduh0520$nevdep==1 & nsduh0520$formersmoker==1] <- 1
nsduh0520$fs_nevdep[nsduh0520$nevdep==1 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_nevdep[nsduh0520$nevdep==0 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_nevdep[nsduh0520$nevdep==0 & nsduh0520$formersmoker==1] <- 0

nsduh0520$fs_everdep[nsduh0520$everdep==1 & nsduh0520$formersmoker==1] <- 1
nsduh0520$fs_everdep[nsduh0520$everdep==1 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_everdep[nsduh0520$everdep==0 & nsduh0520$formersmoker==0] <- 0
nsduh0520$fs_everdep[nsduh0520$everdep==0 & nsduh0520$formersmoker==1] <- 0

save(nsduh0520, file="nsduh2005-2020clean.Rda")
# load("nsduh2005-2020clean.Rda")
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
    alladults  <- rbind(alladults, data.frame(y,"total",groupvar, deparse(substitute(subpop)), prev[1],SE(prev), confint(prev)[1,1], confint(prev)[1,2]))          
    
    agegroupnames <- c("18to25", "26to34","35to49", "50to64","65plus")
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

depsmkprevs_by_year <- NULL
gender <- c("males", "females")

for (x in 1:2){# irsex: 1 = males, 2 = females
  totalpop <- subset(nsduh0520, nsduh0520$irsex==x)
  
  currentsmokers <- subset(nsduh0520, nsduh0520$currentsmoker==1 & nsduh0520$irsex==x) 
  formersmokers <- subset(nsduh0520, nsduh0520$formersmoker==1 & nsduh0520$irsex==x) 
  neversmokers <- subset(nsduh0520, nsduh0520$neversmoker==1 & nsduh0520$irsex==x)
  
  deppop <- subset(nsduh0520, nsduh0520$dep==1 & nsduh0520$irsex==x)
  notdeppop <- subset(nsduh0520, nsduh0520$notdep==1 & nsduh0520$irsex==x)
  nevdeppop <- subset(nsduh0520, nsduh0520$nevdep==1 & nsduh0520$irsex==x)
  everdeppop <- subset(nsduh0520, nsduh0520$everdep==1 & nsduh0520$irsex==x)
  
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("dep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("notdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("nevdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("everdep",totalpop)))

  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("currentsmoker",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("formersmoker",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("neversmoker",totalpop)))

  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("ns_dep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("ns_notdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("ns_nevdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("ns_everdep",totalpop)))

  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("cs_dep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("cs_notdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("cs_nevdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("cs_everdep",totalpop)))

  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("fs_dep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("fs_notdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("fs_nevdep",totalpop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("fs_everdep",totalpop)))
  # 
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("dep",neversmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("notdep",neversmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("nevdep",neversmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("everdep",neversmokers)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("dep",currentsmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("notdep",currentsmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("nevdep",currentsmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("everdep",currentsmokers)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("dep",formersmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("notdep",formersmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("nevdep",formersmokers)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("everdep",formersmokers)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("currentsmoker",deppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("formersmoker",deppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("neversmoker",deppop)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("currentsmoker",notdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("formersmoker",notdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("neversmoker",notdeppop)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("currentsmoker",nevdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("formersmoker",nevdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("neversmoker",nevdeppop)))
  
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("currentsmoker",everdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("formersmoker",everdeppop)))
  depsmkprevs_by_year <- rbind(depsmkprevs_by_year, cbind(gender[x], getprevsbyage("neversmoker",everdeppop)))
  
} 

colnames(depsmkprevs_by_year)[1] <- "gender"
depsmkprevs_by_year$prev=as.numeric(depsmkprevs_by_year$prev)
depsmkprevs_by_year$se=as.numeric(depsmkprevs_by_year$se)
depsmkprevs_by_year$prev_highCI=as.numeric(depsmkprevs_by_year$prev_highCI)
depsmkprevs_by_year$prev_lowCI=as.numeric(depsmkprevs_by_year$prev_lowCI)
depsmkprevs_by_year$group <- paste(depsmkprevs_by_year$gender, depsmkprevs_by_year$status, depsmkprevs_by_year$age, sep="_")

save(depsmkprevs_by_year, file="depsmkprevs_2005-2020.rda")