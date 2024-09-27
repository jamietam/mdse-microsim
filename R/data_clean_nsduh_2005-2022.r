# Set directory where data will be saved
# mainDir <- "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim"
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/"
setwd(file.path(mainDir))

library(dplyr)
library(tidyselect)
library(plyr)

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

load("data-raw/nsduhvars_2005-2022.rda")
nsduh0519 <- subset(nsduh0522,year<=2019)

# Add NSDUH 2020
load("data-raw/NSDUH_2020.RData")
nsduh20 <- NSDUH_2020
nsduh20$year <- 2020
nsduh20$vestr <- nsduh20$VESTRQ1Q4_C
nsduh20$ANALWC1 <- nsduh20$ANALWTQ1Q4_C
nsduh20<- nsduh20 %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol","nic")),contains(c("vap", "preg","K6","SPD","smi","ami","mde","race","edu")))

# Add NSDUH 2021
load("data-raw/NSDUH_2021.RData")
nsduh21 <- PUF2021_121323
nsduh21$year <- 2021
nsduh21$vestr <- nsduh21$VESTR_C
nsduh21$ANALWC1 <- nsduh21$ANALWT2_C
nsduh21 <- nsduh21 %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol")),contains(c("vap", "preg","K6","SPD","smi","ami","mde","race","edu")))

load("data-raw/NSDUH_2022.Rdata")
nsduh22 <- NSDUH_2022
nsduh22$year <- 2022
nsduh22$vestr <- nsduh22$VESTR_C
nsduh22$ANALWC1 <- nsduh22$ANALWT2_C
nsduh22$vapnicevr <- nsduh22$nicvapever
nsduh22$vapnicrec <- nsduh22$nicvaprec
nsduh22$vapnicflag <- nsduh22$nicvapflag
nsduh22$vapnicmon <- nsduh22$nicvapmon
nsduh22$vapnicyr <- nsduh22$nicvapyr

nsduh22$nicvap30n <- nsduh22$NICVAP30N
nsduh22$nicvap30n[nsduh22$nicvap30n>30] <- NA
nsduh22<- nsduh22 %>% select(starts_with(c("cig","AD","CAT","year","ve","ANALWC","ajam","ir","ahlt","yod","yol","nic")),contains(c("vap", "preg","K6","SPD","smi","ami","mde","race","edu")))

nsduh0522 <- rbind.fill(nsduh21, nsduh22, nsduh20,nsduh0519)

nsduh0522<-subset(nsduh0522,CATAG6>1)
nsduh0522<-nsduh0522[c("year", "CATAG6","irsex","amdelt","amdeyr", "vestr","verep","ANALWC1","cigyr","CIG100LF","amdetxrx","ahltmde","cigmon","ircigrc",
"vapnicevr","vapnicrec","vapanyrec","vapanyevr","vapanyflag","vapanyyr","vapanymon","vapnicflag","vapnicyr","vapnicmon", "nicvap30n")] # Keep only the variables needed

save(nsduh0522, file="data-raw/nsduhvars_2005-2022.rda")

# Clean data --------------------------------------------------------------

load("data-raw/nsduhvars_2005-2022.rda")

## ECIG USE - Vaping nicotine or tobacco
nsduh0522$O[nsduh0522$vapnicevr==2 | nsduh0522$vapnicevr==91] <-1
nsduh0522$O[nsduh0522$vapnicevr==1 ] <-0

nsduh0522$evervap[nsduh0522$vapnicevr==2 | nsduh0522$vapnicevr==91] <-0
nsduh0522$evervap[nsduh0522$vapnicevr==1 ] <-1

# vaped nicotine or tobacco, but not in past 30 days
nsduh0522$Q[nsduh0522$evervap==1 &  (nsduh0522$vapnicrec==2 | nsduh0522$vapnicrec==3 | nsduh0522$vapnicrec==9 | nsduh0522$vapnicrec==19)] <-1
nsduh0522$Q[nsduh0522$evervap==1 &  nsduh0522$vapnicrec==1] <- 0
nsduh0522$Q[nsduh0522$evervap==0] <- 0

# vaped nicotine or tobacco in past 30 days
nsduh0522$E[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1] <- 1
nsduh0522$E[nsduh0522$Q==1] <- 0
nsduh0522$E[nsduh0522$evervap==0] <- 0

# vaped nicotine/ecig 5+ days
nsduh0522$vap5[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n>=5] <- 1
nsduh0522$vap5[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n<5] <- 0
nsduh0522$vap5[nsduh0522$Q==1] <- 0
nsduh0522$vap5[nsduh0522$evervap==0] <- 0

#vaped nicotine/ecig 7+ days
nsduh0522$vap7[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n>=7] <- 1
nsduh0522$vap7[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n<7] <- 0
nsduh0522$vap7[nsduh0522$Q==1] <- 0
nsduh0522$vap7[nsduh0522$evervap==0] <- 0

#vaped nicotine/ecig 10+ days
nsduh0522$vap10[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n>=10] <- 1
nsduh0522$vap10[nsduh0522$evervap==1 & nsduh0522$vapnicrec==1 & nsduh0522$nicvap30n<10] <- 0
nsduh0522$vap10[nsduh0522$Q==1] <- 0
nsduh0522$vap10[nsduh0522$evervap==0] <- 0

# cigmon = cigarette smoked within the past 30 days

# dual cig and e-cig(nicotine) within past 30 days
nsduh0522$dual[nsduh0522$cigmon==1 & nsduh0522$E==1] <- 1
nsduh0522$dual[nsduh0522$cigmon==0 & nsduh0522$E==1] <- 0
nsduh0522$dual[nsduh0522$cigmon==1 & nsduh0522$E==0] <- 0
nsduh0522$dual[nsduh0522$cigmon==0 & nsduh0522$E==0] <- 0

# exclusive cig past 30 days, no e-cig nicotine vaping in past 30 days
nsduh0522$exclcig[nsduh0522$cigmon==1 & nsduh0522$E==1] <- 0
nsduh0522$exclcig[nsduh0522$cigmon==0 & nsduh0522$E==1] <- 0
nsduh0522$exclcig[nsduh0522$cigmon==1 & nsduh0522$E==0] <- 1
nsduh0522$exclcig[nsduh0522$cigmon==0 & nsduh0522$E==0] <- 0

# exclusive vape (nicotine) past 30 days, no cig smoked in past 30 days
nsduh0522$exclvap[nsduh0522$cigmon==1 & nsduh0522$E==1] <- 0
nsduh0522$exclvap[nsduh0522$cigmon==0 & nsduh0522$E==1] <- 1
nsduh0522$exclvap[nsduh0522$cigmon==1 & nsduh0522$E==0] <- 0
nsduh0522$exclvap[nsduh0522$cigmon==0 & nsduh0522$E==0] <- 0

# no use of vape (nicotine) OR cig smoked in past 30 days
nsduh0522$neither[nsduh0522$cigmon==1 & nsduh0522$E==1] <- 0
nsduh0522$neither[nsduh0522$cigmon==0 & nsduh0522$E==1] <- 0
nsduh0522$neither[nsduh0522$cigmon==1 & nsduh0522$E==0] <- 0
nsduh0522$neither[nsduh0522$cigmon==0 & nsduh0522$E==0] <- 1

# tobacco product use status for vaping nicotine and smoking cigarettes in past 30 days
nsduh0522$tobstat[nsduh0522$cigmon==1 & nsduh0522$E==1] <- 3 # dual use
nsduh0522$tobstat[nsduh0522$cigmon==0 & nsduh0522$E==1] <- 1 # vaping only
nsduh0522$tobstat[nsduh0522$cigmon==1 & nsduh0522$E==0] <- 2 # smoking only
nsduh0522$tobstat[nsduh0522$cigmon==0 & nsduh0522$E==0] <- 0 # neither

## Assign MD status based on past year and lifetime reports of MD

# Current MD = past year MDE (D)
nsduh0522$D[nsduh0522$amdeyr==1] <- 1
nsduh0522$D[nsduh0522$amdeyr==2] <- 0

# Former MD = no past year MDE, but lifetime MDE (R)
nsduh0522$R[nsduh0522$amdeyr==2 & nsduh0522$amdelt==1] <- 1
nsduh0522$R[nsduh0522$amdeyr==2 & nsduh0522$amdelt==2] <- 0
nsduh0522$R[nsduh0522$amdeyr==1] <- 0

# Never MD = no reported lifetime MDE (but may be subject to recall error) (H)
nsduh0522$H[nsduh0522$amdeyr==2 & nsduh0522$amdelt==2] <- 1
nsduh0522$H[nsduh0522$amdeyr==1& nsduh0522$amdelt==1] <- 0
nsduh0522$H[nsduh0522$amdeyr==2 & nsduh0522$amdelt==1] <- 0

# Ever MD = reported lifetime MDE
nsduh0522$everdep[nsduh0522$H==1] <- 0
nsduh0522$everdep[nsduh0522$H==0] <- 1

# smoked within the past 12 months and at least 100 cigs in lifetime
nsduh0522$C[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)]<-1
nsduh0522$C[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0522$C[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)] <-0
nsduh0522$C[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-0

# did not smoke within the past 12 months but at least 100 cigs in lifetime
nsduh0522$F[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)]<-0
nsduh0522$F[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-0 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0522$F[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)] <-1
nsduh0522$F[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-0

# never smoked 100 cigarettes in lifetime or never smoked at all
nsduh0522$N[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)]<-0
nsduh0522$N[nsduh0522$cigyr==1 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-1 # Has not smoked 100 cigs in life OR Never used cigarettes
nsduh0522$N[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5)] <-0
nsduh0522$N[nsduh0522$cigyr==0 & (nsduh0522$CIG100LF==2 | nsduh0522$CIG100LF==91)]<-1

# ever smoked 100 cigarettes in lifetime
# nsduh0522$eversmk[nsduh0522$CIG100LF==2 |nsduh0522$CIG100LF==91] <- 0
# nsduh0522$eversmk[nsduh0522$CIG100LF==1 | nsduh0522$CIG100LF==3 |nsduh0522$CIG100LF==5] <- 1

# dual states
# never smk and never vap
nsduh0522$NO[nsduh0522$N==1 & nsduh0522$O==1] <- 1
nsduh0522$NO[nsduh0522$N==0 | nsduh0522$O==0] <- 0

# current smk and never vap
nsduh0522$CO[nsduh0522$C==1 & nsduh0522$O==1] <- 1
nsduh0522$CO[nsduh0522$C==0 | nsduh0522$O==0] <- 0

# former smk and never vap
nsduh0522$FO[nsduh0522$F==1 & nsduh0522$O==1] <- 1
nsduh0522$FO[nsduh0522$F==0 | nsduh0522$O==0] <- 0

# never smk and current vap
nsduh0522$NE[nsduh0522$N==1 & nsduh0522$E==1] <- 1
nsduh0522$NE[nsduh0522$N==0 | nsduh0522$E==0] <- 0

# current smk and current vap
nsduh0522$CE[nsduh0522$C==1 & nsduh0522$E==1] <- 1
nsduh0522$CE[nsduh0522$C==0 | nsduh0522$E==0] <- 0

# former smk and current vap
nsduh0522$FE[nsduh0522$F==1 & nsduh0522$E==1] <- 1
nsduh0522$FE[nsduh0522$F==0 | nsduh0522$E==0] <- 0

# never smk and former vap 
nsduh0522$NQ[nsduh0522$N==1 & nsduh0522$Q==1] <- 1
nsduh0522$NQ[nsduh0522$N==0 | nsduh0522$Q==0] <- 0
  
# current smk and former vap
nsduh0522$CQ[nsduh0522$C==1 & nsduh0522$Q==1] <- 1 
nsduh0522$CQ[nsduh0522$C==0 | nsduh0522$Q==0] <- 0

# former smk and former vap
nsduh0522$FQ[nsduh0522$F==1 & nsduh0522$Q==1] <- 1
nsduh0522$FQ[nsduh0522$F==0 | nsduh0522$Q==0] <- 0


save(nsduh0522,file=paste0("data-raw/nsduh2005-2022clean.Rda"))
load("data-raw/nsduh2005-2022clean.Rda")

# Load survey packages
library(survey)
options(survey.lonely.psu="adjust")
nsduh0522 <- nsduh0522[order(nsduh0522$vestr, nsduh0522$verep),] # re-order survey design variables

## Generate age group-specific prevalences and confidence intervals across adult population
getprevsbyage <- function(groupvar, subpop){
  byagegroup = NULL
  alladults = NULL
  for (y in 2005:2022){
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

## Create new dataframe with NSDUH prevalences for model fitting
mdseprevsB <- NULL
totalpop <- nsduh0522
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("N",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("C",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("F",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("E",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("O",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("Q",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("D",totalpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NO",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CO",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FO",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NE",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CE",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FE",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NQ",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CQ",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FQ",totalpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("neither",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclvap",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclcig",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("dual",totalpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap5",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap7",totalpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap10",totalpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",totalpop)))

# population with depression
Dpop <- subset(nsduh0522, nsduh0522$D==1)
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("N",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("C",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("F",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("E",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("O",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("Q",Dpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NO",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CO",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FO",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NE",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CE",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FE",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NQ",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CQ",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FQ",Dpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("neither",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclvap",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclcig",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("dual",Dpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap5",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap7",Dpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap10",Dpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",Dpop)))

# population without depression
notDpop <- subset(nsduh0522, nsduh0522$H==1)
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("N",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("C",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("F",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("E",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("O",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("Q",notDpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NO",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CO",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FO",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NE",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CE",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FE",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("NQ",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("CQ",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("FQ",notDpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("neither",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclvap",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("exclcig",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("dual",notDpop)))

mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap5",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap7",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("vap10",notDpop)))
mdseprevsB<-rbind(mdseprevsB,cbind("both",getprevsbyage("cigmon",notDpop)))

colnames(mdseprevsB)[1] <- "gender"
mdseprevsB$prev=as.numeric(mdseprevsB$prev)
mdseprevsB$se=as.numeric(mdseprevsB$se)
mdseprevsB$prev_highCI=as.numeric(mdseprevsB$prev_highCI)
mdseprevsB$prev_lowCI=as.numeric(mdseprevsB$prev_lowCI)

mdseprevs <- NULL

gender <- c("Men", "Women")
for (x in 1:2){# irsex: 1 = males, 2 = females
  totalpop <- subset(nsduh0522, nsduh0522$irsex==x)
  Dpop <- subset(nsduh0522, nsduh0522$D==1 & nsduh0522$irsex==x)
  notDpop <- subset(nsduh0522, nsduh0522$D==0 & nsduh0522$irsex==x)
  
  # formervapers <- subset(nsduh0522, nsduh0522$Q==1 & nsduh0522$irsex==x)
  # nevervapers <- subset(nsduh0522, nsduh0522$O==1 & nsduh0522$irsex==x)
  # currentvapers <- subset(nsduh0522, nsduh0522$E==1 & nsduh0522$irsex==x)
  # dualusers <- subset(nsduh0522, nsduh0522$dual==1 & nsduh0522$irsex==x)
  # exclvapers <- subset(nsduh0522, nsduh0522$exclvap==1 & nsduh0522$irsex==x)
  # exclsmks <- subset(nsduh0522, nsduh0522$exclcig==1 & nsduh0522$irsex==x)
  # neitherpop <- subset(nsduh0522, nsduh0522$neitherpop==1 & nsduh0522$irsex==x)
  
  # vap5 <- subset(nsduh0522, nsduh0522$vap5==1 & nsduh0522$irsex==x)
  # vap7 <- subset(nsduh0522, nsduh0522$vap7==1 & nsduh0522$irsex==x)
  # vap10 <- subset(nsduh0522, nsduh0522$vap10==1 & nsduh0522$irsex==x)
  
  # NO <- subset(nsduh0522, nsduh0522$NO==1 & nsduh0522$irsex==x)
  # CO <- subset(nsduh0522, nsduh0522$CO==1 & nsduh0522$irsex==x)
  # FO <- subset(nsduh0522, nsduh0522$FO==1 & nsduh0522$irsex==x)
  # NE <- subset(nsduh0522, nsduh0522$NE==1 & nsduh0522$irsex==x)
  # CE <- subset(nsduh0522, nsduh0522$CE==1 & nsduh0522$irsex==x)
  # FE <- subset(nsduh0522, nsduh0522$FE==1 & nsduh0522$irsex==x)
  # NQ <- subset(nsduh0522, nsduh0522$NQ==1 & nsduh0522$irsex==x)
  # CQ <- subset(nsduh0522, nsduh0522$CQ==1 & nsduh0522$irsex==x)
  # FQ <- subset(nsduh0522, nsduh0522$FQ==1 & nsduh0522$irsex==x)
  
  Npop <- subset(nsduh0522, nsduh0522$N==1 & nsduh0522$irsex==x)
  Cpop <- subset(nsduh0522, nsduh0522$C==1 & nsduh0522$irsex==x)
  Fpop <- subset(nsduh0522, nsduh0522$F==1 & nsduh0522$irsex==x)
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("D",Npop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("D",Cpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("D",Fpop)))
  
  ##call getprevsbyage for the vap states under totalpop,Dpop,and Hpop
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("N",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("C",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("F",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("D",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("O",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("E",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("Q",totalpop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("N",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("C",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("F",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("O",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("E",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("Q",Dpop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("N",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("C",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("F",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("O",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("E",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("Q",notDpop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("cigmon",notDpop)))
  
  ##call getprevsbyage for the <30days vap states under totalpop, depop, and Hpop
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap5",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap7",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap10",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap5",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap7",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap10",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap5",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap7",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("vap10",notDpop)))
  
  ##call getprevsbyage for the dual states under totalpop, depop, and Hpop
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NO",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CO",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FO",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NE",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CE",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FE",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NQ",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CQ",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FQ",totalpop)))
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NO",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CO",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FO",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NE",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CE",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FE",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NQ",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CQ",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FQ",Dpop)))
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NO",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CO",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FO",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NE",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CE",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FE",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("NQ",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("CQ",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("FQ",notDpop)))

  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neither",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclvap",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclcig",totalpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dual",totalpop)))
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neither",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclvap",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclcig",Dpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dual",Dpop)))
  
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("neither",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclvap",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("exclcig",notDpop)))
  mdseprevs <- rbind(mdseprevs, cbind(gender[x], getprevsbyage("dual",notDpop)))
  
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

mdseprevs = subset(mdseprevs,!(survey_year<2020 & prev==0.000 & se==0.00)) # remove rows without any e-cig data pre-2020

save(mdseprevs,file=paste0("data/mdseprevs0522.rda"))

load("mdseprevs0522.rda")

# Data visualization and results figures -------------------------------------------------------------------
library(ggplot2)
library(reshape)
library(grid)
library(gridBase)
library(gridExtra)
library(plyr)

xaxisbreaks = seq(2005,2022,1) # specify the ticks on the x-axis of your results plots
date = "Sept2024"

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

mdseprevs$subpopulation[mdseprevs$subpopulation=="Dpop"] <- "Current MDE"
mdseprevs$subpopulation[mdseprevs$subpopulation=="notDpop"] <- "No MDE"
mdseprevs$subpopulation[mdseprevs$subpopulation=="totalpop"] <- "Total"
smkprevbyMDF <- ggplot()+
  geom_pointrange(data= subset(mdseprevs,age==18.25&gender=="Women"&status=="C"&(subpopulation=="Total"|subpopulation=="Current MDE")), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,65),breaks=seq(0,65,5)) +
  labs(title="Women, ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 17),legend.position = c(0.95, 0.02),   legend.justification = c("right", "bottom"),
        legend.box.just = "right",  legend.margin = margin(6, 6, 6, 6),legend.box.background = element_rect(color="black", size=1),)
smkprevbyMDM <- ggplot()+
  geom_pointrange(data= subset(mdseprevs,age==18.25&gender=="Men"&status=="C"&(subpopulation=="Total"|subpopulation=="Current MDE")), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
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
  geom_pointrange(data= subset(mdseprevs,age==18.99&subpopulation=="Total"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,26),breaks=seq(0,26,2)) +
  labs(title="Current MD, Ages 18+")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev18.25 <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.25&subpopulation=="Total"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,26),breaks=seq(0,26,2)) +
  labs(title="Current MD, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 14))
MDprev18.25cs <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.25&subpopulation=="Cpop"&status=="dep"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MDE prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
  labs(title="Current smoking, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))
MDprev18.25fs <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.25&subpopulation=="Fpop"&status=="D"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MDE prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
  labs(title="Former smoking, Ages 18-25")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))
MDprev18.25ns <-ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.25&subpopulation=="Npop"&status=="D"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
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
  geom_pointrange(data= subset(mdseprevs,age==18.99&subpopulation=="Cpop"&status=="D"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Current smoking")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev_fspop<- ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.99&subpopulation=="Fpop"&status=="D"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Former smoking")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

MDprev_nspop<- ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.99&subpopulation=="Npop"&status=="D"), aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=gender))+
  scale_y_continuous(name="MD Prevalence (%)",limits=c(0,22),breaks=seq(0,22,2)) +
  labs(title="Never smoking")+
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

jpeg(filename = paste0("MDprev_smkstat","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(MDprev_nspop,MDprev_cspop, MDprev_fspop),3,"")
dev.off()

smkprevF<- ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.99&
                                 (subpopulation=="Current MDE"|subpopulation=="Never MDE")&status=="C"&gender=="Women"), 
                  aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,55),breaks=seq(0,55,5)) +
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  labs(title="Smoking prevalence (Past Year), Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

smkprevM <- ggplot() + 
  geom_pointrange(data= subset(mdseprevs,age==18.99&
                                 (subpopulation=="Current MDE"|subpopulation=="Never MDE")&status=="C"&gender=="Men"), 
                  aes(x = survey_year, y = prev*100,ymin=prev_lowCI*100, ymax=prev_highCI*100,color=subpopulation))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,55),breaks=seq(0,55,5)) +
  scale_x_continuous(name="Year",limits=c(min(xaxisbreaks),max(xaxisbreaks)),breaks=xaxisbreaks)  +
  labs(title="Smoking prevalence (Past Year), Men")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), text = element_text(size = 20))

jpeg(filename = paste0("smkprevbyMD","_" ,date, ".jpg"),width=16, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(smkprevF,smkprevM),2,"")
dev.off()

# Past 30 day tobacco use by MD status, 2020-2022
bardata <- subset(mdseprevs, age==18.99 & survey_year >=2020 & (status=="dual" | status=="exclcig" | status=="exclvap" | status=="neither") & (subpopulation=="Current MDE" | subpopulation=="Never MDE" | subpopulation=="Total"))
bardata$subpopulation=as.factor(bardata$subpopulation)
levels(bardata$subpopulation) <- c("Current MDE","Never MDE", "Total")
bardata$status=as.factor(bardata$status)
levels(bardata$status) <- c("Dual use","Exclusive cig", "Exclusive e-cig", "Neither")
bardata$status = factor(bardata$status, levels=c("Neither","Exclusive e-cig","Exclusive cig","Dual use")) # reorder factor levels
group.colors <- c("Dual use" = "#0000FF", "Exclusive cig" = "#000000", "Exclusive e-cig" ="#00CCFF", "Neither" = "#CCCCCC")
bardata$prev = round(bardata$prev*100,1)

tobaccostat<-ggplot(data=subset(bardata,gender=="both")) + 
  geom_col(aes(x=as.factor(survey_year),y=prev,fill=status))+
  facet_wrap(~subpopulation)+
  scale_fill_manual(values=group.colors)+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,101),breaks=seq(0,100,5)) +
  labs(title="Past 30 day tobacco use, NSDUH 2020-2022")+xlab("Population")+
  theme(axis.title.x = element_blank(),legend.title = element_blank(), text = element_text(size = 15))+
  geom_label(position=position_stack(vjust=0.5), aes(x=as.factor(survey_year),y=prev, group=status, label=prev))

jpeg(filename = paste0("tobstatbyMDE","_" ,date, ".jpg"),width=6, height=6, units ="in", res=1000)
tobaccostat
dev.off()       

