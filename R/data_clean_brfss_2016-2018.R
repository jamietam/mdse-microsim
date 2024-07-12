rm(list = ls()) 
library(tidyverse)
library(survey)
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
setwd(file.path(mainDir))

#load in brfss2016-2018vars
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/BRFSS2018vars.Rda")
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/brfss2017vars.Rda")
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/brfss2016vars.Rda")

##function to process states
process_brfss <- function(brfssvars, ogdataset) {
  # Add smoking variable
  brfssvars$SMOKDAY2 <- ogdataset$SMOKDAY2
  
  #add depression vars andcheck to see if ogdataset contains these variables
  if ("MENTHLTH" %in% names(ogdataset)) {
    brfssvars$MENTHLTH <- ogdataset$MENTHLTH
    #deppop <- rbind(deppop, subset(brfssvars, brfssvars$MENTHLTH <= 30))
  }
  if ("ADPLEAS1" %in% names(ogdataset)) {
    brfssvars$ADPLEAS1 <- ogdataset$ADPLEAS1
    brfssvars$ADPLEAS1[brfssvars$ADPLEAS1 > 4] <- NA
    #deppop <- rbind(deppop, subset(brfssvars$ADPLEAS1 > 1 & brfssvars$ADPLEAS1 < 5))
  }
  if ("ADDOWN1" %in% names(ogdataset)) {
    brfssvars$ADDOWN1 <- ogdataset$ADDOWN1
    brfssvars$ADDOWN1[brfssvars$ADDOWN1 > 4] <- NA
    brfssvars$PHQ2 <- brfssvars$ADPLEAS1 - 1 + brfssvars$ADDOWN1 -1 #subset when phq2 >=3
    #deppop <- rbind(deppop, subset(brfssvars$ADDOWN1 > 1 & brfssvars$ADDOWN1 < 5))
  }
  
  # Total population
  totalpop <- brfssvars
  
  # Vaping and smoking status
  brfssvars$O[brfssvars$ECIGARET== 2] <- 1
  brfssvars$O[brfssvars$ECIGARET== 1] <- 0
  brfssvars$O[brfssvars$ECIGARET>2] <- NA
  
  brfssvars$E[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW <= 2] <- 1
  brfssvars$E[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3] <- 0
  brfssvars$E[brfssvars$ECIGNOW > 3] <- NA
  
  brfssvars$Q[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1] <- 1
  brfssvars$Q[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3] <- 0
  brfssvars$Q[brfssvars$ECIGNOW > 3 | brfssvars$ECIGARET > 2] <- NA
  
  brfssvars$N[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4] <- 1
  brfssvars$N[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3] <- 0
  brfssvars$N[brfssvars$SMOKDAY2 < 3] <- 0
  
  brfssvars$C[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2] <- 1
  brfssvars$C[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
  brfssvars$F[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3] <- 1
  brfssvars$F[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
  brfssvars$NO[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & (brfssvars$ECIGARET== 2)] <- 1
  brfssvars$NO[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & (brfssvars$ECIGARET== 1)] <- 0
  
  brfssvars$NE[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$NE[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$NQ[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$NQ[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & (brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3)] <- 0
  
  brfssvars$CO[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & (brfssvars$ECIGARET== 2)] <- 1
  brfssvars$CO[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$CE[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$CE[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3)] <- 0
  
  brfssvars$CQ[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$CQ[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3)] <- 0
  
  brfssvars$FO[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & (brfssvars$ECIGARET == 2)] <- 1
  brfssvars$FO[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$FE[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$FE[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3)] <- 0
  
  brfssvars$FQ[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$FQ[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3)] <- 0
  
  return(list(totalpop = totalpop, brfssvars = brfssvars))
}

#load in og data sets (brfss2018,brfss2017,brfss2016)
load("~/GitHub/mds-microsim/R/LLCP2018.Rda")
load("~/GitHub/mds-microsim/R/LLCP2017.Rda")
load("~/GitHub/mds-microsim/R/LLCP2016.Rda")

#process states and extract data for each year
brfss18 <- brfss2018vars
result18 <- process_brfss(brfss18, brfss2018)
brfss18 <- NULL
brfss18 <- result18$brfssvars
totalpop18 <- result18$totalpop
#deppop18 <- result18$deppop

brfss17 <- brfss2017vars
result17 <- process_brfss(brfss17, brfss2017)
brfss17 <- NULL
brfss17 <- result17$brfssvars
totalpop17 <- result17$totalpop
#deppop17 <- result17$deppop

brfss16 <- brfss2016vars
result16 <- process_brfss(brfss16, brfss2016)
brfss16 <- NULL
brfss16 <- result16$brfssvars
totalpop16 <- result16$totalpop
#deppop16 <- result16$deppop

#create column of single states (1:6 = N,C,F,O,E,Q)
singlestates <- function(brfss){
  states <- c("N","C","F","O","E","Q")
  for(i in seq_along(states)){
    brfss$singlestates[brfss[[states[i]]] == 1] <- i
  }
  return(brfss)
}
totalpop18 <- singlestates(brfss18)
totalpop17 <- singlestates(brfss17)
totalpop16 <- singlestates(brfss16)

#create column of combined states (1:9 = NO:FQ)
get_states <- function(brfss){
  states <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
  for (i in seq_along(states)) {
    brfss$states[brfss[[states[i]]] == 1] <- i
  }
  return(brfss)
}
totalpop18 <- get_states(brfss18)
totalpop17 <- get_states(brfss17)
totalpop16 <- get_states(brfss16)


#generate prevalences
options(survey.lonely.psu = "adjust")
design16total <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = totalpop16)
design16mh <- subset(design16total, MENTHLTH <= 30)
design16addep <- subset(design16total, ADDEPEV2 == 1)
design17total <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = totalpop17)
design17mh <- subset(design17total, MENTHLTH <= 30)
design17addep <- subset(design17total, ADDEPEV2 == 1)
design18total <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = totalpop18)
design18mh <- subset(design18total, MENTHLTH <= 30)
design18addep <- subset(design18total, ADDEPEV2 == 1)
design18phq <- subset(design18total, PHQ2 >= 3)

#Smoking prevalences
svyciprop(~X_RFSMOK3==2, design=design16, method="mean",level=0.95,na.rm=TRUE)
svyciprop(~X_RFSMOK3==2, design=design17, method="mean",level=0.95,na.rm=TRUE)
svyciprop(~X_RFSMOK3==2, design=design18, method="mean",level=0.95,na.rm=TRUE)

#Vaping prevalences
svyciprop(~X_CURECIG==2, design=design18, method="mean",level=0.95,na.rm=TRUE)
svyciprop(~X_CURECIG==2, design=design17, method="mean",level=0.95,na.rm=TRUE)
svyciprop(~X_CURECIG==2, design=design16, method="mean",level=0.95,na.rm=TRUE)


##eventually generate prevalences for every state

depstat = c("dep","nodep")
gender = c("males","females")
statenames <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
singstatenames <- c("N","C","F","O","E","Q")
#tobcatlabel = c("neither","exclcig","exclvap","dual")
years = c(2016,2016,2016,2017,2017,2017,2018,2018,2018,2018)
subgroup <- c("totalpop", "mhdays","addepev","totalpop","mhdays","addepev","totalpop","mhdays","addepev","phq2")
svys = list(design16total,design16mh,design16addep,design17total,design17mh,design17addep,design18total,design18mh,design18addep,design18phq)
brfss_smkecigdep <- NULL
for (d in c(1:10)){ # for each survey year
  thissvydesign=svys[[d]]
  year = years[d]
  for (i in c(1:9)){ # tobacco+ecig use category
    for (s in c(1:2)){ # gender status 
        prop.ci = svyciprop(~states==i, design=subset(thissvydesign, SEX==s), method="mean",level=0.95,na.rm=TRUE) 
        newrow <- cbind(years[d], gender[s], depstat[s], statenames[i],subgroup[d], round(prop.ci[1]*100,2), round(SE(prop.ci)*100,2), round(attr(prop.ci, "ci")[1]*100,2), round(attr(prop.ci, "ci")[2]*100,2))
        brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow) # append table with new row of data
    } 
    prop.ci2 = svyciprop(~states==i, design=thissvydesign, method="mean",level=0.95,na.rm=TRUE)
    newrow2 <- cbind(years[d],"both", depstat[s], statenames[i], subgroup[d], round(prop.ci2[1]*100,2),round(SE(prop.ci2)*100,2), round(attr(prop.ci2, "ci")[1]*100,2), round(attr(prop.ci2, "ci")[2]*100,2))
    brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow2) # append table with new row of data
  }
  for(j in c(1:6)){ # single state ecig use categories
    for (s in c(1:2)){ # gender status
      prop.ci = svyciprop(~singlestates==j, design=subset(thissvydesign, SEX==s), method="mean",level=0.95,na.rm=TRUE) 
      newrow <- cbind(years[d], gender[s], depstat[s], singstatenames[j],subgroup[d], round(prop.ci[1]*100,2), round(SE(prop.ci)*100,2), round(attr(prop.ci, "ci")[1]*100,2), round(attr(prop.ci, "ci")[2]*100,2))
      brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow) # append table with new row of data
    } 
    prop.ci2 = svyciprop(~singlestates==j, design=thissvydesign, method="mean",level=0.95,na.rm=TRUE)
    newrow2 <- cbind(years[d],"both", depstat[s], singstatenames[j], subgroup[d], round(prop.ci2[1]*100,2),round(SE(prop.ci2)*100,2), round(attr(prop.ci2, "ci")[1]*100,2), round(attr(prop.ci2, "ci")[2]*100,2))
    brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow2) # append table with new row of data
  }
}
colnames(brfss_smkecigdep) <- c("year","gender","depstatus","states","subgroup", "prev","stderr","lowCIprev","highCIprev")

