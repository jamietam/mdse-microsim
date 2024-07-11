rm(list = ls()) 
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
setwd(file.path(mainDir))

#load in brfss2016-2018vars
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/BRFSS2018vars.Rda")
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/brfss2017vars.Rda")
load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/brfss2016vars.Rda")

##function to process states
process_brfss <- function(brfssvars, ogdataset) {
  # Add depression variables
  brfssvars$MENTHLTH <- ogdataset$MENTHLTH
  brfssvars$ADPLEAS1 <- ogdataset$ADPLEAS1
  brfssvars$CRGVPRB2 <- ogdataset$CRGVPRB2
  brfssvars$ADDOWN1 <- ogdataset$ADDOWN1
  
  # Add smoking variable
  brfssvars$SMOKDAY2 <- ogdataset$SMOKDAY2
  
  #debug
  # print("variables added:")
  # print(names(brfssvars))
  
  # Total population
  totalpop <- brfssvars
  
  # Depression population
  deppop <- subset(brfssvars, (brfssvars$ADPLEAS1 > 1 & brfssvars$ADPLEAS1 < 5) | brfssvars$MENTHLTH <= 30 | brfssvars$CRGVPRB2 == 10 | (brfssvars$ADDOWN1 > 1 & brfssvars$ADDOWN1 < 5))
  
  ecigaret2 <- subset(brfss2017vars, brfss2017vars$ECIGARET==2)
  # Vaping and smoking status
  brfssvars$O[brfssvars$ECIGARET== 2] <- 1
  brfssvars$O[brfssvars$ECIGARET== 1] <- 0
  
  brfssvars$E[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW <= 2] <- 1
  brfssvars$E[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3] <- 0
  
  brfssvars$Q[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1] <- 1
  brfssvars$Q[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3] <- 0
  
  brfssvars$Never[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4] <- 1
  brfssvars$Never[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3] <- 0
  brfssvars$Never[brfssvars$SMOKDAY2 < 3] <- 0
  
  brfssvars$Current[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2] <- 1
  brfssvars$Current[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
  brfssvars$Former[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3] <- 1
  brfssvars$Former[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
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
  
  return(list(totalpop = totalpop, deppop = deppop, brfssvars = brfssvars))
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
deppop18 <- result18$deppop

brfss17 <- brfss2017vars
result17 <- process_brfss(brfss17, brfss2017)
brfss17 <- NULL
brfss17 <- result17$brfssvars
totalpop17 <- result17$totalpop
deppop17 <- result17$deppop

brfss16 <- brfss2016vars
result16 <- process_brfss(brfss16, brfss2016)
brfss16 <- NULL
brfss16 <- result16$brfssvars
totalpop16 <- result16$totalpop
deppop16 <- result16$deppop

#create column of states (1:9 = NO:FQ)
get_states <- function(brfss){
  states <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
  for (i in seq_along(states)) {
    brfss$states[brfss[[states[i]]] == 1] <- i
  }
  return(brfss)
}
brfss18 <- get_states(brfss18)
brfss17 <- get_states(brfss17)
brfss16 <- get_states(brfss16)


#generate prevalences
library(survey)
options(survey.lonely.psu = "adjust")
design16 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss16)
design17 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss17)
design18 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss18)

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
states <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
#tobcatlabel = c("neither","exclcig","exclvap","dual")
years = 2016:2018
svys = list(design16, design17, design18)
brfss_smkecigdep <- NULL
for (d in c(1:3)){ # for each survey year
  thissvydesign=svys[[d]]
  year = years[d]
  for (i in c(1:9)){ # tobacco use category
    for (s in c(1:2)){ 
      prop.ci = svyciprop(~states==i, design=subset(thissvydesign,(ADDEPEV2==s & SEX==s)), method="mean",level=0.95,na.rm=TRUE)
      newrow <- cbind(years[d], gender[s], depstat[s], states[i], round(prop.ci[1]*100,2), round(SE(prop.ci)*100,2), round(attr(prop.ci, "ci")[1]*100,2), round(attr(prop.ci, "ci")[2]*100,2))
      brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow) # append table with new row of data
    } 
    prop.ci2 = svyciprop(~states==i, design=subset(thissvydesign,ADDEPEV2==s), method="mean",level=0.95,na.rm=TRUE)
    newrow2 <- cbind(years[d],"both", depstat[s], states[i],round(prop.ci2[1]*100,2),round(SE(prop.ci2)*100,2), round(attr(prop.ci2, "ci")[1]*100,2), round(attr(prop.ci2, "ci")[2]*100,2))
    brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow2) # append table with new row of data
  }
}
colnames(brfss_smkecigdep) <- c("year", "gender","depstatus", "states","prev", "stderr", "lowCIprev","highCIprev")





