rm(list = ls()) 
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
setwd(file.path(mainDir))

load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/BRFSS2018vars.Rda")
load("~/GitHub/mds-microsim/R/LLCP2018.Rda")
load("~/GitHub/mds-microsim/R/LLCP2017.Rda")
load("~/GitHub/mds-microsim/R/LLCP2016.Rda")


#brfss 2018
brfss18 <- brfss2018vars

#add depression variables
brfss18$MENTHLTH <- brfss2018$MENTHLTH
brfss18$ADPLEAS1 <- brfss2018$ADPLEAS1
brfss18$CRGVPRB2 <- brfss2018$CRGVPRB2
brfss18$ADDOWN1 <- brfss2018$ADDOWN1

#add smoking variable
brfss18$SMOKDAY2 <- brfss2018$SMOKDAY2

#totalpop
totalpop <- brfss18
#deppop
deppop <- subset(brfss18, (brfss18$ADPLEAS1 > 1 & brfss18$ADPLEAS1 < 5) | brfss18$MENTHLTH <= 30 | brfss18$CRGVPRB2 == 10 | (brfss2018$ADDOWN1>1& brfss2018$ADDOWN1<5))

#never vapers, still uncertain of _ECIGSTS variable definition
brfss18$O[(brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3) | brfss18$ECIGARET == 2] <- 1
brfss18$O[(brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1] <- 0

#current vapers
brfss18$E[brfss18$X_CURECIG == 2 & brfss18$ECIGNOW <= 2] <- 1
brfss18$E[brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3] <- 0

#former vapers
brfss18$Q[brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3 & brfss18$ECIGARET == 1] <- 1
brfss18$Q[brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3] <- 0
brfss18$Q[brfss18$ECIGARET == 2] <- 0

#never smokers
brfss18$Never[brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 4] <- 1
brfss18$Never[brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 3] <- 0
brfss18$Never[brfss18$SMOKDAY2 < 3] <- 0

#current smokers
brfss18$Current[brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 2] <- 1
brfss18$Current[brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 == 3 | brfss18$X_SMOKER3 == 4)] <- 0

#former smokers
brfss18$Former[brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 3] <- 1
brfss18$Former[brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 < 3 | brfss18$X_SMOKER3 == 4)] <- 0

#NO state
brfss18$NO[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 4) & ((brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3) | brfss18$ECIGARET == 2)] <- 1
brfss18$NO[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 3) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1)] <- 0
#NE state
brfss18$NE[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 4) & ((brfss18$X_CURECIG == 2 & (brfss18$ECIGNOW == 1 | brfss18$ECIGNOW == 2)) & brfss18$ECIGARET == 1)] <- 1
brfss18$NE[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 3) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1)] <- 0
#NQ state
brfss18$NQ[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 4) & (brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3 & brfss18$ECIGARET == 1)] <- 1
brfss18$NQ[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 3) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 2)] <- 0
#CO state
brfss18$CO[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 2) & ((brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3) | brfss18$ECIGARET == 2)] <- 1
brfss18$CO[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 == 3 | brfss18$X_SMOKER3 == 4)) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1)] <- 0
#CE state
brfss18$CE[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 2) & ((brfss18$X_CURECIG == 2 & (brfss18$ECIGNOW == 1 | brfss18$ECIGNOW == 2)) & brfss18$ECIGARET == 1)] <- 1
brfss18$CE[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 == 3 | brfss18$X_SMOKER3 == 4)) & (brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3)] <- 0
#CQ state
brfss18$CQ[(brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 2) & (brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3 & brfss18$ECIGARET == 1)] <- 1
brfss18$CQ[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 == 3 | brfss18$X_SMOKER3 == 4)) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 2)] <- 0
#FO state
brfss18$FO[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 3) & ((brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3) | brfss18$ECIGARET == 2)] <- 1
brfss18$FO[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 < 3 | brfss18$X_SMOKER3 == 4)) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1)] <- 0
#FE state
brfss18$FE[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 3) & ((brfss18$X_CURECIG == 2 & (brfss18$ECIGNOW == 1 | brfss18$ECIGNOW == 2)) & brfss18$ECIGARET == 1)] <- 1
brfss18$FE[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 < 3 | brfss18$X_SMOKER3 == 4)) & (brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3)] <- 0
#FQ state
brfss18$FQ[(brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 3) & (brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3 & brfss18$ECIGARET == 1)] <- 1
brfss18$FQ[(brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 < 3 | brfss18$X_SMOKER3 == 4)) & ((brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 2)] <- 0


##function to process states
process_brfss <- function(brfssvars, year) {
  # Add depression variables
  brfssvars$MENTHLTH <- brfssvars$MENTHLTH
  brfssvars$ADPLEAS1 <- brfssvars$ADPLEAS1
  brfssvars$CRGVPRB2 <- brfssvars$CRGVPRB2
  brfssvars$ADDOWN1 <- brfssvars$ADDOWN1
  
  # Add smoking variable
  brfssvars$SMOKDAY2 <- brfssvars$SMOKDAY2
  
  # Total population
  totalpop <- brfssvars
  
  # Depression population
  deppop <- subset(brfssvars, (brfssvars$ADPLEAS1 > 1 & brfssvars$ADPLEAS1 < 5) | brfssvars$MENTHLTH <= 30 | brfssvars$CRGVPRB2 == 10 | (brfssvars$ADDOWN1 > 1 & brfssvars$ADDOWN1 < 5))
  
  # Vaping and smoking status
  brfssvars$O[(brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3) | brfssvars$ECIGARET == 2] <- 1
  brfssvars$O[(brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1] <- 0
  
  brfssvars$E[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW <= 2] <- 1
  brfssvars$E[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3] <- 0
  
  brfssvars$Q[brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1] <- 1
  brfssvars$Q[brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3] <- 0
  brfssvars$Q[brfssvars$ECIGARET == 2] <- 0
  
  brfssvars$Never[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4] <- 1
  brfssvars$Never[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3] <- 0
  brfssvars$Never[brfssvars$SMOKDAY2 < 3] <- 0
  
  brfssvars$Current[brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2] <- 1
  brfssvars$Current[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
  brfssvars$Former[brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3] <- 1
  brfssvars$Former[brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)] <- 0
  
  brfssvars$NO[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & ((brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3) | brfssvars$ECIGARET == 2)] <- 1
  brfssvars$NO[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$NE[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$NE[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$NQ[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 4) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$NQ[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 3) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 2)] <- 0
  
  brfssvars$CO[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & ((brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3) | brfssvars$ECIGARET == 2)] <- 1
  brfssvars$CO[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$CE[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$CE[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3)] <- 0
  
  brfssvars$CQ[(brfssvars$X_RFSMOK3 == 2 & brfssvars$X_SMOKER3 < 2) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$CQ[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 == 3 | brfssvars$X_SMOKER3 == 4)) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 2)] <- 0
  
  brfssvars$FO[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & ((brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3) | brfssvars$ECIGARET == 2)] <- 1
  brfssvars$FO[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 1)] <- 0
  
  brfssvars$FE[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & ((brfssvars$X_CURECIG == 2 & (brfssvars$ECIGNOW == 1 | brfssvars$ECIGNOW == 2)) & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$FE[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3)] <- 0
  
  brfssvars$FQ[(brfssvars$X_RFSMOK3 == 1 & brfssvars$X_SMOKER3 == 3) & (brfssvars$X_CURECIG == 1 & brfssvars$ECIGNOW == 3 & brfssvars$ECIGARET == 1)] <- 1
  brfssvars$FQ[(brfssvars$X_RFSMOK3 == 1 & (brfssvars$X_SMOKER3 < 3 | brfssvars$X_SMOKER3 == 4)) & ((brfssvars$X_CURECIG == 2 & brfssvars$ECIGNOW < 3) | brfssvars$ECIGARET == 2)] <- 0
  
  return(list(totalpop = totalpop, deppop = deppop, brfssvars = brfssvars))
}
#process states and extract data for each year
brfss18 <- brfss2018vars
result18 <- process_brfss(brfss18, 2018)
brfss18 <- NULL
brfss18 <- result18$brfssvars
totalpop18 <- result18$totalpop
deppop18 <- result18$deppop

brfss17 <- brfss2017vars
result17 <- process_brfss(brfss17, 2017)
brfss17 <- NULL
brfss17 <- result17$brfssvars
totalpop17 <- result17$totalpop
deppop17 <- result17$deppop

brfss16 <- brfss2016vars
result16 <- process_brfss(brfss16, 2016)
brfss16 <- NULL
brfss16 <- result16$brfssvars
totalpop16 <- result16$totalpop
deppop16 <- result16$deppop




load("~/GitHub/mds-microsim/data/BRFSS2018vars.Rda")
load("~/GitHub/mds-microsim/data/brfss2017vars.Rda")
load("~/GitHub/mds-microsim/data/brfss2016vars.Rda")


## Generate age group-specific prevalences and confidence intervals across adult population
install.packages("survey")
library(survey)
getprevsbyage <- function(groupvar, subpop){
  byagegroup = NULL
  alladults = NULL
  for (y in 2016:2018){
    svy <-svydesign(id=~verep, strata=~vestr, nest=TRUE, weights=~ANALWC1, data = subpop)
    
    prev <-svymean(as.formula(paste("~",groupvar)),design=svy,na.rm=TRUE) 
    alladults  <- rbind(alladults, data.frame(y,18.99,groupvar, deparse(substitute(subpop)), prev[1],SE(prev), confint(prev)[1,1], confint(prev)[1,2]))          
    
    agegroupnames <- c(18.24, 25.34,35.44, 45.54, 55.64, 65.99)
    for (k in 1:6){
      prev <- svymean(as.formula(paste("~",groupvar)),design=subset(svy,X_AGE_G==k),na.rm=TRUE) # 
      byagegroup <- rbind(byagegroup, data.frame(y,agegroupnames[k],groupvar, deparse(substitute(subpop)), prev[1],SE(prev),confint(prev)[1,1], confint(prev)[1,2]))          
    }
    
  }
  names(alladults) <- names(byagegroup)
  byagegroup <- rbind(alladults, byagegroup)
  colnames(byagegroup) <- c("survey_year","age","status","subpopulation","prev","se", "prev_lowCI","prev_highCI")
  return(byagegroup)
}

#get prevalences by gender data frames
for (x in 1:2){ #1 is male, #2 is female
  totalpop <- subset(totalpop, totalpop$irsex==x)
  deppop <- subset(deppop, deppop$irsex==x)
}




