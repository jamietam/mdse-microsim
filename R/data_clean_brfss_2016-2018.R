mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
setwd(file.path(mainDir))

load("C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/BRFSS2018vars.Rda")

#brfss 2018
brfss18 <- brfss2018vars

#add depression variables
brfss18$MENTHLTH <- brfss2018$MENTHLTH
brfss18$ADPLEAS1 <- brfss2018$ADPLEAS1
brfss18$CRGVPRB2 <- brfss2018$CRGVPRB2
brfss18$ADDOWN1 <- brfss2018$ADDOWN1

#totalpop
totalpop <- brfss18
#deppop
deppop <- subset(brfss18, (brfss18$ADPLEAS1 > 1 & brfss18$ADPLEAS1 < 5) | brfss18$MENTHLTH <= 30 | brfss18$CRGVPRB2 == 10 | (brfss2018$ADDOWN1>1& brfss2018$ADDOWN1<5))

#never vapers, still uncertain of _ECIGSTS variable definition
brfss18$O[(brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3) | brfss18$ECIGARET == 2] <- 1
brfss18$O[(brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3) | brfss18$ECIGARET == 1] <- 0

#current vapers
brfss18$E[(brfss18$X_CURECIG == 2 & (brfss18$ECIGNOW == 1 | brfss18$ECIGNOW == 2)) & brfss18$ECIGARET == 1] <- 1
brfss18$E[brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3] <- 0

#former vapers
brfss18$Q[brfss18$X_CURECIG == 1 & brfss18$ECIGNOW == 3 & brfss18$ECIGARET == 1] <- 1
brfss18$Q[brfss18$X_CURECIG == 2 & brfss18$ECIGNOW < 3] <- 0
brfss18$Q[brfss18$ECIGARET == 2] <- 0

#never smokers
brfss18$N[brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 4] <- 1
brfss18$N[brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 3] <- 0

#current smokers
brfss18$C[brfss18$X_RFSMOK3 == 2 & brfss18$X_SMOKER3 < 2] <- 1
brfss18$C[brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 == 3 | brfss18$X_SMOKER3 == 4)] <- 0

#former smokers
brfss18$F[brfss18$X_RFSMOK3 == 1 & brfss18$X_SMOKER3 == 3] <- 1
brfss18$F[brfss18$X_RFSMOK3 == 1 & (brfss18$X_SMOKER3 < 3 | brfss18$X_SMOKER3 == 4)] <- 0

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
#FE state
#FQ state
