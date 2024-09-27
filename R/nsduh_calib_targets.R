# Create list of calibration targets
#mainDir = "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/"
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/"
setwd(file.path(mainDir))

load("data/mdseprevs0522.rda")

mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "females"

# mds_microsim targets
l.calib_targets <- vector(mode = "list")
l.calib_targets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="N" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="C" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="F" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$D <- as.matrix(subset(mdseprevs, sex==whichgender & status=="D" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$ND <- as.matrix(subset(mdseprevs, sex==whichgender & status=="N" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$CD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="C" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$FD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="F" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])

# targets for NO, CO, FO, NE, CE, FE, NQ, CQ, FQ
l.calib_targets$NO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$CO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$FO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$NE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$CE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$FE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$NQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$CQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$FQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])

# targets for OD, ED, QD
# OD = never vaping among people with depression
# ED = current vaping among people with depression
# QD = former vaping among people with depression
l.calib_targets$OD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="O" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$ED <- as.matrix(subset(mdseprevs, sex==whichgender & status=="E" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$QD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="Q" & subpopulation=="Dpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

# vap_microsim targets
l.calib_targets$O <- as.matrix(subset(mdseprevs, sex==whichgender & status=="O" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$E <- as.matrix(subset(mdseprevs, sex==whichgender & status=="E" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$Q <- as.matrix(subset(mdseprevs, sex==whichgender & status=="Q" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

# vaping targets for those who vaped past 5+,7+,10+ days 
l.calib_targets$vap5 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap5" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$vap7 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap7" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$vap10 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap10" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

# targets for exclcig, exclvap, dual, and neither
l.calib_targets$nsduhexclcig <- as.matrix(subset(mdseprevs, sex==whichgender & status=="exclcig" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$nsduhexclvap <- as.matrix(subset(mdseprevs, sex==whichgender & status=="exclvap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$nsduhneither <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neither" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$nsduhdual <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dual" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

for (target in names(l.calib_targets)){
  rownames(l.calib_targets[[target]]) <- NULL # remove row names in each matrix
}

save(l.calib_targets,file=paste0("data/nsduh_calib_targets_",whichgender,".RData"))
