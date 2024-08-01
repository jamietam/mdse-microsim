# Create list of calibration targets
mainDir = "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/"
setwd(file.path(mainDir))

load("C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/data/mdseprevs0522.rda")

# drop 18.99 age group
mdseprevs <- subset(mdseprevs, age!=18.99)
mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "females"

# smk_microsim targets
lst_smktargets <- vector(mode = "list")
lst_smktargets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_smktargets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_smktargets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])

save(lst_smktargets,file=paste0("smk_calib_targets_",whichgender,".RData"))

# dep_microsim targets
lst_deptargets <- vector(mode = "list")
lst_deptargets$H <- as.matrix(subset(mdseprevs, sex==whichgender & status=="nevdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_deptargets$D <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$R <- as.matrix(subset(mdseprevs, sex==whichgender & status=="fdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$E <- as.matrix(subset(mdseprevs, sex==whichgender & status=="everdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

save(lst_deptargets,file=paste0("dep_calib_targets_",whichgender,".RData"))

# targets for NO, CO, FO, NE, CE, FE, NQ, CQ, FQ
lst_dualtargets <- vector(mode = "list")
lst_dualtargets$NO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_dualtargets$CO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_dualtargets$FO <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FO" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_dualtargets$NE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_dualtargets$CE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_dualtargets$FE <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FE" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_dualtargets$NQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="NQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_dualtargets$CQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="CQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_dualtargets$FQ <- as.matrix(subset(mdseprevs, sex==whichgender & status=="FQ" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
directory <- "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/data/"
save(lst_dualtargets,file=paste0(directory, "dual_calib_targets_",whichgender,".RData"))

# targets for OD, ED, QD
lst_vapdeptargets <- vector(mode = "list")
# OD = never vaping among people with depression
lst_vapdeptargets$OD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="nevervap" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
# ED = current vaping among people with depression
lst_vapdeptargets$ED <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentvap" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
# QD = former vaping among people with depression
lst_vapdeptargets$QD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formervap" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
directory <- "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/data/"
save(lst_vapdeptargets,file=paste0(directory, "vapdep_calib_targets_",whichgender,".RData"))


# vap_microsim targets
lst_vaptargets <- vector(mode = "list")
lst_vaptargets$O <- as.matrix(subset(mdseprevs, sex==whichgender & status=="nevervap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_vaptargets$E <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentvap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_vaptargets$Q <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formervap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

# vaping targets for those who vaped past 5+,7+,10+ days 
lst_cvaptargets <- vector(mode = "list")
lst_cvaptargets$vap5 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap5" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_cvaptargets$vap7 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap7" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_cvaptargets$vap10 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap10" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

#save into mds-microsim/data
directory <- "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/data/"
save(lst_vaptargets,file=paste0(directory, "vap_calib_targets_",whichgender,".RData"))
save(lst_cvaptargets,file=paste0(directory, "cvap_calib_targets_",whichgender,".RData"))

# targets for exclcig, exclvap, dual, and neither
lst_nsduhtargets <- vector(mode = "list")
lst_nsduhtargets$nsduhexclcig <- as.matrix(subset(mdseprevs, sex==whichgender & status=="exclcig" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_nsduhtargets$nsduhexclvap <- as.matrix(subset(mdseprevs, sex==whichgender & status=="exclvap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_nsduhtargets$nsduhneither <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neither" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_nsduhtargets$nsduhdual <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dual" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

directory <- "C:/Users/klx3/Dropbox/tobacco-modeling-team/kane/GitHub/mds-microsim/data/"
save(lst_nsduhtargets,file=paste0(directory, "nsduh_general_targets_",whichgender,".RData"))


