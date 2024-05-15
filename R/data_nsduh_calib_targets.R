# Create list of calibration targets
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/data/"
setwd(file.path(mainDir))

load("mdseprevs0522.rda")

# drop 18.99 age group
mdseprevs <- subset(mdseprevs, age!=18.99)
mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "females"

# smk_microsim targets
lst_smktargets <- vector(mode = "list")
lst_smktargets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_smktargets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_smktargets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
rownames(lst_smktargets$N) <- NULL
rownames(lst_smktargets$C) <- NULL
rownames(lst_smktargets$F) <- NULL

save(lst_smktargets,file=paste0("smk_calib_targets_",whichgender,".RData"))

# dep_microsim targets
lst_deptargets <- vector(mode = "list")
lst_deptargets$H <- as.matrix(subset(mdseprevs, sex==whichgender & status=="nevdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_deptargets$D <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$R <- as.matrix(subset(mdseprevs, sex==whichgender & status=="fdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$E <- as.matrix(subset(mdseprevs, sex==whichgender & status=="everdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

save(lst_deptargets,file=paste0("dep_calib_targets_",whichgender,".RData"))