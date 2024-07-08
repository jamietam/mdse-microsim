# Create list of calibration targets
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
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

# vap_microsim targets
lst_vaptargets <- vector(mode = "list")
lst_vaptargets$O <- as.matrix(subset(mdseprevs, sex==whichgender & status=="nevervap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_vaptargets$V <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentvap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_vaptargets$Q <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formervap" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

# vaping targets for those who vaped past 5+,7+,10+ days 
lst_cvaptargets <- vector(mode = "list")
lst_cvaptargets$vap5 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap5" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_cvaptargets$vap7 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap7" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_cvaptargets$vap10 <- as.matrix(subset(mdseprevs, sex==whichgender & status=="vap10" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

#save into mds-microsim/data
directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(lst_vaptargets,file=paste0(directory, "vap_calib_targets_",whichgender,".RData"))
save(lst_cvaptargets,file=paste0(directory, "cvap_calib_targets_",whichgender,".RData"))

