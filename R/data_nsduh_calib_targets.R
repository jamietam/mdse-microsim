# Create list of calibration targets
mainDir <- "C:/Users/JT936/Dropbox/GitHub/mds-microsim/data/"
setwd(file.path(mainDir))

load("mdseprevs0520.rda")

mdseprevs$age[mdseprevs$age=="18to25"]<- 18.25
mdseprevs$age[mdseprevs$age=="26to34"]<- 26.34
mdseprevs$age[mdseprevs$age=="35to49"]<- 35.49
mdseprevs$age[mdseprevs$age=="50to64"]<- 50.64
mdseprevs$age[mdseprevs$age=="65plus"]<- 65.99

mdseprevs$age<-as.numeric(mdseprevs$age)

mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "males"

# smk_microsim targets
lst_smktargets <- vector(mode = "list")
lst_smktargets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_smktargets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_smktargets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
rownames(lst_smktargets$N) <- NULL
rownames(lst_smktargets$C) <- NULL
rownames(lst_smktargets$F) <- NULL

save(lst_smktargets,file=paste0("smk_calib_targets_",whichgender,".RData"))

# dep_microsim targets
lst_deptargets <- vector(mode = "list")
lst_deptargets$H <- as.matrix(subset(mdseprevs, gender==whichgender & status=="nevdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_deptargets$D <- as.matrix(subset(mdseprevs, gender==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$R <- as.matrix(subset(mdseprevs, gender==whichgender & status=="notdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$E <- as.matrix(subset(mdseprevs, gender==whichgender & status=="everdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

save(lst_deptargets,file=paste0("dep_calib_targets_",whichgender,".RData"))
