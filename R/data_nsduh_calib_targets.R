# Create list of calibration targets
mainDir <- "C:/Users/JT936/Dropbox/GitHub/mds-microsim/data/"
setwd(file.path(mainDir))

load("depsmkprevs_2005-2020.rda")
depsmkprevs_by_year<-depsmkprevs_by_year[order(depsmkprevs_by_year$age),]

whichgender = "females"

# smk_microsim targets
lst_smktargets <- vector(mode = "list")
lst_smktargets$N <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_smktargets$C <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_smktargets$F <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
rownames(lst_smktargets$N) <- NULL
rownames(lst_smktargets$C) <- NULL
rownames(lst_smktargets$F) <- NULL

save(lst_smktargets,file=paste0("smk_calib_targets_",whichgender,".RData"))

# dep_microsim targets
lst_deptargets <- vector(mode = "list")
lst_deptargets$H <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="nevdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_deptargets$D <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_deptargets$R <- as.matrix(subset(depsmkprevs_by_year, gender==whichgender & status=="notdep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
  
# lst_deptargets$everdeptotal <- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="total")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]
# lst_deptargets$everdep18to25 <- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="18to25")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]
# lst_deptargets$everdep26to34 <- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="26to34")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]
# lst_deptargets$everdep35to49 <- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="35to49")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]
# lst_deptargets$everdep50to64 <- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="50to64")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]
# lst_deptargets$everdep65plus<- subset(depsmkprevs_by_year, gender==whichgender & status=="everdep" & subpopulation=="totalpop"&age=="65plus")[,c("survey_year","prev","se","prev_lowCI","prev_highCI")]

# save(lst_deptargets,file=paste0("dep_microsim_targets_",whichgender,".RData"))


