# Create list of calibration targets
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/"
setwd(file.path(mainDir))

load("data/mdseprevs0522.rda")

mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "females"

# smk_microsim targets
l.calib_targets <- vector(mode = "list")
l.calib_targets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$D <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$ND <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
l.calib_targets$CD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
l.calib_targets$FD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])

rownames(l.calib_targets$N) <- NULL
rownames(l.calib_targets$C) <- NULL
rownames(l.calib_targets$F) <- NULL
rownames(l.calib_targets$D) <- NULL
rownames(l.calib_targets$ND) <- NULL
rownames(l.calib_targets$CD) <- NULL
rownames(l.calib_targets$FD) <- NULL

save(l.calib_targets,file=paste0("data/mdse_calib_targets_",whichgender,".RData"))