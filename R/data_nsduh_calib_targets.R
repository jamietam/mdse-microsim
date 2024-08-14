# Create list of calibration targets
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/"
setwd(file.path(mainDir))

load("data/mdseprevs0522.rda")

mdseprevs<-mdseprevs[order(mdseprevs$age),]

whichgender = "males"

# smk_microsim targets
lst_calibtargets <- vector(mode = "list")
lst_calibtargets$N <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_calibtargets$C <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_calibtargets$F <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_calibtargets$D <- as.matrix(subset(mdseprevs, sex==whichgender & status=="dep" & subpopulation=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_calibtargets$ND <- as.matrix(subset(mdseprevs, sex==whichgender & status=="neversmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
lst_calibtargets$CD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="currentsmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])
lst_calibtargets$FD <- as.matrix(subset(mdseprevs, sex==whichgender & status=="formersmk" & subpopulation=="deppop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")])

rownames(lst_calibtargets$N) <- NULL
rownames(lst_calibtargets$C) <- NULL
rownames(lst_calibtargets$F) <- NULL
rownames(lst_calibtargets$D) <- NULL
rownames(lst_calibtargets$ND) <- NULL
rownames(lst_calibtargets$CD) <- NULL
rownames(lst_calibtargets$FD) <- NULL

save(lst_calibtargets,file=paste0("data/mdse_calib_targets_",whichgender,".RData"))