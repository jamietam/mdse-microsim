rm(list = ls()) 
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/R"
setwd(file.path(mainDir))
load("brfss1618.rda")

brfss <- as.data.frame(brfss_smkecigdep)

whichgender = "females"
##change column names to match nsduh calib target column names
colnames(brfss)[colnames(brfss) == "year"] <- "survey_year"
colnames(brfss)[colnames(brfss) == "stderr"] <- "se"
colnames(brfss)[colnames(brfss) == "lowCIprev"] <- "prev_lowCI"
colnames(brfss)[colnames(brfss) == "highCIprev"] <- "prev_highCI"


#smoking targets

#depression targets

#vaping targets
b_vaptargets <- vector(mode = "list")
b_vaptargets$brfssO <- as.matrix(subset(brfss, gender==whichgender & states=="O" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
b_vaptargets$brfssE <- as.matrix(subset(brfss, gender==whichgender & states=="E" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
b_vaptargets$brfssQ <- as.matrix(subset(brfss, gender==whichgender & states=="Q" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(b_vaptargets,file=paste0(directory, "brfss_vap_calib_targets_",whichgender,".RData"))
