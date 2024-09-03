rm(list = ls()) 
load("C:/Users/klx3/OneDrive - Yale University/Documents/GitHub/mds-microsim/R/brfss1618.rda")

brfss <- as.data.frame(brfss_smkecigdep)

whichgender = "females"
##change column names to match nsduh calib target column names
colnames(brfss)[colnames(brfss) == "year"] <- "survey_year"
colnames(brfss)[colnames(brfss) == "stderr"] <- "se"
colnames(brfss)[colnames(brfss) == "lowCIprev"] <- "prev_lowCI"
colnames(brfss)[colnames(brfss) == "highCIprev"] <- "prev_highCI"

#make exclsmk, exclvap, neither, and dual
# Ensure 'prev' column is numeric
brfss$prev[brfss$prev == ""] <- NA
brfss$prev <- as.numeric(as.character(brfss$prev))

# Extract column names from brfss
column_names <- names(brfss)

# Loop through years and ages
for(y in c(2016:2018)) { # loop through years
  for(a in c(18.24,25.34,35.49,50.64,65.99)) { # loop through ages
    for(s in c("males", "females"))
    # neither
    neither <- sum(subset(brfss, survey_year == y & age == a & gender==s & depstatus =="nodep" & subgroup=="totalpop" & (states == "NO" | states == "NQ" | states == "FO" | states == "FQ"))[,"prev"], na.rm = TRUE)
    new_row_neither <- data.frame(survey_year = y, gender = s, age = a, depstatus = NA, states = "neither", subgroup = "totalpop", prev = neither, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_neither) <- column_names
    brfss <- rbind(brfss, new_row_neither)
    
    # exclvap
    exclvap <- sum(subset(brfss, survey_year == y & age == a & gender==s & depstatus =="nodep" & subgroup=="totalpop" & (states == "NE" | states == "FE"))[,"prev"], na.rm = TRUE)
    new_row_exclvap <- data.frame(survey_year = y, gender = s, age = a, depstatus = NA, states = "exclvap", subgroup = "totalpop", prev = exclvap, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_exclvap) <- column_names
    brfss <- rbind(brfss, new_row_exclvap)
    
    # exclsmk
    exclsmk <- sum(subset(brfss, survey_year == y & age == a & gender==s & depstatus =="nodep" & subgroup=="totalpop" & (states == "CO" | states == "CQ"))[,"prev"], na.rm = TRUE)
    new_row_exclsmk <- data.frame(survey_year = y, gender = s, age = a, depstatus = NA, states = "exclsmk", subgroup = "totalpop", prev = exclsmk, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_exclsmk) <- column_names
    brfss <- rbind(brfss, new_row_exclsmk)
  }
}


#make these four states into targets
brfsstargets <- vector(mode = "list")
brfsstargets$b_exclsmk <- as.matrix(subset(brfss, gender==whichgender & states=="exclsmk" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
brfsstargets$b_exclvap <- as.matrix(subset(brfss, gender==whichgender & states=="exclvap" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
brfsstargets$b_neither <- as.matrix(subset(brfss, gender==whichgender & states=="neither" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
brfsstargets$b_dual <- as.matrix(subset(brfss, gender==whichgender & states=="CE" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(brfsstargets,file=paste0(directory, "brfss_general_calib_targets_",whichgender,".RData"))

#smoking targets

#depression targets

#vaping targets
b_vaptargets <- vector(mode = "list")
b_vaptargets$brfssO <- as.matrix(subset(brfss, gender==whichgender & states=="O" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
b_vaptargets$brfssE <- as.matrix(subset(brfss, gender==whichgender & states=="E" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
b_vaptargets$brfssQ <- as.matrix(subset(brfss, gender==whichgender & states=="Q" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(b_vaptargets,file=paste0(directory, "brfss_vap_calib_targets_",whichgender,".RData"))

