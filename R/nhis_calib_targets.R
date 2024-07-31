rm(list = ls()) 
mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/R"
setwd(file.path(mainDir))
load("nhis1922.rda")

nhis <- as.data.frame(nhis_smkecigdep)

whichgender = "females"
##change column names to match nsduh calib target column names
colnames(nhis)[colnames(nhis) == "year"] <- "survey_year"
colnames(nhis)[colnames(nhis) == "stderr"] <- "se"
colnames(nhis)[colnames(nhis) == "lowCIprev"] <- "prev_lowCI"
colnames(nhis)[colnames(nhis) == "highCIprev"] <- "prev_highCI"

#create targets for exclvap, exclsmk, dual, neither
# Ensure 'prev' column is numeric
nhis$prev[nhis$prev == ""] <- NA
nhis$prev <- as.numeric(as.character(nhis$prev))

# Extract column names from nhis
column_names <- names(nhis)
for(y in c(2019,2022)) { # loop through years
  for(a in c(18.24,25.34,35.49,50.64,65.99)) { # loop through ages
    for(s in c("males", "females"))
      # neither
      neither <- sum(subset(nhis, survey_year == y & age == a & gender==s & subgroup=="totalpop" & (states == "NO" | states == "NQ" | states == "FO" | states == "FQ"))[,"prev"], na.rm = TRUE)
    new_row_neither <- data.frame(survey_year = y, gender = s, age = a, states = "neither", subgroup = "totalpop", prev = neither, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_neither) <- column_names
    nhis <- rbind(nhis, new_row_neither)
    
    # exclvap
    exclvap <- sum(subset(nhis, survey_year == y & age == a & gender==s & subgroup=="totalpop" & (states == "NE" | states == "FE"))[,"prev"], na.rm = TRUE)
    new_row_exclvap <- data.frame(survey_year = y, gender = s, age = a, states = "exclvap", subgroup = "totalpop", prev = exclvap, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_exclvap) <- column_names
    nhis <- rbind(nhis, new_row_exclvap)
    
    # exclsmk
    exclsmk <- sum(subset(nhis, survey_year == y & age == a & gender==s & subgroup=="totalpop" & (states == "CO" | states == "CQ"))[,"prev"], na.rm = TRUE)
    new_row_exclsmk <- data.frame(survey_year = y, gender = s, age = a, states = "exclsmk", subgroup = "totalpop", prev = exclsmk, se = NA, prev_lowCI = NA, prev_highCI = NA, stringsAsFactors = FALSE)
    names(new_row_exclsmk) <- column_names
    nhis <- rbind(nhis, new_row_exclsmk)
  }
}

nhistargets <- vector(mode = "list")
nhistargets$n_exclsmk <- as.matrix(subset(nhis, gender==whichgender & states=="exclsmk" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
nhistargets$n_exclvap <- as.matrix(subset(nhis, gender==whichgender & states=="exclvap" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
nhistargets$n_neither <- as.matrix(subset(nhis, gender==whichgender & states=="neither" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
nhistargets$n_dual <- as.matrix(subset(nhis, gender==whichgender & states=="CE" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(nhistargets,file=paste0(directory, "nhis_general_calib_targets_",whichgender,".RData"))

#smoking targets

#depression targets

#vaping targets
n_vaptargets <- vector(mode = "list")
n_vaptargets$nhisO <- as.matrix(subset(nhis, gender==whichgender & states=="O" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
n_vaptargets$nhisE <- as.matrix(subset(nhis, gender==whichgender & states=="E" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)
n_vaptargets$nhisQ <- as.matrix(subset(nhis, gender==whichgender & states=="Q" & subgroup=="totalpop")[,c("age", "survey_year","prev","se","prev_lowCI","prev_highCI")],rownames.force = NA)

directory <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/data/"
save(n_vaptargets,file=paste0(directory, "nhis_vap_calib_targets_",whichgender,".RData"))
