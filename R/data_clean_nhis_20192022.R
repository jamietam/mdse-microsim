# NHIS 2019 & 2022 has questions about e-cigarettes, smoking, and depression status
rm(list = ls()) 
# mainDir <- "C:/Users/klx3/OneDrive - Yale University/Documents/Github/mds-microsim/"
mainDir <- "/Users/jt936/Dropbox/GitHub/mds-microsim/"
setwd(file.path(mainDir))

library(survey)
library(haven) # to read in the Stata file
library(dplyr)

nhis_data <- read_dta("data-raw/nhis_00016.dta")

##subset for years 2019 and 2022
nhis_data <- subset(nhis_data, year == 2019 | year == 2022)

# data clean
  # depression status
  nhis_data$phqintr[nhis_data$phqintr>3] <- NA
  nhis_data$phqdep[nhis_data$phqdep>3] <- NA
  nhis_data$phqsleep[nhis_data$phqsleep>3] <- NA
  nhis_data$phqengy[nhis_data$phqengy>3] <- NA
  nhis_data$phqeat[nhis_data$phqeat>3] <- NA
  nhis_data$phqbad[nhis_data$phqbad>3] <- NA
  nhis_data$phqconc[nhis_data$phqconc>3] <- NA
  nhis_data$phqmove[nhis_data$phqmove>3] <- NA
  nhis_data$phq_score = nhis_data$phqintr + nhis_data$phqdep + nhis_data$phqsleep + nhis_data$phqengy + nhis_data$phqeat + 
    nhis_data$phqbad + nhis_data$phqconc + nhis_data$phqmove
  nhis_data$phqcat[nhis_data$phqcat>4] <- NA
  
  # PHQ-9 Score ranges from 0-24. None= 0-5. Mild = 5-9. Moderate = 10-14. Severe = 15. Use these as subpops
  nhis_data$D_mild[nhis_data$phq_score>=5] <- 1 # mild to severe
  nhis_data$D_mild[nhis_data$phq_score<5] <- 0
  nhis_data$D_mod[nhis_data$phq_score>=10] <- 1 # moderate to severe
  nhis_data$D_mod[nhis_data$phq_score<10] <- 0
  nhis_data$D_sev[nhis_data$phq_score>=15] <- 1 # severe only
  nhis_data$D_sev[nhis_data$phq_score<15] <- 0
  
  # current smoking use
  nhis_data$C[nhis_data$smokestatus2==11 | nhis_data$smokestatus2==12] <- 1
  nhis_data$C[nhis_data$smokestatus2==0] <- 0
  nhis_data$C[nhis_data$smokestatus2>12] <- 0
  
  nhis_data$N[nhis_data$smokestatus2==30] <- 1
  nhis_data$N[nhis_data$smokestatus2<30] <- 0
  nhis_data$N[nhis_data$smokestatus2>30] <- 0
  
  nhis_data$F[nhis_data$smokestatus2==20] <- 1
  nhis_data$F[nhis_data$smokestatus2<20] <- 0
  nhis_data$F[nhis_data$smokestatus2>20] <- 0
  
  # current e-cigarette use
  nhis_data$E[nhis_data$ecigstatus==1] <- 0
  nhis_data$E[nhis_data$ecigstatus==2] <- 0
  nhis_data$E[nhis_data$ecigstatus==3] <- 1
  nhis_data$E[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$O[nhis_data$ecigstatus==1] <- 1
  nhis_data$O[nhis_data$ecigstatus==2] <- 0
  nhis_data$O[nhis_data$ecigstatus==3] <- 0
  nhis_data$O[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$Q[nhis_data$ecigstatus==1] <- 0
  nhis_data$Q[nhis_data$ecigstatus==2] <- 1
  nhis_data$Q[nhis_data$ecigstatus==3] <- 0
  nhis_data$Q[nhis_data$ecigstatus>3] <- NA
  
  # dual states
  nhis_data$NO[nhis_data$smokestatus2==30 & nhis_data$ecigstatus==1] <- 1
  nhis_data$NO[nhis_data$smokestatus2<30 | nhis_data$smokestatus2>30] <- 0
  nhis_data$NO[nhis_data$ecigstatus==2 | nhis_data$ecigstatus==3] <- 0
  nhis_data$NO[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$CO[(nhis_data$smokestatus2==11 | nhis_data$smokestatus2==12) & nhis_data$ecigstatus==1] <- 1
  nhis_data$CO[(nhis_data$smokestatus2==0 | nhis_data$smokestatus2>12)] <- 0
  nhis_data$CO[nhis_data$ecigstatus==2 | nhis_data$ecigstatus==3] <- 0
  nhis_data$CO[nhis_data$ecigstatus>3] <- NA

  nhis_data$FO[nhis_data$smokestatus2==20 & nhis_data$ecigstatus==1] <- 1
  nhis_data$FO[(nhis_data$smokestatus2<20 | nhis_data$smokestatus2>20)] <- 0
  nhis_data$FO[nhis_data$ecigstatus==2 | nhis_data$ecigstatus==3] <- 0
  nhis_data$FO[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$NE[nhis_data$smokestatus2==30 & nhis_data$ecigstatus==3] <- 1
  nhis_data$NE[nhis_data$smokestatus2<30 | nhis_data$smokestatus2>30] <- 0
  nhis_data$NE[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==2] <- 0
  nhis_data$NE[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$CE[(nhis_data$smokestatus2==11 | nhis_data$smokestatus2==12) & nhis_data$ecigstatus==3] <- 1
  nhis_data$CE[(nhis_data$smokestatus2==0 | nhis_data$smokestatus2>12)] <- 0
  nhis_data$CE[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==2] <- 0
  nhis_data$CE[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$FE[nhis_data$smokestatus2==20 & nhis_data$ecigstatus==3] <- 1
  nhis_data$FE[(nhis_data$smokestatus2<20 | nhis_data$smokestatus2>20)] <- 0
  nhis_data$FE[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==2] <- 0
  nhis_data$FE[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$NQ[nhis_data$smokestatus2==30 & nhis_data$ecigstatus==2] <- 1
  nhis_data$NQ[nhis_data$smokestatus2<30 | nhis_data$smokestatus2>30] <- 0
  nhis_data$NQ[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==3] <- 0
  nhis_data$NQ[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$CQ[(nhis_data$smokestatus2==11 | nhis_data$smokestatus2==12) & nhis_data$ecigstatus==2] <- 1
  nhis_data$CQ[(nhis_data$smokestatus2==0 | nhis_data$smokestatus2>12)] <- 0
  nhis_data$CQ[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==3] <- 0
  nhis_data$CQ[nhis_data$ecigstatus>3] <- NA
  
  nhis_data$FQ[nhis_data$smokestatus2==20 & nhis_data$ecigstatus==2] <- 1
  nhis_data$FQ[(nhis_data$smokestatus2<20 | nhis_data$smokestatus2>20)] <- 0
  nhis_data$FQ[nhis_data$ecigstatus==1 | nhis_data$ecigstatus==3] <- 0
  nhis_data$FQ[nhis_data$ecigstatus>3] <- NA
  
#create agegroups 18.24,25.34,35.49,50.64,65.99
  nhis_data$ageg[nhis_data$age >= 18 & nhis_data$age <= 25] <- 1 
  nhis_data$ageg[nhis_data$age >= 25 & nhis_data$age <= 34] <- 2
  nhis_data$ageg[nhis_data$age >= 35 & nhis_data$age <= 49] <- 3
  nhis_data$ageg[nhis_data$age >= 50 & nhis_data$age <= 64] <- 4
  nhis_data$ageg[nhis_data$age >= 65 & nhis_data$age <= 99] <- 5

# create new column for states in dataframe
get_states <- function(nhis){
  states <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
  for (i in seq_along(states)) {
    nhis$states[nhis[[states[i]]] == 1] <- i
  }
  return(nhis$states)
}
nhis_data$states <- get_states(nhis_data)
#single states for smoking
smk_singlestates <- function(nhis){
  states <- c("N","C","F")
  for(i in seq_along(states)){
    nhis$smk_singlestates[nhis[[states[i]]] == 1] <- i
  }
  return(nhis$smk_singlestates)
}
nhis_data$smk_singlestates <- smk_singlestates(nhis_data)
#single states for vaping
e_singlestates <- function(nhis){
  states <- c("O","E","Q")
  for(i in seq_along(states)){
    nhis$e_singlestates[nhis[[states[i]]] == 1] <- i
  }
  return(nhis$e_singlestates)
}
nhis_data$e_singlestates <- e_singlestates(nhis_data)


# Create survey designs by depression subgroups (total, D_mild, D_mod, D_sev)
# total
options(survey.lonely.psu = "adjust")
svy19 <-svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2019))
svy22 <-svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2022))
# D_mild
svy19_mild <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2019 & D_mild==1))
svy22_mild <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2022 & D_mild==1))
# D_mod
svy19_mod <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2019 & D_mod==1))
svy22_mod <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2022 & D_mod==1))
# D_sev
svy19_sev <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2019 & D_sev==1))
svy22_sev <- svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2022 & D_sev==1))

# get prevalence by year, age and states
gender = c("males","females")
statenames <- c("NO", "NE", "NQ", "CO", "CE", "CQ", "FO", "FE", "FQ")
smk_singlestatenames <- c("N","C","F")
e_singlestatenames <- c("O","E","Q")
svys = list(svy19, svy22, svy19_mild, svy22_mild, svy19_mod, svy22_mod, svy19_sev, svy22_sev)
years = c(2019,2022,2019,2022,2019,2022,2019,2022)
agegroupnames <- c(18.24,25.34,35.49,50.64,65.99)
subgroupnames <- c("totalpop", "totalpop", "d_mild", "d_mild", "d_mod", "d_mod", "d_sev", "d_sev")
nhis_smkecigdep <- NULL
for(s in c(1:8)){ #for each subgroup
  thissvydesign=svys[[s]]
  year = years[[s]]
  for(d in c(1:9)){ #loop through states dual use
    for(a in c(1:5)){ #by age group
      for(g in c(1:2)){ #by sex
        prop.ci = svyciprop(~states==d, design=subset(thissvydesign, sex==g & ageg==a), method="mean",level=0.95,na.rm=TRUE) 
        newrow <- cbind(years[s],gender[g],agegroupnames[a],statenames[d],subgroupnames[s],round(prop.ci[1],2),round(SE(prop.ci),2),round(attr(prop.ci, "ci")[1],2),round(attr(prop.ci, "ci")[2],2))
        nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow) # append table with new row of data
      }
      prop.ci2 = svyciprop(~states==d, design=subset(thissvydesign, ageg==a), method="mean",level=0.95,na.rm=TRUE)
      newrow2 <- cbind(years[s],"both",agegroupnames[a],statenames[d],subgroupnames[s],round(prop.ci2[1],2),round(SE(prop.ci2),2),round(attr(prop.ci2, "ci")[1],2),round(attr(prop.ci2, "ci")[2],2))
      nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow2) # append table with new row of data
    }
  }
  for(e in c(1:3)){ #loop through single state ecig use
    for(a in c(1:5)){ #by age group
      for(g in c(1:2)){ #by sex
        prop.ci = svyciprop(~e_singlestates==e, design=subset(thissvydesign, sex==g & ageg==a), method="mean",level=0.95,na.rm=TRUE) 
        newrow <- cbind(years[s],gender[g],agegroupnames[a],e_singlestatenames[e],subgroupnames[s],round(prop.ci[1],2),round(SE(prop.ci),2),round(attr(prop.ci, "ci")[1],2),round(attr(prop.ci, "ci")[2],2))
        nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow) # append table with new row of data
      }
      prop.ci2 = svyciprop(~e_singlestates==e, design=subset(thissvydesign, ageg==a), method="mean",level=0.95,na.rm=TRUE)
      newrow2 <- cbind(years[s],"both",agegroupnames[a],e_singlestatenames[e],subgroupnames[s],round(prop.ci2[1],2),round(SE(prop.ci2),2),round(attr(prop.ci2, "ci")[1],2),round(attr(prop.ci2, "ci")[2],2))
      nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow2) # append table with new row of data
    }
  }
  for(m in c(1:3)){ #loop through single state smoking use
    for(a in c(1:5)){ #by age group
      for(g in c(1:2)){ #by sex
        prop.ci = svyciprop(~smk_singlestates==m, design=subset(thissvydesign, sex==g & ageg==a), method="mean",level=0.95,na.rm=TRUE) 
        newrow <- cbind(years[s],gender[g],agegroupnames[a],smk_singlestatenames[m],subgroupnames[s],round(prop.ci[1],2),round(SE(prop.ci),2),round(attr(prop.ci, "ci")[1],2),round(attr(prop.ci, "ci")[2],2))
        nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow) # append table with new row of data
      }
      prop.ci2 = svyciprop(~smk_singlestates==m, design=subset(thissvydesign, ageg==a), method="mean",level=0.95,na.rm=TRUE)
      newrow2 <- cbind(years[s],"both",agegroupnames[a],smk_singlestatenames[m],subgroupnames[s],round(prop.ci2[1],2),round(SE(prop.ci2),2),round(attr(prop.ci2, "ci")[1],2),round(attr(prop.ci2, "ci")[2],2))
      nhis_smkecigdep <- rbind(nhis_smkecigdep,newrow2) # append table with new row of data
    }
  }
}
colnames(nhis_smkecigdep) <- c("year","gender","age","states","subgroup", "prev","stderr","lowCIprev","highCIprev")
save(nhis_smkecigdep,file=paste0("data/nhis1922.rda"))

# e-cigarette use prevalence in 2019
svymean(~E,design=svy19,na.rm=TRUE)
# matches with CDC MMWR 4.5% https://www.cdc.gov/mmwr/volumes/69/wr/mm6946a4.htm

# e-cigarette use prevalence in 2021
# svymean(~E,design=svy21,na.rm=TRUE) # https://www.cdc.gov/mmwr/volumes/72/wr/mm7218a1.htm

# e-cigarette use prevalence in 2022
svymean(~E,design=svy22,na.rm=TRUE) # https://www.cdc.gov/mmwr/volumes/72/wr/mm7218a1.htm