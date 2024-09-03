# NHIS 2019 & 2022 has questions about e-cigarettes, smoking, and depression status

mainDir <- "C:/Users/JT936/Dropbox/tempo-lab/data/NHIS"
setwd(file.path(mainDir))

library(survey)
library(haven) # to read in the Stata file
library(dplyr)

nhis_data <- read_dta(file="nhis_00016.dta")

# clean and recode data

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

# PHQ-9 Score ranges from 0-24. None= 0-5. Mild = 5-9. Moderate = 10-14. Severe = 15.
nhis_data$D_mild[nhis_data$phq_score>=5] <- 1 # mild to severe
nhis_data$D_mild[nhis_data$phq_score<5] <- 0
nhis_data$D_mod[nhis_data$phq_score>=10] <- 1 # moderate to severe
nhis_data$D_mod[nhis_data$phq_score<10] <- 0
nhis_data$D_sev[nhis_data$phq_score>=15] <- 1 # severe only
nhis_data$D_sev[nhis_data$phq_score<15] <- 0

# current e-cigarette use
nhis_data$E[nhis_data$ecigstatus==1] <- 0
nhis_data$E[nhis_data$ecigstatus==2] <- 0
nhis_data$E[nhis_data$ecigstatus==3] <- 1

nhis_data$O[nhis_data$ecigstatus==1] <- 1
nhis_data$O[nhis_data$ecigstatus==2] <- 0
nhis_data$O[nhis_data$ecigstatus==3] <- 0

nhis_data$Q[nhis_data$ecigstatus==1] <- 0
nhis_data$Q[nhis_data$ecigstatus==2] <- 1
nhis_data$Q[nhis_data$ecigstatus==3] <- 0


# Create survey designs 
svy19 <-svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2019))
svy21 <-svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2021))
svy22 <-svydesign(id=~psu, strata=~strata, nest=TRUE,weights=~sampweight, data=subset(nhis_data,year==2022))

# e-cigarette use prevalence in 2019
svymean(~E,design=svy19,na.rm=TRUE)
# matches with CDC MMWR 4.5% https://www.cdc.gov/mmwr/volumes/69/wr/mm6946a4.htm

# e-cigarette use prevalence in 2021
# svymean(~E,design=svy21,na.rm=TRUE) # https://www.cdc.gov/mmwr/volumes/72/wr/mm7218a1.htm

# e-cigarette use prevalence in 2022
svymean(~E,design=svy22,na.rm=TRUE) # https://www.cdc.gov/mmwr/volumes/72/wr/mm7218a1.htm

