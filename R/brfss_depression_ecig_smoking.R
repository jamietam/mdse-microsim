library(foreign)
library(survey)
library(reshape)
library(ggplot2)
setwd("C:/Users/JT936/Dropbox/tempo-lab/data/BRFSS/")
options(survey.lonely.psu = "adjust") # Set options for allowing a single observation per stratum 

# ADDEPEV3 Has a doctor, nurse, or other health professional ever told you that you had a depressive disorder 
#(including depression, major depression, dysthymia, or minor depression)? For each, tell me 1"Yes", 2"No", or you're "Not sure":

# X_ECIGSTS Four-level e-cigarette smoker status:   1 Everyday e-cigarette user, 2 Someday e-cigarette user,3 Former e-cigarette user, 4 Non-e-cigaretteuser
# ECIGARET Haveyoueverusedane-cigaretteorotherelectronicvapingproduct,evenjustonetime,inyourentirelife? 1=yes, 2= no
# ECIGNOW Doyounowusee-cigarettesorotherelectronic"vaping"productseveryday (1),somedays (2) ,ornotatall (3)?
# X_CURECIG Adultswhoarecurrente-cigaretteusers 1 = No, 2= Yes

# SMOKER3 Four-levelsmokerstatus:Everydaysmoker,Somedaysmoker,Formersmoker,Non-smoker
# RFSMOK3 Adultswhoarecurrentsmokers  1 = No, 2 = Yes

# brfss2016 <- read.xport("LLCP2016.XPT")
# save(brfss2016,file="LLCP2016.Rda")
# 
# brfss2017 <- read.xport("LLCP2017.XPT")
# save(brfss2017,file="LLCP2017.Rda")
# 
# brfss2018 <- read.xport("LLCP2018.XPT")
# brfss2018$SEX <- brfss2018$SEX1
# save(brfss2018,file="LLCP2018.Rda")

brfss2018$X_ECIGSTS[brfss2018$ECIGNOW==1] <- 1
brfss2018$X_ECIGSTS[brfss2018$ECIGNOW==2] <- 2
brfss2018$X_ECIGSTS[brfss2018$ECIGNOW==3 & brfss2018$ECIGARET== 1] <- 3
brfss2018$X_ECIGSTS[brfss2018$ECIGNOW==3 & brfss2018$ECIGARET== 2] <- 4

brfss2018$currentvap[brfss2018$ECIGNOW<=2] <- 1 #current e-cig user
brfss2018$currentvap[brfss2018$ECIGNOW==3] <- 0 # not currently using e-cigs
# 
# # Ecig and smoking status
# brfss2016$tobcat[brfss2016$X_CURECIG==1 & brfss2016$X_RFSMOK3==1] <- 1 # Neither
# brfss2016$tobcat[brfss2016$X_CURECIG==1 & brfss2016$X_RFSMOK3==2] <- 2 # Smoker only
# brfss2016$tobcat[brfss2016$X_CURECIG==2 & brfss2016$X_RFSMOK3==1] <- 3 # E-cig only
# brfss2016$tobcat[brfss2016$X_CURECIG==2 & brfss2016$X_RFSMOK3==2] <- 4 # Dual Users
# 
# brfss2017$tobcat[brfss2017$X_CURECIG==1 & brfss2017$X_RFSMOK3==1] <- 1 # Neither
# brfss2017$tobcat[brfss2017$X_CURECIG==1 & brfss2017$X_RFSMOK3==2] <- 2 # Smoker only
# brfss2017$tobcat[brfss2017$X_CURECIG==2 & brfss2017$X_RFSMOK3==1] <- 3 # E-cig only
# brfss2017$tobcat[brfss2017$X_CURECIG==2 & brfss2017$X_RFSMOK3==2] <- 4 # Dual Users
# 
# brfss2018$tobcat[brfss2018$X_CURECIG==1 & brfss2018$X_RFSMOK3==1] <- 1 # Neither
# brfss2018$tobcat[brfss2018$X_CURECIG==1 & brfss2018$X_RFSMOK3==2] <- 2 # Smoker only
# brfss2018$tobcat[brfss2018$X_CURECIG==2 & brfss2018$X_RFSMOK3==1] <- 3 # E-cig only
# brfss2018$tobcat[brfss2018$X_CURECIG==2 & brfss2018$X_RFSMOK3==2] <- 4 # Dual Users
# 
# vars <- c("X_STSTR","X_LLCPWT","X_AGE_G","SEX","ADDEPEV2","X_ECIGSTS","X_CURECIG","ECIGNOW","ECIGARET", "X_RFSMOK3","X_SMOKER3", "tobcat")
# 
# brfss2016vars <- brfss2016[vars]
# brfss2017vars <- brfss2017[vars]
# brfss2018vars <- brfss2018[vars]
# 
# save(brfss2016vars,file="brfss2016vars.Rda")
# save(brfss2017vars,file="brfss2017vars.Rda")
# save(brfss2018vars,file="brfss2018vars.Rda")

load("~/GitHub/mds-microsim/data/BRFSS2018vars.Rda")
load("~/GitHub/mds-microsim/data/brfss2017vars.Rda")
load("~/GitHub/mds-microsim/data/brfss2016vars.Rda")

table(brfss2016vars$X_SMOKER3, brfss2016vars$X_ECIGSTS)
table(brfss2017vars$X_SMOKER3, brfss2017vars$X_ECIGSTS)
table(brfss2016vars$X_RFSMOK3, brfss2016vars$X_CURECIG)
table(brfss2017vars$X_RFSMOK3, brfss2017vars$X_CURECIG)

design16 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss2016vars)
design17 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss2017vars)
design18 <- svydesign(id=~1, strata = ~X_STSTR, weights = ~X_LLCPWT, data = brfss2018vars)

# Smoking prevalence in 2017: 16.4%
# Smoking prevalence in 2018: 15.5%
svyciprop(~X_RFSMOK3==2, design=design17, method="mean",level=0.95,na.rm=TRUE)
svyciprop(~X_RFSMOK3==2, design=design18, method="mean",level=0.95,na.rm=TRUE)

svyciprop(~X_CURECIG==2, design=design18, method="mean",level=0.95,na.rm=TRUE)

# Current smoking = 15.5% in 2018
# Current vaping = 4.4% in 2017 https://www.shadac.org/sites/default/files/BRFSS-SPOTLIGHT_Smoking%26Vaping.pdf

depstat = c("dep","nodep")
gender = c("males","females")
tobcatlabel = c("neither","exclcig","exclvap","dual")

years = 2016:2018
svys = list(design16, design17, design18)
brfss_smkecigdep <- NULL
for (d in c(1:3)){ # for each survey year
  thissvydesign=svys[[d]]
  year = years[d]
  for (i in c(1:4)){ # tobacco use category
    for (s in c(1:2)){ 
        prop.ci = svyciprop(~tobcat==i, design=subset(thissvydesign,(ADDEPEV2==s & SEX==s)), method="mean",level=0.95,na.rm=TRUE)
        newrow <- cbind(years[d], gender[s], depstat[s], tobcatlabel[i], round(prop.ci[1]*100,2), round(SE(prop.ci)*100,2), round(attr(prop.ci, "ci")[1]*100,2), round(attr(prop.ci, "ci")[2]*100,2))
        brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow) # append table with new row of data
    } 
    prop.ci2 = svyciprop(~tobcat==i, design=subset(thissvydesign,ADDEPEV2==s), method="mean",level=0.95,na.rm=TRUE)
    newrow2 <- cbind(years[d],"both", depstat[s], tobcatlabel[i],round(prop.ci2[1]*100,2),round(SE(prop.ci2)*100,2), round(attr(prop.ci2, "ci")[1]*100,2), round(attr(prop.ci2, "ci")[2]*100,2))
    brfss_smkecigdep <- rbind(brfss_smkecigdep,newrow2) # append table with new row of data
  }
}
colnames(brfss_smkecigdep) <- c("year", "gender","depstatus", "tobstatus","prev", "stderr", "lowCIprev","highCIprev")
