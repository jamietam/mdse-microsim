mainDir <- "/Users/srs249/University of Michigan Dropbox/Sarah Skolnick/GitHub/mdse-microsim/"
setwd(file.path(mainDir))

library(openxlsx)
library(ggplot2)
library(survey)

whichgender="females"

## Price index for inflating to 2023 USD
# healthcare
bea2024.hc <- read.xlsx("data-raw/BEA_consumption_2024Section2all_xls.xlsx",sheet = "T20304-A",startRow=7,colNames=TRUE)[16,paste0(2003:2023)] # row 16 = Health care services
bea2024.hc <- sapply(bea2024.hc, as.numeric) 
# total expenditures
bea2024.total <- read.xlsx("data-raw/BEA_consumption_2024Section2all_xls.xlsx",sheet = "T20304-A",startRow=7,colNames=TRUE)[1,paste0(2022:2023)] # row 1 = Personal consumption expenditures (PCE)


# Consumer expenditures ---------------------------------------------------
# data from BLS is 2022 USD, inflate to 2023 USD
bls2022 <- read.xlsx("data-raw/reference-person-age-ranges-2022.xlsx",startRow=2,colNames=TRUE) 
colnames(bls2022) <- c("Item","18.99", "18.24","25.34","35.44","45.54", "55.64","65.99","65.74","75.99") # assume no consumption among children, start at age 18
totalexp <- as.numeric(bls2022[40,c(3:7,9,10)]) # average consumer expenditures across all categories by age
healthcare <- as.numeric(bls2022[444,c(3:7,9,10)]) # health care expenditures by age
nonhealthcare <- totalexp-healthcare
c.nonhealth <- max(bea2024.total)/min(bea2024.total)*c(rep(0,18),rep(nonhealthcare[1],7),rep(nonhealthcare[2],10),rep(nonhealthcare[3],10),rep(nonhealthcare[4],10),rep(nonhealthcare[5],10),rep(nonhealthcare[6],10),rep(nonhealthcare[7],25))

consumerexpenditures <- as.data.frame(cbind(age=0:99, consexp=c.nonhealth))
ggplot(data=consumerexpenditures)+geom_line(aes(x=age,y=consexp))+theme_light()


# Healthcare costs --------------------------------------------------------
# data from Swedler et al at 2015 USD, inflate to 2023 USD
c.NH <- c.NR <- bea2024.hc["2023"]/bea2024.hc["2015"]*c(rep(0,20),rep(2743, 10), rep(3214,10),rep(3763,10),rep(4401,10),rep(5143,10), rep(6007, 10), rep(7010,20)) # cost of remaining one cycle Never Smoking, No MD
c.CH <- c.CR <- bea2024.hc["2023"]/bea2024.hc["2015"]*c(rep(0,20),rep(2909, 10), rep(3413,10),rep(4000,10),rep(4683,10),rep(5478,10), rep(6403, 10), rep(7479,20)) # cost of remaining one cycle Current Smoking, No MD
c.FH <- c.FR <- bea2024.hc["2023"]/bea2024.hc["2015"]*c(rep(0,20),rep(3208, 10), rep(3754,10),rep(4390,10),rep(5130,10),rep(5990,10), rep(6990, 10), rep(8153,20)) # cost of remaining one cycle Former Smoking, No MD

# data from Egede et al 2014 USD, inflate to 2023 USD
c.D <- bea2024.hc["2023"]/bea2024.hc["2014"]*2654 # incremental cost of depression 

# combine smoking and depression costs
c.ND <- c.NH + c(rep(0,18),rep(c.D,82))
c.CD <- c.CH + c(rep(0,18),rep(c.D,82))
c.FD <- c.FH + c(rep(0,18),rep(c.D,82))

# create figure
healthcarecosts <- as.data.frame(rbind(cbind(c.NH,"Never smoking","No MDE"),cbind(c.CH,"Current smoking","No MDE"),cbind(c.FH,"Former smoking","No MDE"), 
                                       cbind(c.ND,"Never smoking","MDE"),cbind(c.CD,"Current smoking","MDE"),cbind(c.FD,"Former smoking","MDE")))
healthcarecosts$age <- rep(c(0:99),6)
names(healthcarecosts) <- c("costs","smoking_status","dep_status", "age")
healthcarecosts$smoking_status <- factor(healthcarecosts$smoking_status,levels = c("Never smoking","Current smoking","Former smoking"))
healthcarecosts$costs <- as.numeric(healthcarecosts$costs)
healthcarecosts$agecat[healthcarecosts$age<18]<-0.17
healthcarecosts$agecat[healthcarecosts$age>17&healthcarecosts$age<20]<-18.19
healthcarecosts$agecat[healthcarecosts$age>19&healthcarecosts$age<30]<-20.29
healthcarecosts$agecat[healthcarecosts$age>29&healthcarecosts$age<40]<-30.39
healthcarecosts$agecat[healthcarecosts$age>39&healthcarecosts$age<50]<-40.49
healthcarecosts$agecat[healthcarecosts$age>49&healthcarecosts$age<60]<-50.59
healthcarecosts$agecat[healthcarecosts$age>59&healthcarecosts$age<70]<-60.69
healthcarecosts$agecat[healthcarecosts$age>69&healthcarecosts$age<80]<-70.79
healthcarecosts$agecat[healthcarecosts$age>79]<-80.99
healthcarecosts$agecat<- as.factor(healthcarecosts$agecat)

jpeg(paste0("output/healthcarecosts.jpeg"), width = 8, height = 3.5, units = "in", res = 500)
ggplot(data=subset(healthcarecosts,agecat!=0.17))+geom_bar(aes(x=agecat,y=costs,colour = smoking_status,fill = smoking_status), position = "dodge2",stat="identity") +
  facet_wrap(~dep_status)+theme_light()+
  scale_y_continuous(name = "Costs $USD",limits=c(0,14000),seq(0,14000,2000))+
  scale_fill_manual(values=c("#00BA38","#F8766D", "#619CFF"))+
  scale_color_manual(values=c("#00BA38","#F8766D", "#619CFF"))+
  labs(title="Medical expenditures by smoking and depression status")
dev.off()


# Utilities ---------------------------------------------------------------
if (whichgender=="females"){
  # not depressed, no e-cig
  u.NOH <- u.NOR <- u.NQH <- u.NQR <- c(rep(1,18),rep(0.839,7),rep(0.821,20), rep(0.795,20),rep(0.7605,35))
  u.COH <- u.COR <- u.CQH <- u.CQR <- c(rep(1,18),rep(0.829,7),rep(0.804,20), rep(0.773,20),rep(0.724,35))
  u.FOH <- u.FOR <- u.FQH <- u.FQR <- c(rep(1,18),rep(0.832,7),rep(0.816,20), rep(0.778,20),rep(0.7405,35))
  
  # depressed, no e-cig
  u.NOD <- u.NQD <- c(rep(1,18),rep(0.824,7),rep(0.801,20), rep(0.773,20),rep(0.7095,35))
  u.COD <- u.CQD <- c(rep(1,18),rep(0.817,7),rep(0.793,20), rep(0.757,20),rep(0.7075,35))
  u.FOD <- u.FQD <- c(rep(1,18),rep(0.826,7),rep(0.799,20), rep(0.761,20),rep(0.7095,35))
  
  # not depressed, e-cig
  u.NEH <- u.NER <- c(rep(1,18),rep(0.839,7),rep(0.821,20), rep(0.776,20),rep(0.7605,35))
  u.CEH <- u.CER <- c(rep(1,18),rep(0.817,7),rep(0.8,20), rep(0.767,20),rep(0.7405,35))
  u.FEH <- u.FER <- c(rep(1,18),rep(0.821,7),rep(0.816,20), rep(0.777,20),rep(0.764,35))
  
  # depressed, e-cig
  u.NED <- c(rep(1,18),rep(0.824,7),rep(0.8,20), rep(0.776,20),rep(0.7075,35))
  u.CED <- c(rep(1,18),rep(0.799,7),rep(0.798,20), rep(0.755,20),rep(0.7095,35))
  u.FED <- c(rep(1,18),rep(0.821,7),rep(0.798,20), rep(0.759,20),rep(0.7095,35))
  
} else {
  # males
  # not depressed, no e-cig
  u.NOH <- u.NOR <- u.NQH <- u.NQR <- c(rep(1,18),rep(0.843,7),rep(0.821,20), rep(0.787,20),rep(0.7405,35))
  u.COH <- u.COR <- u.CQH <- u.CQR <- c(rep(1,18),rep(0.832,7),rep(0.804,20), rep(0.773,20),rep(0.7095,35))
  u.FOH <- u.FOR <- u.FQH <- u.FQR <- c(rep(1,18),rep(0.843,7),rep(0.811,20), rep(0.776,20),rep(0.724,35))
  
  # depressed, no e-cig
  u.NOD <- u.NQD <- c(rep(1,18),rep(0.826,7),rep(0.8,20), rep(0.761,20),rep(0.7075,35))
  u.COD <- u.CQD <- c(rep(1,18),rep(0.8,7),rep(0.781,20), rep(0.717,20),rep(0.706,35))
  u.FOD <- u.FQD <- c(rep(1,18),rep(0.826,7),rep(0.8,20), rep(0.757,20),rep(0.7075,35))
  
  # not depressed, e-cig
  u.NEH <- u.NER <- c(rep(1,18),rep(0.839,7),rep(0.816,20), rep(0.767,20),rep(0.7075,35))
  u.CEH <- u.CER <- c(rep(1,18),rep(0.817,7),rep(0.801,20), rep(0.767,20),rep(0.764,35))
  u.FEH <- u.FER <- c(rep(1,18),rep(0.832,7),rep(0.811,20), rep(0.773,20),rep(0.7725,35))
  
  # depressed, e-cig
  u.NED <- c(rep(1,18),rep(0.824,7),rep(0.801,20), rep(0.767,20),rep(0.7625,35))
  u.CED <- c(rep(1,18),rep(0.805,7),rep(0.793,20), rep(0.757,20),rep(0.6995,35))
  u.FED <- c(rep(1,18),rep(0.823,7),rep(0.799,20), rep(0.759,20),rep(0.7095,35))
}

# Create a helper function to generate each combination
create_utility_df <- function(values, smk_status, gender, dep_status, vap_status = NA) {
  data.frame(
    age = 0:99,
    utility = values,
    smk_status = smk_status,
    gender = gender,
    dep_status = dep_status,
    vap_status = vap_status,
    stringsAsFactors = FALSE
  )
}


# Combine all groups into one data frame
utilities <- do.call(rbind, list(
  create_utility_df(u.NOH, "N", whichgender, "notD","notE"),
  create_utility_df(u.COH, "C", whichgender, "notD","notE"),
  create_utility_df(u.FOH, "F", whichgender, "notD","notE"),
  create_utility_df(u.NEH, "N", whichgender, "notD","E"),
  create_utility_df(u.CEH, "C", whichgender, "notD","E"),
  create_utility_df(u.FEH, "F", whichgender, "notD","E"),
  create_utility_df(u.NOD, "N", whichgender, "D","notE"),
  create_utility_df(u.COD, "C", whichgender, "D","notE"),
  create_utility_df(u.FOD, "F", whichgender, "D","notE"),
  create_utility_df(u.NED, "N", whichgender, "D","E"),
  create_utility_df(u.CED, "C", whichgender, "D","E"),
  create_utility_df(u.FED, "F", whichgender, "D","E")
))

# Convert column types
utilities$age <- as.numeric(utilities$age)
utilities$utility <- as.numeric(utilities$utility)

# Factor levels — adjusted to match values in your data
utilities$smk_status <- factor(utilities$smk_status, levels = c("C", "N", "F"))
utilities$dep_status <- factor(utilities$dep_status, levels = c("notD", "D"))
utilities$vap_status <- factor(utilities$vap_status, levels = c("notE", "E"))

pdf(paste0("output/utilities.pdf"), width = 11, height = 8.5, onefile=TRUE)
# jpeg(paste0("output/utilities.jpeg"), width = 5, height = 3.5, units = "in", res = 500)
ggplot(data=utilities) + 
  geom_step(aes(x=age,y=utility,colour = dep_status,linetype=vap_status)) +
  facet_wrap(~smk_status)+
  scale_y_continuous(limits = c(0.7,1),breaks=seq(0.7,1,0.05))+
  labs(title="Utilities by vaping and depression status - smoking groups")

ggplot(data=utilities) + 
  geom_step(aes(x=age,y=utility,colour = smk_status,linetype=vap_status)) +
  facet_wrap(~dep_status)+
  scale_y_continuous(limits = c(0.7,1),breaks=seq(0.7,1,0.05))+
  labs(title="Utilities by smoking and vaping status - depression groups")

ggplot(data=utilities) + 
  geom_step(aes(x=age,y=utility,colour = smk_status,linetype=dep_status)) +
  facet_wrap(~vap_status)+
  scale_y_continuous(limits = c(0.7,1),breaks=seq(0.7,1,0.05))+
  labs(title="Utilities by smoking and depression status - vaping groups")
dev.off()

# Productivities ----------------------------------------------------------
ppcps<-read.csv("data-raw/asecpub23csv/pppub23.csv")
cps<-read.csv("data-raw/asecpub23csv/asec_csv_repwgt_2023.csv")
cps$PH_SEQ <- cps$h_seq
mergedcps <- merge(cps,ppcps,by = c("PH_SEQ","PPPOS"))
svy <- svrepdesign(data = mergedcps, repweights = "pwwgt[0-160]+", weights = ~MARSUPWT, type = "JK1", scale = 4/160)#, rscales = rep(1, 160), mse = TRUE)
avg_wages <-svyby(~WSAL_VAL,~A_AGE,design = svy, svymean) 
w <- c(avg_wages[1:81,2],rep(avg_wages[81,2],4),rep(avg_wages[82,2],15))*1.311 # Fringe rate in 2023: 31% among civilian workers https://www.bls.gov/news.release/archives/ecec_06162023.pdf
productivities <- as.data.frame(cbind(age=0:99, w))

jpeg(paste0("output/productivities.jpeg"), width = 5, height = 3.5, units = "in", res = 500)
ggplot(data=productivities)+geom_point(aes(x=age,y=w/1000))+theme_light()+scale_y_continuous("Average productivity ($, thousands)")+
  labs(title="Productivity by age, 2023 USD")
dev.off()

save(c.NH, c.NR, c.ND, c.CH, c.CR, c.CD, c.FH, c.FR, c.FD, c.nonhealth,
     u.NOH, u.NOR, u.NQH, u.NQR, u.COH, u.COR, u.CQH, u.CQR, u.FOH, u.FOR, u.FQH, u.FQR, 
     u.NOD, u.NQD, u.COD, u.CQD, u.FOD, u.FQD, u.NEH, u.NER, u.CEH, u.CER, u.FEH, u.FER, 
     u.NED, u.CED, u.FED, w, file=paste0("data/cuw_inputs_",whichgender,".RData"))

# Sources:
## Costs
# Swedler DI, Miller TR, Ali B, Waeher G, Bernstein SL. National medical expenditures by smoking status in American adults: an application of Manning's two-stage model to nationally representative data. BMJ Open. 2019 Jul 16;9(7):e026592. doi: 10.1136/bmjopen-2018-026592. PMID: 31315859; PMCID: PMC6661572.
# Price Index for health care services Table 2.3.4 Line 16: Bureau of Economic Analysis - https://apps.bea.gov/histdatacore/fileStructDisplay.html?theID=12000&HMI=7&oldDiv=National%20Accounts&year=2024&quarter=,%20Q1&ReleaseDate=May-31-2024&Vintage=Second
# Egede LE, Bishu KG, Walker RJ, Dismuke CE. Impact of diagnosed depression on healthcare costs in adults with and without diabetes: United States, 2004-2011. J Affect Disord. 2016 May;195:119-26. doi: 10.1016/j.jad.2016.02.011. Epub 2016 Feb 9. PMID: 26890289; PMCID: PMC4779740.
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4779740/pdf/nihms759011.pdf

## Consumption
# 2022: https://www.bls.gov/cex/tables/calendar-year/mean-item-share-average-standard-error.htm#rf-age 

## Utilities
# Analysis of BRFSS 2022 data based on number of healthy days (physical or mental) 
# Converts number of healthy days into utility scores by age, depression status, smoking status, gender

## Productivity
# Source: U.S. Census Bureau, Current Population Survey, 2023 Annual Social and Economic Supplement (CPS ASEC).
# Total wages by age group, both sexes combined
# https://www.census.gov/data/datasets/time-series/demo/cps/cps-asec.2023.html#list-tab-165711867
# Fringe rate in 2023: 31% among civilian workers https://www.bls.gov/news.release/archives/ecec_06162023.pdf