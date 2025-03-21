mainDir <- "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
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
c.D <- bea2024["2023"]/bea2024["2014"]*2654 # incremental cost of depression 

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
  u.NH <- u.NR <- c(rep(1,25), rep(0.85,5),rep(0.84,5), rep(0.83,5), rep(0.81,5), rep(0.79,5), rep(0.76,5), rep(0.74,5), rep(0.72,5), rep(0.72,5), rep(0.71,5), rep(0.67,5), rep(0.63,5), rep(0.55,15))
  u.CH <- u.CR <- c(rep(1,25), rep(0.79,5),rep(0.79,5), rep(0.78,5), rep(0.76,5), rep(0.73,5), rep(0.71,5), rep(0.68,5), rep(0.66,5), rep(0.67,5), rep(0.65,5), rep(0.61,5), rep(0.58,5), rep(0.50,15))
  u.FH <- u.FR <- c(rep(1,25), rep(0.83,5),rep(0.83,5), rep(0.82,5), rep(0.80,5), rep(0.77,5), rep(0.75,5), rep(0.72,5), rep(0.70,5), rep(0.71,5), rep(0.69,5), rep(0.65,5), rep(0.62,5), rep(0.54,15))
} else {
  # males
  u.NH <- u.NR <- c(rep(1,25), rep(0.87,5),rep(0.86,5), rep(0.85,5), rep(0.83,5), rep(0.81,5), rep(0.79,5), rep(0.76,5), rep(0.74,5), rep(0.75,5), rep(0.73,5), rep(0.69,5), rep(0.65,5), rep(0.57,15))
  u.CH <- u.CR <- c(rep(1,25), rep(0.82,5),rep(0.81,5), rep(0.80,5), rep(0.78,5), rep(0.76,5), rep(0.73,5), rep(0.71,5), rep(0.69,5), rep(0.69,5), rep(0.68,5), rep(0.64,5), rep(0.60,5), rep(0.52,15))
  u.FH <- u.FR <- c(rep(1,25), rep(0.86,5),rep(0.85,5), rep(0.84,5), rep(0.82,5), rep(0.80,5), rep(0.77,5), rep(0.75,5), rep(0.73,5), rep(0.73,5), rep(0.72,5), rep(0.68,5), rep(0.64,5), rep(0.56,15))
}
u.ND <- u.NH + c(rep(0,18),rep(-0.22,8),rep(-0.29,19),rep(-0.35,20),rep(-0.36,35)) # apply disutility of depression among non-smoking persons MEPS 2003
u.FD <- u.FH + c(rep(0,18),rep(-0.22,8),rep(-0.29,19),rep(-0.35,20),rep(-0.36,35)) # apply disutility of depression among non-smoking persons MEPS 2003
u.CD <- u.CH + c(rep(0,18),rep(-0.27,8),rep(-0.42,19),rep(-0.36,20),rep(-0.22,35)) # apply disutility of depression among current smoking persons MEPS 2003

utilities <- as.data.frame(rbind(cbind(0:99,u.NH,"N",whichgender,"HR"),cbind(0:99,u.CH,"C",whichgender,"HR"),cbind(0:99,u.FH,"F",whichgender,"HR"), cbind(0:99,u.ND,"N",whichgender,"D"),cbind(0:99,u.CD,"C",whichgender,"D"),cbind(0:99,u.FD,"F",whichgender,"D")))
names(utilities) <- c("age","utility","smoking_status","gender","depression_status")
utilities$age <- as.numeric(utilities$age)
utilities$utility <- as.numeric(utilities$utility)
utilities$smoking_status <- factor(utilities$smoking_status,levels = c("C","N","F"))
utilities$depression_status <- factor(utilities$depression_status,levels = c("HR","D"))

jpeg(paste0("output/utilities.jpeg"), width = 5, height = 3.5, units = "in", res = 500)
ggplot(data=utilities)+geom_line(aes(x=age,y=utility,colour = smoking_status,linetype = depression_status)) +
  scale_y_continuous(limits = c(0,1))+
  labs(title="Utilities by smoking and depression status")
dev.off()


# Productivities ----------------------------------------------------------
ppcps<-read.csv("data-raw/asecpub23csv/pppub23.csv")
cps<-read.csv("data-raw/asecpub23csv/asec_csv_repwgt_2023.csv")
cps$PH_SEQ <- cps$h_seq
mergedcps <- merge(cps,ppcps,by = c("PH_SEQ","PPPOS"))
svy <- svrepdesign(data = mergedcps, repweights = "pwwgt[0-160]+", weights = ~MARSUPWT, type = "JK1", scale = 4/160)#, rscales = rep(1, 160), mse = TRUE)
avg_wages <-svyby(~WSAL_VAL,~A_AGE,design = svy, svymean) 
w = c(avg_wages[1:81,2],rep(avg_wages[81,2],4),rep(avg_wages[82,2],15))*1.311 # Fringe rate in 2023: 31% among civilian workers https://www.bls.gov/news.release/archives/ecec_06162023.pdf
productivities <- as.data.frame(cbind(age=0:99, w))

# avg_income_cat <-svyby(~PTOTVAL,~AGE1,design = svy, svymean) 
# w_old <- c(rep(0,15), rep(24110,10),rep(52730,5),rep(64890,5),rep(71680,5),rep(77270,5),rep(79000,5),rep(79150,5), rep(73290,5),rep(64520,5),rep(53050,5),rep(49260,5),rep(41030,25))
# w_cat <- c(avg_income_cat[1:81,2],rep(avg_income_cat[81,2],4),rep(avg_income_cat[82,2],15))

jpeg(paste0("output/productivities.jpeg"), width = 5, height = 3.5, units = "in", res = 500)
ggplot(data=productivities)+geom_point(aes(x=age,y=earnings/1000))+theme_light()+scale_y_continuous("Average productivity ($, thousands)")+
  labs(title="Productivity by age, 2023 USD")
dev.off()

save(c.NH, c.NR, c.ND, c.CH, c.CR, c.CD, c.FH, c.FR, c.FD, c.nonhealth,
     u.NH, u.NR, u.ND, u.CH, u.CR, u.CD, u.FH, u.FR, u.FD, 
     w, file=paste0("data/cuw_inputs_",whichgender,".RData"))

# Sources:
## Costs
# Swedler DI, Miller TR, Ali B, Waeher G, Bernstein SL. National medical expenditures by smoking status in American adults: an application of Manning's two-stage model to nationally representative data. BMJ Open. 2019 Jul 16;9(7):e026592. doi: 10.1136/bmjopen-2018-026592. PMID: 31315859; PMCID: PMC6661572.
# Price Index for health care services Table 2.3.4 Line 16: Bureau of Economic Analysis - https://apps.bea.gov/histdatacore/fileStructDisplay.html?theID=12000&HMI=7&oldDiv=National%20Accounts&year=2024&quarter=,%20Q1&ReleaseDate=May-31-2024&Vintage=Second
# Egede LE, Bishu KG, Walker RJ, Dismuke CE. Impact of diagnosed depression on healthcare costs in adults with and without diabetes: United States, 2004-2011. J Affect Disord. 2016 May;195:119-26. doi: 10.1016/j.jad.2016.02.011. Epub 2016 Feb 9. PMID: 26890289; PMCID: PMC4779740.
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4779740/pdf/nihms759011.pdf

## Consumption
# 2022: https://www.bls.gov/cex/tables/calendar-year/mean-item-share-average-standard-error.htm#rf-age 

## Utilities
# Xu X, Fiacco L, Rostron B, et al. Assessing quality-adjusted years of life lost associated with exclusive cigarette smoking and smokeless tobacco use. 
# Preventive medicine. 2021/09/01/ 2021;150:106707. doi:https://doi.org/10.1016/j.ypmed.2021.106707
# AND
# analysis of Medical Expenditure Panel Survey (MEPS) 2003 examining disutility of depression among smoking vs. non-smoking persons

## Productivity
# Source: U.S. Census Bureau, Current Population Survey, 2023 Annual Social and Economic Supplement (CPS ASEC).
# Total mean income by age group, both sexes combined
# https://www.census.gov/data/datasets/time-series/demo/cps/cps-asec.2023.html#list-tab-165711867