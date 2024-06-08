library(openxlsx)

## Cost inputs
bea2024 <- read.xlsx("/Users/jt936/Dropbox/GitHub/mds-microsim/data/BEA_consumption_2024Section2all_xls.xlsx",sheet = "T20304-A",startRow=7,colNames=TRUE)[16,paste0(2015:2023)] # row 16 = health care services
bea2024 <- sapply(bea2024, as.numeric)
# data from Swedler et al at 2015 USD, inflate to 2023 USD
c.NH <- c.NR <- c.ND <- max(bea2024)/min(bea2024)*c(rep(0,20),rep(2743, 10), rep(3214,10),rep(3763,10),rep(4401,10),rep(5143,10), rep(6007, 10), rep(7010,20)) # cost of remaining one cycle Never Smoking, No MD
c.CH <- c.CR <- c.CD <- max(bea2024)/min(bea2024)*c(rep(0,20),rep(2909, 10), rep(3413,10),rep(4000,10),rep(4683,10),rep(5478,10), rep(6403, 10), rep(7479,20)) # cost of remaining one cycle Current Smoking, No MD
c.FH <- c.FR <- c.FD <- max(bea2024)/min(bea2024)*c(rep(0,20),rep(3208, 10), rep(3754,10),rep(4390,10),rep(5130,10),rep(5990,10), rep(6990, 10), rep(8153,20)) # cost of remaining one cycle Former Smoking, No MD

## Consumption inputs in 2023 USD
bls2022 <- read.xlsx("/Users/jt936/Dropbox/GitHub/mds-microsim/data/reference-person-age-ranges-2022.xlsx",startRow=2,colNames=TRUE)
colnames(bls2022) <- c("Item","18.99", "18.24","25.34","35.44","45.54", "55.64","65.99","65.74","75.99") # assume no consumption among children, start at age 18
totalexp <- as.numeric(bls2022[40,c(3:7,9,10)]) # average consumer expenditures across all categories by age
healthcare <- as.numeric(bls2022[444,c(3:7,9,10)]) # health care expenditures by age
nonhealthcare <- totalexp-healthcare
# data from BLS is 2022 USD, inflate to 2023 USD
bea2024.total <- read.xlsx("/Users/jt936/Dropbox/GitHub/mds-microsim/data/BEA_consumption_2024Section2all_xls.xlsx",sheet = "T20304-A",startRow=7,colNames=TRUE)[1,paste0(2022:2023)] # row 16 = health care services
c.nonhealth <- max(bea2024.total)/min(bea2024.total)*c(rep(0,18),rep(nonhealthcare[1],7),rep(nonhealthcare[2],10),rep(nonhealthcare[3],10),rep(nonhealthcare[4],10),rep(nonhealthcare[5],10),rep(nonhealthcare[6],10),rep(nonhealthcare[7],25))

## Utility inputs

# females
u.NH <- u.ND <- u.NR <- c(rep(1,25), rep(0.85,5),rep(0.84,5), rep(0.83,5), rep(0.81,5), rep(0.79,5), rep(0.76,5), rep(0.74,5), rep(0.72,5), rep(0.72,5), rep(0.71,5), rep(0.67,5), rep(0.63,5), rep(0.55,15))
u.CH <- u.CD <- u.CR <- c(rep(1,25), rep(0.79,5),rep(0.79,5), rep(0.78,5), rep(0.76,5), rep(0.73,5), rep(0.71,5), rep(0.68,5), rep(0.66,5), rep(0.67,5), rep(0.65,5), rep(0.61,5), rep(0.58,5), rep(0.50,15))
u.FH <- u.FD <- u.FR <- c(rep(1,25), rep(0.83,5),rep(0.83,5), rep(0.82,5), rep(0.80,5), rep(0.77,5), rep(0.75,5), rep(0.72,5), rep(0.70,5), rep(0.71,5), rep(0.69,5), rep(0.65,5), rep(0.62,5), rep(0.54,15))

# # males
# u.NH <- u.ND <- u.NR <- c(rep(1,25), rep(0.87,5),rep(0.86,5), rep(0.85,5), rep(0.83,5), rep(0.81,5), rep(0.79,5), rep(0.76,5), rep(0.74,5), rep(0.75,5), rep(0.73,5), rep(0.69,5), rep(0.65,5), rep(0.57,15))
# u.CH <- u.CD <- u.CR <- c(rep(1,25), rep(0.82,5),rep(0.81,5), rep(0.80,5), rep(0.78,5), rep(0.76,5), rep(0.73,5), rep(0.71,5), rep(0.69,5), rep(0.69,5), rep(0.68,5), rep(0.64,5), rep(0.60,5), rep(0.52,15))
# u.FH <- u.FD <- u.FR <- c(rep(1,25), rep(0.86,5),rep(0.85,5), rep(0.84,5), rep(0.82,5), rep(0.80,5), rep(0.77,5), rep(0.75,5), rep(0.73,5), rep(0.73,5), rep(0.72,5), rep(0.68,5), rep(0.64,5), rep(0.56,15))

# # Productivity inputs in 2023 USD
w <- c(rep(0,15), rep(24110,10),rep(52730,5),rep(64890,5),rep(71680,5),rep(77270,5),rep(79000,5),rep(79150,5), rep(73290,5),rep(64520,5),rep(53050,5),rep(49260,5),rep(41030,25))
# w.NH <- w.ND <- w.NR <- w.CH <- w.CD <- w.CR <- w.FH <- w.FD <- w.FR <- w

save(c.NH, c.NR, c.ND, c.CH, c.CR, c.CD, c.FH, c.FR, c.FD, c.nonhealth,
     u.NH, u.NR, u.ND, u.CH, u.CR, u.CD, u.FH, u.FR, u.FD, 
     w, file=paste0("data/cuw_inputs_",whichgender,".RData"))


# Sources:
## Costs
# Swedler DI, Miller TR, Ali B, Waeher G, Bernstein SL. National medical expenditures by smoking status in American adults: an application of Manning's two-stage model to nationally representative data. BMJ Open. 2019 Jul 16;9(7):e026592. doi: 10.1136/bmjopen-2018-026592. PMID: 31315859; PMCID: PMC6661572.
# Price Index for health care services Table 2.3.4 Line 16: Bureau of Economic Analysis - https://apps.bea.gov/histdatacore/fileStructDisplay.html?theID=12000&HMI=7&oldDiv=National%20Accounts&year=2024&quarter=,%20Q1&ReleaseDate=May-31-2024&Vintage=Second


## Consumption
# 2022: https://www.bls.gov/cex/tables/calendar-year/mean-item-share-average-standard-error.htm#rf-age 
## Utilities
# Xu X, Fiacco L, Rostron B, et al. Assessing quality-adjusted years of life lost associated with exclusive cigarette smoking and smokeless tobacco use. 
# Preventive medicine. 2021/09/01/ 2021;150:106707. doi:https://doi.org/10.1016/j.ypmed.2021.106707

## Productivity
# Source: U.S. Census Bureau, Current Population Survey, 2023 Annual Social and Economic Supplement (CPS ASEC).
# Total mean income by age group, both sexes combined