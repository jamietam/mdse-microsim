setwd(file.path("/Users/JT936/Dropbox/GitHub/mdse-microsim/"))

whichgender = "females"
perc_sex <- ifelse(whichgender=="females", 100/(105+100), 105/(105+100)) # human sex ratio is 1.05 favoring males https://genderdata.worldbank.org/en/indicator/sp-pop-brth-mf?utm_source=chatgpt.com

# Population
pop <- read.csv("data-raw/np2023_d1_mid.csv") # Read in Census data population projections 2022-2100 by single year of age
sex_code <- if (whichgender == "females") 2 else 1 # Filter based on sex
pop <- subset(pop, SEX == sex_code & ORIGIN == 0 & RACE == 0)

# Births by sex from 1900-2021
births_past <- read.csv("data-raw/NCHS_-_Births_and_General_Fertility_Rates__United_States.csv",header = TRUE)[,1:2]
births_past$Birth.Number <- as.numeric(gsub(",","",births_past$Birth.Number)) # remove commas
births_past <- rbind(as.data.frame(cbind(Year=1900:1908,Birth.Number=births_past[1,2])),births_past) # duplicate births for 1900-1908
births_past <- rbind(births_past,as.data.frame(cbind(Year=2019:2021,Birth.Number=c(3747540,3613647,3664292))))
births_past[,whichgender] <- births_past$Birth.Number*perc_sex
rownames(births_past) <- births_past$Year
# Births by sex from 2022-2100
births_proj <- as.data.frame(pop[,c("YEAR","POP_0")])
births_proj[,whichgender] <- births_proj$POP_0
rownames(births_proj) <- births_proj$YEAR

births <- as.matrix(c(births_past[, whichgender], births_proj[, whichgender]))
rownames(births) <- 1900:2100                      

# Cleaning/formatting dataframe to include total population for each gender by year
pop <- t(pop[, -c(1:3, 5)])
colnames(pop) <- 2022:2100
pop <- pop[-1,]
pop[100, ] <- pop[100, ] + pop[101, ]
pop <- pop[-101,]
rownames(pop) <- 0:99

# Select specific age and year ranges
age_range <- 0:99
year_range <- 2022:2100
pop <- pop[as.character(age_range), as.character(year_range)]

save(pop, births, file=paste0("data/pop_",whichgender,".RData"))
