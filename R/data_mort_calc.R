setwd(file.path("/Users/JT936/Dropbox/GitHub/mds-microsim/"))

library("readxl")

whichgender = "males"

# Population
pop <- read.csv("data/np2023_d1_mid.csv")
if (whichgender=="females"){
  pop <- pop[pop$SEX == 2 & pop$ORIGIN == 0 & pop$RACE == 0,]
}
if (whichgender=="males"){
  pop <- pop[pop$SEX == 1 & pop$ORIGIN == 0 & pop$RACE == 0,]
}

# Cleaning/formatting dataframe to include total population for each gender by year
pop <- t(pop[, -c(1:3,5)])
colnames(pop) <- c(2022:2100)
pop <- pop[-1,]
pop[100,] <- pop[100,] + pop[101,]
pop <- pop[-101,]
rownames(pop) <- c(0:99)

# Mortality Rates
deathrates_cs <- read_excel("data/cisnet_deathrates.xlsx", sheet = paste0("cs_",whichgender))
deathrates_ns <- read_excel("data/cisnet_deathrates.xlsx", sheet = paste0("ns_",whichgender))
deathrates_fs <- read_excel("data/cisnet_deathrates.xlsx", sheet = paste0("fs_",whichgender))

# Remove the age column
deathrates_cs <- deathrates_cs[,-1]
deathrates_ns <- deathrates_ns[,-1]
deathrates_fs <- deathrates_fs[,-1]

age_range <- 18:99
year_range <- 2022:2100

# Matching dimensions to the prev matrices.
deathrates_cs <- deathrates_cs[as.character(age_range+1), as.character(year_range)]
deathrates_fs <- deathrates_fs[as.character(age_range+1), as.character(year_range)]
deathrates_ns <- deathrates_ns[as.character(age_range+1), as.character(year_range)]

pop <- pop[as.character(age_range), as.character(year_range)]

# from 2022-2100
save(pop, deathrates_cs, deathrates_ns, deathrates_fs, file=paste0("data/mort_data_",whichgender,".RData"))