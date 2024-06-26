setwd(file.path("/Users/JT936/Dropbox/GitHub/mds-microsim/"))

whichgender = "females"

# Population
pop <- read.csv("data/np2023_d1_mid.csv") # Read in Census data population projections 2022-2100 by single year of age

# Filter based on gender
gender_code <- if (whichgender == "females") 2 else 1
pop <- subset(pop, SEX == gender_code & ORIGIN == 0 & RACE == 0)

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

# from 2022-2100
save(pop, file=paste0("data/pop_",whichgender,".RData"))