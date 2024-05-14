library("readxl")

# Mortality
# get_prevs_by_age

setwd(file.path("/Users/john/Documents/Yale/YSPH Research/mds-microsim-policy/"))

# Run model
model_res<-main.byage(v.params, list(initeff = matrix(1, nrow = dim(smk_init)[1], ncol = dim(smk_init)[2]), 
                               cesseff = matrix(1, nrow = dim(smk_cess)[1], ncol = dim(smk_cess)[2])))


# Population
pop <- read.csv("data/np2023_d1_mid.csv")
pop_f <- pop[pop$SEX == 2 & pop$ORIGIN == 0 & pop$RACE == 0,]
pop_m <- pop[pop$SEX == 1 & pop$ORIGIN == 0 & pop$RACE == 0,]

# Cleaning/formatting dataframe to include total population for each gender by year
pop_f <- t(pop_f[, -c(1:3,5)])
colnames(pop_f) <- c(2022:2100)
pop_f <- pop_f[-1,]
pop_f[100,] <- pop_f[100,] + pop_f[101,]
pop_f <- pop_f[-101,]
rownames(pop_f) <- c(0:99)

pop_m <- t(pop_m[, -c(1:3,5)])
colnames(pop_m) <- c(2022:2100)
pop_m <- pop_m[-1,]
pop_m[100,] <- pop_m[100,] + pop_m[101,]
pop_m <- pop_m[-101,]
rownames(pop_m) <- c(0:99)



# Mortality Rates
deathrates_cs_f <- read_excel("data/cisnet_deathrates.xlsx", sheet = "cs_females")
deathrates_ns_f <- read_excel("data/cisnet_deathrates.xlsx", sheet = "ns_females")
deathrates_fs_f <- read_excel("data/cisnet_deathrates.xlsx", sheet = "fs_females")
deathrates_cs_m <- read_excel("data/cisnet_deathrates.xlsx", sheet = "cs_males")
deathrates_ns_m <- read_excel("data/cisnet_deathrates.xlsx", sheet = "ns_males")
deathrates_fs_m <- read_excel("data/cisnet_deathrates.xlsx", sheet = "fs_males")


# Remove the age column
deathrates_cs_f <- deathrates_cs_f[,-1]
deathrates_ns_f <- deathrates_ns_f[,-1]
deathrates_fs_f <- deathrates_fs_f[,-1]
deathrates_cs_m <- deathrates_cs_m[,-1]
deathrates_ns_m <- deathrates_ns_m[,-1]
deathrates_fs_m <- deathrates_fs_m[,-1]

age_range <- 18:99
year_range <- 2022:2100

# Matching dimensions to the prev matrices.
deathrates_cs_f <- deathrates_cs_f[as.character(age_range+1), as.character(year_range)]
deathrates_fs_f <- deathrates_fs_f[as.character(age_range+1), as.character(year_range)]
deathrates_ns_f <- deathrates_ns_f[as.character(age_range+1), as.character(year_range)]
deathrates_cs_m <- deathrates_cs_m[as.character(age_range+1), as.character(year_range)]
deathrates_fs_m <- deathrates_fs_m[as.character(age_range+1), as.character(year_range)]
deathrates_ns_m <- deathrates_ns_m[as.character(age_range+1), as.character(year_range)]

pop_f <- pop_f[as.character(age_range), as.character(year_range)]
pop_m <- pop_m[as.character(age_range), as.character(year_range)]

# The gender for this depends on what the input is into the main model
prev_cs <- xtabs(prev ~ age + year, data = model_res$C)
attr(prev_cs, "class") <- NULL
attr(prev_cs, "call") <- NULL

prev_fs <- xtabs(prev ~ age + year, data = model_res$F)
attr(prev_fs, "class") <- NULL
attr(prev_fs, "call") <- NULL

prev_cs <- prev_cs[as.character(age_range), as.character(year_range)]
prev_fs <- prev_fs[as.character(age_range), as.character(year_range)]

# For females
female_SAD <- colSums(pop_f * (prev_cs * (deathrates_cs_f - deathrates_ns_f) + prev_fs * (deathrates_fs_f - deathrates_ns_f)))

# For males
male_SAD <- colSums(pop_m * (prev_cs * (deathrates_cs_m - deathrates_ns_m) + prev_fs * (deathrates_fs_m - deathrates_ns_m)))
