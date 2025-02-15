rm(list = ls())  # remove any variables in R's memory
setwd(file.path("/Users/jt936/Dropbox/GitHub/mdse-microsim/"))
# setwd(file.path("/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/"))
library(reshape2)
library(openxlsx)
whichgender = "males"

if (whichgender=="females"){ 
  # Sex = 0 is for males, 1 for females
  smk_init_bc <- subset(read.delim("data-raw/new_shg_initiation.txt", sep=",", skip = 5, header=TRUE),Sex==1)[,-c(1:39)] # remove unnecessary columns (race, sex, age) and birth cohorts (1864-1899)
  smk_cess_bc <- subset(read.delim("data-raw/new_shg_cessation.txt", sep=",", skip = 5, header=TRUE),Sex==1)[,-c(1:39)] 
  # Read in latest CISNET smoking initiation and cessation parameters by age (0:99) and birth cohort (1864:2100)
  p.NX_bc <- read.csv(paste0("data-raw/all_final_results_FEMALENever.csv"))
  p.CX_bc <- read.csv(paste0("data-raw/all_final_results_FEMALECurrent.csv"))
  p.FX_bc <- read.csv(paste0("data-raw/all_final_results_FEMALEFormer.csv"))     
  le_N <- as.matrix(read.xlsx(paste0("data-raw/cisnet_deathrates.xlsx"),sheet = "LE_ns_females")[,-1]) ## ASK RAFAEL FOR UPDATED LIFE EXPECTANCIES USING FUNCTION THAT CONVERTS MORTALITY
} else {
  smk_init_bc <- subset(read.delim("data-raw/new_shg_initiation.txt", sep=",", skip = 5, header=TRUE),Sex==0)[,-c(1:39)] # remove unnecessary columns (race, sex, age) and birth cohorts (1864-1899)
  smk_cess_bc <- subset(read.delim("data-raw/new_shg_cessation.txt", sep=",", skip = 5, header=TRUE),Sex==0)[,-c(1:39)] 
  p.NX_bc <- read.csv(paste0("data-raw/all_final_results_MALENever.csv"))
  p.CX_bc <- read.csv(paste0("data-raw/all_final_results_MALECurrent.csv"))
  p.FX_bc <- read.csv(paste0("data-raw/all_final_results_MALEFormer.csv")) 
  le_N <- as.matrix(read.xlsx(paste0("data-raw/cisnet_deathrates.xlsx"),sheet = "LE_ns_males")[,-1])
}

names(smk_init_bc) <- names(smk_cess_bc) <- c(1900:2100)
smk_cess_bc$age <- smk_init_bc$age <- c(0:99)

smk_init <- melt(smk_init_bc,id.vars =c("age"), value.name = "prob",variable.name = "birthyear") # ages = rows, calendar year = columns, and reformat as matrix
smk_init$birthyear <- as.numeric(as.character(smk_init$birthyear))
smk_init$year <- smk_init$birthyear+smk_init$age
smk_init <- acast(smk_init, age~year,value.var="prob")

smk_cess <- melt(smk_cess_bc,id.vars =c("age"), value.name = "prob",variable.name = "birthyear") # ages = rows, calendar year = columns, and reformat as matrix
smk_cess$birthyear <- as.numeric(as.character(smk_cess$birthyear))
smk_cess$year <- smk_cess$birthyear+smk_cess$age
smk_cess <- acast(smk_cess, age~year,value.var="prob")

# Create a matrix of mortality by calendar year
p.NX_bc$year <- p.NX_bc$birthyear+p.NX_bc$age
p.CX_bc$year <- p.CX_bc$birthyear+p.CX_bc$age
p.FX_bc$year <- p.FX_bc$birthyear+p.FX_bc$age

p.NX <- acast(p.NX_bc, age~year, value.var="final") # ages = rows, calendar year = columns, and reformat as matrix
p.NX <- p.NX[,-c(1:36)] # remove age column and calendar years 1864-1899 
p.NX <- p.NX[,-c(202:300)] # remove birth cohorts 2101-2199

p.CX <- acast(p.CX_bc, age~year, value.var="final") # ages = rows, calendar year = columns, and reformat as matrix
p.CX <- p.CX[,-c(1:36)] # remove age column and calendar years 1864-1899 
p.CX <- p.CX[,-c(202:300)] # remove birth cohorts 2101-2199

p.FX <- acast(p.FX_bc, age~year, value.var="final") # ages = rows, calendar year = columns, and reformat as matrix
p.FX <- p.FX[,-c(1:36)] # remove age column and calendar years 1864-1899 
p.FX <- p.FX[,-c(202:300)] # remove birth cohorts 2101-2199

colnames(smk_cess) <- colnames(smk_init) <- c(1900:2199)
  
colnames(p.NX) <- colnames(p.CX)<- colnames(p.FX) <- c(1900:2100)

# Create mortality array for 1-40 years since quitting
a_p.FX.ysq <- array(NA, dim = c(100, 201, 40))

# Get former smoker relative risk of mortality compared to current smoker - See Thun (2013) & Tam (2021) Graphic Health Warnings Supplement
rr.FX.CX <- if (whichgender == "females") {
  pmin(1, 0.8613 * exp(-0.023 * (1:40)))
} else {
  pmin(1, 1.0313 * exp(-0.024 * (1:40)))
}

# Fill the array with computed p.FX values for cohorts and calendar years
for (j in 1:40) { # years since quitting
  for (age in 1:100) { # ages 0-99
    for (i in 1:201) { # birth cohort or calendar year
      a_p.FX.ysq[age, i, j] <- pmax(rr.FX.CX[j] * p.CX[age, i], p.NX[age, i]) # by calendar year
    }
  }
}

save(p.NX,p.CX,p.FX,a_p.FX.ysq,smk_init,smk_cess,le_N, file=paste0("data/smk_precomputed_inputs_",whichgender,".RData"))