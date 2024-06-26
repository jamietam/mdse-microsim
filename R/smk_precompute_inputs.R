rm(list = ls())  # remove any variables in R's memory
setwd(file.path("/Users/jt936/Dropbox/GitHub/mds-microsim/"))
# setwd(file.path("/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/"))

whichgender <- "females"

# Precompute all smoking and mortality probabilities by birth cohort, and by calendar year (cy)
load(paste0("data/smk_inputs_",whichgender,".RData")) # Load all smoking and mortality inputs as matrices
cohorts=1900:2100

precompute_diag <- function(statdata, cohorts, finval) {    # transition probabilities (per cycle) by birth cohort
  precomp = matrix(nrow=100, ncol=201)
  for (bc in cohorts) {
    bc_i = bc - 1899
    precomp[1:min(100,202-bc_i),bc_i] <- diag(statdata[,bc_i:201])
    precomp[100,bc_i] <- finval
  }
  return(precomp)
}
smk_init <- precompute_diag(smk_init_cisnet,cohorts[-length(cohorts)],0) # probability to become Current smoker when Never smoker
smk_cess <- precompute_diag(smk_cess_cisnet,cohorts[-length(cohorts)],0) # probability to become Former smoker when Current smoker
p.NX <- precompute_diag(death_ns,cohorts[-length(cohorts)],1) # probability to die when Never smoker
p.CX <- precompute_diag(death_cs,cohorts[-length(cohorts)],1) # probability to die when Current smoker

# Fill in missing values with last available value for that age
for(i in 1:nrow(p.CX)){
  p.NX[i,which(is.na(p.NX[i,])):201]<-p.NX[i,(min(which(is.na(p.NX[i,])))-1)]
  p.CX[i,which(is.na(p.CX[i,])):201]<-p.CX[i,(min(which(is.na(p.CX[i,])))-1)]

  smk_init[i,which(is.na(smk_init[i,])):201]<-smk_init[i,(min(which(is.na(smk_init[i,])))-1)]
  smk_cess[i,which(is.na(smk_cess[i,])):201]<-smk_cess[i,(min(which(is.na(smk_cess[i,])))-1)]
}

# Create mortality array for 1-40 years since quitting
a_p.FX.ysq <- a_p.FX.ysq_cy <- array(NA, dim = c(100, 201, 40))

# Get former smoker relative risk of mortality compared to current smoker - See Thun (2013) & Tam (2021) Graphic Health Warnings Supplement
rr.FX.CX <- if (whichgender == "females") {
  pmin(1, 0.8613 * exp(-0.023 * (1:40)))
} else {
  pmin(1, 1.0313 * exp(-0.024 * (1:40)))
}

# Fill the array with computed p.FX values for cohorts and calendar years
p.NX_cy <- death_ns
p.CX_cy <- death_cs

for (j in 1:40) { # years since quitting
  for (age in 1:100) { # ages 0-99
    for (i in 1:201) { # birth cohort or calendar year
      a_p.FX.ysq[age, i, j] <- pmax(rr.FX.CX[j] * p.CX[age, i], p.NX[age, i]) # by birth cohort
      a_p.FX.ysq_cy[age, i, j] <- pmax(rr.FX.CX[j] * p.CX_cy[age, i], p.NX_cy[age, i]) # by calendar year
    }
  }
}


rm(smk_cess_cisnet,smk_init_cisnet,death_cs,death_fs,death_ns)

save(p.NX,p.CX,a_p.FX.ysq,smk_init,smk_cess,
     p.NX_cy,p.CX_cy,a_p.FX.ysq_cy, file=paste0("data/smk_precomputed_inputs_",whichgender,".RData"))
