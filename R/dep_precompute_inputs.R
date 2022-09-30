setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim"))
library(openxlsx)

whichgender <- "males"

# Precompute annual mortality probabilities by birth cohort
hmd <- as.matrix(read.xlsx("data/hmd_mortality_age_year.xlsx",sheet=paste0(whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE)) # Load all mortality inputs as matrices

cohorts <- 1900:2100

precompute_diag <- function(hmd, cohorts, finval) { 
  precomp = matrix(nrow=100, ncol=201)
  for (bc in cohorts) {
    ## Transition probabilities (per cycle) by birth cohort
    bc_i = bc - 1899
    precomp[1:min(100,202-bc_i),bc_i] <- diag(hmd[,bc_i:201])
    precomp[100,bc_i] <- finval
  }
  return(precomp)
}

p.HX <- precompute_diag(hmd,cohorts[-length(cohorts)],1) 

# format depression inputs
p.HD = matrix(nrow=100, ncol=201)
p.HD[,1:201] = as.matrix(read.xlsx("data/incidence_eaton.xlsx",sheet=whichgender,rowNames=TRUE, colNames=FALSE))
p.HD[0:12,] = 0 # assume no depressive episodes before age 12
p.DR = c(rep(0.173,99),0) # probability to recover, final value = 0 because mortality prob = 1
p.RD = c(rep(0.058,99),0) # probability of recurrent MD if Former MD or Recall Error (R, E) 
# p.RU = c(rep(0,25),rep(0.152,9),rep(0.101,15),rep(0.120,15),rep(0.923,35),0) # probability to Recall Error (E) when Former MD (R) -- needs to be recalibrated to avoid negative probabilities 


save(p.HX, p.HD, p.DR, p.RD, file=paste0("data/dep_precomputed_inputs_",whichgender,".RData"))
