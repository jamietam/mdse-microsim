setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim/data"))

whichgender <x- "females"

# Precompute all smoking and mortality probabilities by birth cohort
load(paste0(here("data/smk_inputs_"),whichgender,".RData")) # Load all smoking and mortality inputs as matrices
cohorts=1900:2100

precompute_diag <- function(statdata, cohorts, finval) { 
  precomp = matrix(nrow=100, ncol=201)
  for (bc in cohorts) {
    ## Transition probabilities (per cycle) by birth cohort
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
p.FX <- precompute_diag(death_fs,cohorts[-length(cohorts)],1) # probability to die when Former smoker
p.CX[,121:201] <- p.CX[,120] # hold mortality rates constant from 2020 onwards
p.NX[,121:201] <- p.NX[,120] 
p.FX[,121:201] <- p.FX[,120] 

smk_init <- as.data.frame(smk_init)
smk_init[is.na(smk_init)] <- 0
smk_init <- as.matrix(smk_init)

smk_cess <- as.data.frame(smk_cess)
smk_cess[is.na(smk_cess)] <- 0
smk_cess <- as.matrix(smk_cess)

p.NX <- as.data.frame(p.NX)
p.NX[is.na(p.NX)] <- 0
p.NX <- as.matrix(p.NX)

p.CX <- as.data.frame(p.CX)
p.CX[is.na(p.CX)] <- 0
p.CX <- as.matrix(p.CX)

p.FX <- as.data.frame(p.FX)
p.FX[is.na(p.FX)] <- 0
p.FX <- as.matrix(p.FX)

rm(smk_cess_cisnet,smk_init_cisnet,death_cs,death_fs,death_ns)
save(p.NX,p.CX,p.FX,smk_init,smk_cess,file=paste0("smk_precomputed_inputs_",whichgender,".RData"))