rm(list = ls())  # remove any variables in R's memory
# setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim/data"))
setwd(file.path("/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/"))

whichgender <- "males"

# Precompute all smoking and mortality probabilities by birth cohort
load(paste0("data/smk_inputs_",whichgender,".RData")) # Load all smoking and mortality inputs as matrices
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
# p.FX <- precompute_diag(death_fs,cohorts[-length(cohorts)],1) # probability to die when Former smoker

# Fill in missing values with last available value for that age
for(i in 1:nrow(p.CX)){
  p.NX[i,which(is.na(p.NX[i,])):201]<-p.CX[i,(min(which(is.na(p.NX[i,])))-1)]
  p.CX[i,which(is.na(p.CX[i,])):201]<-p.CX[i,(min(which(is.na(p.CX[i,])))-1)]
  # p.FX[i,which(is.na(p.FX[i,])):201]<-p.CX[i,(min(which(is.na(p.FX[i,])))-1)]
  
  smk_init[i,which(is.na(smk_init[i,])):201]<-smk_init[i,(min(which(is.na(smk_init[i,])))-1)]
  smk_cess[i,which(is.na(smk_cess[i,])):201]<-smk_cess[i,(min(which(is.na(smk_cess[i,])))-1)]
}

# # Create array for 40 years since quitting
# p.FX.ysq = vector("list", 40)
# for (j in 1:40){
#   p.FX.ysq[[j]] = matrix(, nrow = 100, ncol = 201)
# }
# # Get former smoker relative risk of mortality compared to current smoker - See Thun (2013) & Tam (2021) Graphic Health Warnings Supplement
# if (whichgender=="females"){
#   rr.FX.CX <- pmin(1,0.8613*exp(-0.023*(1:40)))
# }
# if (whichgender=="males"){
#   rr.FX.CX <- pmin(1,1.0313*exp(-0.024*(1:40)))
# }
# for (j in 1:40){
#   for (age in 1:100){
#     for (bc in 1:201){
#       p.FX.ysq[[j]][age,bc]=pmax(rr.FX.CX[j]*p.CX[age,bc],p.NX[age,bc]) 
#     }
#   }
# }

# Format mortality by years since quit as an array instead of a list because it runs faster
a_p.FX.ysq <- array(NA, dim = c(100,201,40))
for (i in 1: 40){
  a_p.FX.ysq[,,i] <-  p.FX.ysq[[i]]
}

rm(smk_cess_cisnet,smk_init_cisnet,death_cs,death_fs,death_ns)
save(p.NX,p.CX,a_p.FX.ysq,smk_init,smk_cess,file=paste0("data/smk_precomputed_inputs_",whichgender,".RData"))
