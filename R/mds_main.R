main = function(v.params) { # v.params: run model for parameter calibration
  
  t_init <- Sys.time() # Start timer
  
  s.NC_9.17 <- ifelse(calib_inputs["s.NC_9.17","calib"]==1,v.params["s.NC_9.17"],calib_inputs["s.NC_9.17","value"])
  s.NC_18.25 <- ifelse(calib_inputs["s.NC_18.25","calib"]==1,v.params["s.NC_18.25"],calib_inputs["s.NC_18.25","value"])

  s.CF_18.25 <- ifelse(calib_inputs["s.CF_18.25","calib"]==1,v.params["s.CF_18.25"],calib_inputs["s.CF_18.25","value"])
  s.CF_26.34 <- ifelse(calib_inputs["s.CF_26.34","calib"]==1,v.params["s.CF_26.34"],calib_inputs["s.CF_26.34","value"])
  s.CF_35.49 <- ifelse(calib_inputs["s.CF_35.49","calib"]==1,v.params["s.CF_35.49"],calib_inputs["s.CF_35.49","value"])
  s.CF_50.64 <- ifelse(calib_inputs["s.CF_50.64","calib"]==1,v.params["s.CF_50.64"],calib_inputs["s.CF_50.64","value"])
  s.CF_65.99 <- ifelse(calib_inputs["s.CF_65.99","calib"]==1,v.params["s.CF_65.99"],calib_inputs["s.CF_65.99","value"])

  s.HD_12.17 <-  ifelse(calib_inputs["s.HD_12.17","calib"]==1,v.params["s.HD_12.17"],calib_inputs["s.HD_12.17","value"])
  s.HD_18.25 <-  ifelse(calib_inputs["s.HD_18.25","calib"]==1,v.params["s.HD_18.25"],calib_inputs["s.HD_18.25","value"])

  rr.DX_18.25 <- ifelse(calib_inputs["rr.DX_18.25","calib"]==1,v.params["rr.DX_18.25"],calib_inputs["rr.DX_18.25","value"])
  rr.DX_26.34 <- ifelse(calib_inputs["rr.DX_26.34","calib"]==1,v.params["rr.DX_26.34"],calib_inputs["rr.DX_26.34","value"])
  rr.DX_35.49 <- ifelse(calib_inputs["rr.DX_35.49","calib"]==1,v.params["rr.DX_35.49"],calib_inputs["rr.DX_35.49","value"])
  rr.DX_50.64 <- ifelse(calib_inputs["rr.DX_50.64","calib"]==1,v.params["rr.DX_50.64"],calib_inputs["rr.DX_50.64","value"])
  rr.DX_65.99 <- ifelse(calib_inputs["rr.DX_65.99","calib"]==1,v.params["rr.DX_65.99"],calib_inputs["rr.DX_65.99","value"])

  rr.ND.CD <- ifelse(calib_inputs["rr.ND.CD","calib"]==1,v.params["rr.ND.CD"],calib_inputs["rr.ND.CD","value"])
  rr.CH.CD <- ifelse(calib_inputs["rr.CH.CD","calib"]==1,v.params["rr.CH.CD"],calib_inputs["rr.CH.CD","value"])
  rr.CR.CD <- ifelse(calib_inputs["rr.CR.CD","calib"]==1,v.params["rr.CR.CD"],calib_inputs["rr.CR.CD","value"])
  rr.CD.FD <- ifelse(calib_inputs["rr.CD.FD","calib"]==1,v.params["rr.CD.FD"],calib_inputs["rr.CD.FD","value"])

  p.DR=NULL
  p.DR[1:12] <- p.DR[100] <- 0 # probability to recover, final value = 0 because mortality prob = 1
  p.DR[13:99] <- ifelse(calib_inputs["p.DR","calib"]==1,v.params["p.DR"],calib_inputs["p.DR","value"]) # assumes constant recovery by age
  
  ## Incidence
  for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
    bc1 = bc-1899
    for (age in 0:25){ # increase applies to youth and young adults ages 0-25
      if ((bc+age)>=2016 & age>=18){ # starting in 2016
        p.HD[(age+1),bc1] = s.HD_18.25*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=2016 & age<18){
        p.HD[(age+1),bc1] = s.HD_12.17*p.HD[(age+1),bc1]
      }
    }
  }
  ## Initiation - No initiation after 25
  p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))
  
  ## Cessation - No cessation before 18
  p.CF = smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
  
  rr.DX = c(rep(1,18),rep(rr.DX_18.25,8),rep(rr.DX_26.34,9),rep(rr.DX_35.49,15),rep(rr.DX_50.64,15),rep(rr.DX_65.99,34),1)
  
  # Simulate for each birth cohort with parallelization: row = each person within birth cohort, columns = ages 0:99
  m.M <-foreach (i=cohorts, .combine='rbind', .packages='darthtools',
                          .export=c('mds_microsim','probs','get_prevs',
                                    'n.i','n.t','v.n','n.s','v.M_1',
                                    'p.NC','p.CF','p.NX','p.CX','a_p.FX.ysq',
                                    'rr.DX','p.HD', 'p.DR', 'p.RD',
                                    'rr.ND.CD','rr.CH.CD','rr.CR.CD','rr.CD.FD')) %dopar% {
                                      mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
  }
  # run in serial for debugging:
  # m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Calculate costs, utilities, and productivity at each person's age
  m.C <- m.U <- m.W <- matrix(nrow = n.i*length(cohorts), ncol = n.t + 1, 
                              dimnames = list(paste(rep(cohorts,each=n.i), 1:n.i, sep = "."), # each individual, year of birth
                                              paste(0:n.t, sep = " ")))  
  for (t in 1:n.t) {
    m.C[, t] <- costs(m.M[, t],t)
    m.U[, t] <- utils(m.M[, t],t)
    m.W[, t] <- prods(m.M[, t],t)
  }
  
  # Convert matrix from cohort-age to cohort-calendaryear (cy)
  m.M_cy <- m.C_cy <- m.U_cy <- m.W_cy <- matrix(nrow = n.i*length(cohorts), ncol = (length(cohorts)+100))
  for (b in 1:length(cohorts)){
    m.M_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.M[(n.i*(b-1)+1):(n.i*b),]
    m.C_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.C[(n.i*(b-1)+1):(n.i*b),]
    m.U_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.U[(n.i*(b-1)+1):(n.i*b),]
    m.W_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.W[(n.i*(b-1)+1):(n.i*b),]
  }
  colnames(m.M_cy) <- colnames(m.C_cy) <- colnames(m.U_cy) <- colnames(m.W_cy) <- c(min(cohorts):(max(cohorts)+100))

  # Output prevalence results as a list
  model_res <- lapply(c("N","C","F","D"), get_prevs, m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  model_res <- c(model_res, lapply(c("ND","CD","FD"), get_subgroup_prevs, denom="D",m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts))) # denominator is everyone in "D" subpopulation
  names(model_res) <- c("N","C","F","D","ND","CD","FD")
  for (l in 1:length(model_res)){
    model_res[[l]] <- model_res[[l]][order(model_res[[l]][,"age"],decreasing=FALSE),] # re-order the age groups from 18.25, 18.99, 26.34, etc
  }
  
  # Calculate costs, utilities, and productivity for each health state by age in the population
  total_cuw <- rbind(colSums(m.C_cy,na.rm=TRUE),colSums(m.U_cy,na.rm=TRUE),colSums(m.W_cy,na.rm=TRUE))
  rownames(total_cuw)<-c("total costs","total QALYs","total productivity")
  colnames(total_cuw) <-colnames(m.C_cy) <- colnames(m.U_cy) <- colnames(m.W_cy) <- c(min(cohorts):(max(cohorts)+100))
  
  # Assume 2024 is the starting year for discounting purposes, calculate discount weight based on the discount rate d.c
  v.d = c(rep(1,2024-1900), c(1 / (1 + d.c) ^ (0:(ncol(m.C_cy)-(2024-1899))))) # vector of discount weights
  
  # Total discounted costs, QALYs, and productivity
  d_total_cuw <- total_cuw %*% diag(v.d) # multiple each row of the cuw matrix by the discounting vector
  colnames(d_total_cuw) <-colnames(total_cuw)
  model_res$cuw <- t(d_total_cuw)
  
  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer
  return(model_res)
}

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to calibration algorithm
f_gof <- function(v.params){
  
  model_res <- main(v.params)   # Run model for parameter set "v.params"
  v.GOF <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
  
  # Calibrate to N, C, F, D and ND/D, CD/D, FD/D prevalences
  for (r in 1:length(lst_calibtargets)){ # sum of squared differences
    # for (r in 1:4){
    gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
    v.GOF[r] <-gof
  }
  
  # OVERALL
  v.weights <- rep(1,n.target) # can assign targets different weights
  # weighted sum
  GOF_overall <- sum(v.GOF[1:n.target] * v.weights)
  cat(GOF_overall)
  # return GOF
  return(GOF_overall)
}


## For MPI ONLY: 
## Put this at beginning:
# library(doMPI) ## Run on multiple nodes
# cl<-startMPIcluster()
# registerDoMPI(cl)
## Put this at the end of the R script:
# closeCluster(cl)
# mpi.quit()
############################################################################################
## The microsimulation model code was adapted from the DARTH workgroup (www.darthworkgroup.com). 
# 	See Appendix A of the article: 
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
#   Microsimulation modeling for health decision sciences using R: A tutorial. 
#   Med Decis Making. 2018;38(3):400-22.
# For more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################
## The calibration code was adapted from the DARTH workgroup (www.darthworkgroup.com). 
# - Alarid-Escudero F, Maclehose RF, Peralta Y, Kuntz KM, Enns EA. 
#   Non-identifiability in model calibration and implications for 
#   medical decision making. Med Decis Making. 2018; 38(7):810-821.
# - Jalal H, Pechlivanoglou P, Krijkamp E, Alarid-Escudero F, Enns E, 
#   Hunink MG. An Overview of R in Health Decision Sciences. 
#   Med Decis Making. 2017; 37(3): 735-746. 
# For more information: https://darth-git.github.io/calibSMDM2018-materials/
############################################################################################
# Latin Hypercube Sampling Code: https://lhs.r-forge.r-project.org/lhs_questions.html
# Fit by age group # gof <- sum((lst_targets[[r]][lst_targets[[r]][,"age"]<=27,][,"prev"] - model_res[[r]][model_res[[r]][,"age"]<=27,][,"prev"])^2) # only fit to ages groups 18.25, 18.99, 26.34
