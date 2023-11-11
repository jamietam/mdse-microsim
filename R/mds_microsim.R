##################################### Functions ###########################################

## The microsim function keeps track of what happens to each individual during each cycle. 
## Arguments:  
# v.M_1:   vector of initial states for individuals 
# n.i:     number of individuals
# n.t:     total number of cycles to run the model
# v.n:     vector of health state names
# TR.out:  should the output include a microsimulation trace? (default is TRUE)
# TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
# Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
## Makes use of:
# Probs:   function for the estimation of transition probabilities
# Costs:   function for the estimation of cost state values
# Effs:    function for the estimation of state specific health outcomes (QALYs)
mds_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  set.seed(seed)                                      # set the seed for every individual for the random number generator
  
  v.ysq <- rep(n.i, 0) # vector counting how many years since quit
  
  # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- matrix(nrow = n.i, ncol = n.t + 1, 
                dimnames = list(paste(bc, 1:n.i, sep = "."), # each individual, year of birth
                                paste(0:n.t, sep = " ")))  
  m.M[, 1] <- v.M_1                                         # indicate the initial health state   
  
  for (t in 1:n.t) {
    if (bc+t>2100){ # exit for loop if going past the year 2100
      break
    }
    v.ysq <- ifelse((m.M[,t] =="FH"|m.M[,t] =="FD"|m.M[,t] =="FR" ) ,v.ysq +1 , 0)
    v.ysq <- ifelse(v.ysq>40 , 40, v.ysq)
    
    m.P <- probs(bc, t, v.ysq, m.M[, t])           # calculate the transition probabilities at cycle t 
    m.M[, t+1] <- samplev(m.P, 1)      # sample the next health state and store that state in matrix m.M 
    
  }                                                       # close the loop for the time points 
  
  if (TS.out == TRUE) {  # create a  matrix of transitions across states
    TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n.i)
    rownames(TS) <- paste(bc, 1:n.i, sep = ".")   # name the rows 
    colnames(TS) <- paste(0:n.t, sep = " ")   # name the columns (age)
  } else {
    TS <- NULL
  }
  
  if (TR.out == TRUE) { # create a trace from the individual trajectories
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
    TR <- TR / n.i                                       # create a distribution trace
    rownames(TR) <- paste(bc:(bc+n.t), sep = ".")        # name the rows by birth cohort
    colnames(TR) <- v.n                                  # name the columns 
  } else {
    TR <- NULL
  }
  
  results <- list(m.M=m.M, TS=TS, TR=TR) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


## PROBABILITY FUNCTION ----------------------------------------------------
probs <- function(bc, t, v.ysq, M_t) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_t: health state occupied by individuals at cycle t (character variable)
  # ysq: years since quitting if individual is former smoker
  
  bc1 = bc-1899

  # create matrix of state transition probabilities
  m.p_t <- matrix(data = 0, nrow = length(v.n), ncol = n.i)  
  # give the state names to the rows
  rownames(m.p_t) <-  v.n                               
  
  # update m.p_t with the probabilities conditional on survival  
  # interaction effects: rr.ND.CD, rr.CH.CD, rr.CR.CD, rr.CD.FD)
  
  # Happy
  m.p_t["NH", M_t == "NH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.HD[t,bc1])
  m.p_t["CH", M_t == "NH"] <- (1-p.NX[t,bc1])*p.NC[t,bc1]
  m.p_t["ND", M_t == "NH"] <- (1-p.NX[t,bc1])*p.HD[t,bc1]
  m.p_t["X" , M_t == "NH"] <- p.NX[t,bc1]
  
  m.p_t["CH", M_t == "CH"] <- (1-p.CX[t,bc1])*(1-rr.CH.CD*p.HD[t,bc1]-p.CF[t,bc1])
  m.p_t["FH", M_t == "CH"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
  m.p_t["CD", M_t == "CH"] <- (1-p.CX[t,bc1])*rr.CH.CD*p.HD[t,bc1]
  m.p_t["X" , M_t == "CH"] <- p.CX[t,bc1]	
  
  m.p_t["FH", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t=="FH"]])*(1 -p.HD[t,bc1])
  m.p_t["FD", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t=="FH"]])*p.HD[t,bc1]
  m.p_t["X" , M_t =="FH"] <-	a_p.FX.ysq[t, bc1, v.ysq[M_t=="FH"]] 
  
  # Depressed
  m.p_t["ND", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*(1-rr.ND.CD*p.NC[t,bc1]-p.DR[t])
  m.p_t["CD", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*rr.ND.CD*p.NC[t,bc1]
  m.p_t["NR", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*p.DR[t]
  m.p_t["X" , M_t == "ND"] <- rr.DX[t]*p.NX[t,bc1]
  
  m.p_t["CD", M_t == "CD"] <- (1-rr.DX[t]*p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-p.DR[t])
  m.p_t["FD", M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*rr.CD.FD*p.CF[t,bc1]
  m.p_t["CR", M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*p.DR[t]
  m.p_t["X" , M_t == "CD"] <-  rr.DX[t]*p.CX[t,bc1]	
  
  m.p_t["FD", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]])*(1-p.DR[t])
  m.p_t["FR", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]])*p.DR[t]
  m.p_t["X" , M_t == "FD"] <- rr.DX[t]*a_p.FX.ysq[t, bc1, v.ysq[M_t=="FD"]]
  
  # Recovered
  m.p_t["ND", M_t == "NR"] <- (1-p.NX[t,bc1])*p.RD[t]
  m.p_t["NR", M_t == "NR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.RD[t])
  m.p_t["CR", M_t == "NR"] <- (1-p.NX[t,bc1])*p.NC[t,bc1]
  m.p_t["X" , M_t == "NR"] <- p.NX[t,bc1]
  
  m.p_t["CD", M_t == "CR"] <- (1-p.CX[t,bc1])*rr.CR.CD*p.RD[t]
  m.p_t["CR", M_t == "CR"] <- (1-p.CX[t,bc1])*(1-rr.CR.CD*p.RD[t]-p.CF[t,bc1])
  m.p_t["FR", M_t == "CR"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]	
  m.p_t["X" , M_t == "CR"] <-   p.CX[t,bc1]  	
  
  m.p_t["FD", M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]])*p.RD[t]
  m.p_t["FR", M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]])*(1-p.RD[t])
  m.p_t["X" , M_t == "FR"] <- a_p.FX.ysq[t, bc1, v.ysq[M_t=="FR"]]	
  
  m.p_t["X" , M_t == "X"] <-  1		
  
 
  # print birth cohort and age for debugging problematic transition probabilities
  # print(paste0("bc: ", bc, ", age: ",t))
  check_transition_probability(m.p_t,verbose=FALSE)
  check_sum_of_transition_array(m.p_t, n_rows=n.i, n_cycles= n.t, verbose = FALSE)
  return(t(m.p_t)) 
}       

## COSTS FUNCTION ----------------------------------------------------
costs <- function(m.M) { # gets the costs for each person based on health state M and age t
  m.C <- matrix(nrow = n.i*length(cohorts), ncol = n.t + 1, 
                              dimnames = dimnames(m.M))  
  for (t in 1:n.t){
    m.C[m.M =="NH"] <- c.NH[t] 
    m.C[m.M =="CH"] <- c.CH[t]
    m.C[m.M =="FH"] <- c.FH[t]
    m.C[m.M =="ND"] <- c.ND[t]
    m.C[m.M =="CD"] <- c.CD[t]
    m.C[m.M =="FD"] <- c.FD[t]
    m.C[m.M =="NR"] <- c.NR[t]
    m.C[m.M =="CR"] <- c.CR[t]
    m.C[m.M =="FR"] <- c.FR[t]
    m.C[m.M =="X"] <- 0
  }
  return(m.C) 
}      

## UTILITIES FUNCTION ----------------------------------------------------
utils <- function(m.M) { # gets the utilities for each person based on health state M and age t
  m.U <- matrix(nrow = n.i*length(cohorts), ncol = n.t + 1, 
                dimnames = dimnames(m.M))  
  for (t in 1:n.t){
    m.U[m.M =="NH"] <- u.NH[t] 
    m.U[m.M =="CH"] <- u.CH[t]
    m.U[m.M =="FH"] <- u.FH[t]
    m.U[m.M =="ND"] <- u.ND[t]
    m.U[m.M =="CD"] <- u.CD[t]
    m.U[m.M =="FD"] <- u.FD[t]
    m.U[m.M =="NR"] <- u.NR[t]
    m.U[m.M =="CR"] <- u.CR[t]
    m.U[m.M =="FR"] <- u.FR[t]
    m.U[m.M =="X"] <- 0
  }
  return(m.U) 
}      

## PRODUCTIVITY FUNCTION ----------------------------------------------------
productivity <- function(m.M) { # gets the utilities for each person based on health state M and age t
  m.W <- matrix(nrow = n.i*length(cohorts), ncol = n.t + 1, 
                dimnames = dimnames(m.M))  
  for (t in 1:n.t){
    m.W[m.M =="NH"] <- w.NH[t] 
    m.W[m.M =="CH"] <- w.CH[t]
    m.W[m.M =="FH"] <- w.FH[t]
    m.W[m.M =="ND"] <- w.ND[t]
    m.W[m.M =="CD"] <- w.CD[t]
    m.W[m.M =="FD"] <- w.FD[t]
    m.W[m.M =="NR"] <- w.NR[t]
    m.W[m.M =="CR"] <- w.CR[t]
    m.W[m.M =="FR"] <- w.FR[t]
    m.W[m.M =="X"] <- 0
  }
  return(m.W) 
}    


## MODEL PREVALENCE RESULTS ------------------------------------------------
get_prevs <- function(state,m_cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m.m_prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m_cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na_rm=TRUE)
      dead <- sum(select=="X",na_rm=TRUE) 
      counts <- sum(str_count(select,state),na_rm=TRUE)
      prev <- sum(str_count(select,state),na_rm=TRUE)/sum(select!="X",na_rm=TRUE)
      m.m_prevs<-rbind(m.m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.m_prevs) 
}

get_subgroup_prevs <- function(state,denom, m_cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m.m_prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m_cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(str_count(select,denom),na_rm=TRUE)
      counts <- sum(str_count(select,state),na_rm=TRUE)
      dead <- sum(select=="X",na_rm=TRUE) 
      prev <- sum(str_count(select,state),na_rm=TRUE)/sum(str_count(select,denom),na_rm=TRUE)
      m.m_prevs<-rbind(m.m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.m_prevs) 
}

############################################################################################
# The MDS microsimulation model was developed by Jamie Tam and last updated on 11/3/2021
#
# The code was adapted from Appendix A of the article: 
#
# Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400-22.
# 
# See GitHub for more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################