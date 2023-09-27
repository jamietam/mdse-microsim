##################################### Functions ###########################################

# The microsim function keeps track of what happens to each individual during each cycle. 
# Arguments:  
# v_M_1:   vector of initial states for individuals 
# n_i:     number of individuals
# n_t:     total number of cycles to run the model
# v_n:     vector of health state names
# TR.out:  should the output include a microsimulation trace? (default is TRUE)
# TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
# Trt:     are the n_i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
# Makes use of:
# Probs:   function for the estimation of transition probabilities

mds_microsim <- function(bc,v_M_1, n_i, n_t, v_n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  set.seed(seed)                                      # set the seed for every individual for the random number generator
  
  v_ysq <- rep(n_i, 0) # vector counting how many years since quit
  
  # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m_M <- matrix(nrow = n_i, ncol = n_t + 1, 
                dimnames = list(paste(bc, 1:n_i, sep = "."), # each individual, year of birth
                                paste(0:n_t, sep = " ")))  
  m_M[, 1] <- v_M_1                                         # indicate the initial health state   
  
  for (t in 1:n_t) {
    if (bc+t>2100){ # exit for loop if going past the year 2100
      break
    }
    v_ysq <- ifelse((m_M[,t] =="FH"|m_M[,t] =="FD"|m_M[,t] =="FR" ) ,v_ysq +1 , 0)
    v_ysq <- ifelse(v_ysq>40 , 40, v_ysq)
    
    m_P <- probs(bc, t, v_ysq, m_M[, t])           # calculate the transition probabilities at cycle t 
    m_M[, t + 1] <- samplev(m_P, 1)      # sample the next health state and store that state in matrix m_M 
  }                                                       # close the loop for the time points 
   
  if (TS.out == TRUE) {  # create a  matrix of transitions across states
    TS <- paste(m_M, cbind(m_M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n_i)
    rownames(TS) <- paste(bc, 1:n_i, sep = ".")   # name the rows 
    colnames(TS) <- paste(0:n_t, sep = " ")   # name the columns (age)
  } else {
    TS <- NULL
  }
  
  if (TR.out == TRUE) { # create a trace from the individual trajectories
    TR <- t(apply(m_M, 2, function(x) table(factor(x, levels = v_n, ordered = TRUE))))
    TR <- TR / n_i                                       # create a distribution trace
    rownames(TR) <- paste(bc:(bc+n_t), sep = ".")        # name the rows by birth cohort
    colnames(TR) <- v_n                                  # name the columns 
  } else {
    TR <- NULL
  }
  
  results <- list(m_M = m_M, TS = TS, TR = TR) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


## PROBABILITY FUNCTION ----------------------------------------------------
probs <- function(bc, t, v_ysq, M_t) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_t: health state occupied by individuals at cycle t (character variable)
  # ysq: years since quitting if individual is former smoker
  
  bc1 = bc-1899

  # create matrix of state transition probabilities
  m_p.t <- matrix(data = 0, nrow = length(v_n), ncol = n_i)  
  # give the state names to the rows
  rownames(m_p.t) <-  v_n                               
  
  # update m_p.t with the probabilities conditional on survival  
  # interaction effects: rr.ND.CD, rr.CH.CD, rr.CR.CD, rr.CD.CR, rr.CD.FD)
  
  # Happy
  m_p.t["NH", M_t == "NH"] <- (1-p.NX[t])*(1-p.NC[t,bc1]-p.HD[t,bc1])
  m_p.t["CH", M_t == "NH"] <- (1-p.NX[t])*p.NC[t,bc1]
  m_p.t["ND", M_t == "NH"] <- (1-p.NX[t])*p.HD[t,bc1]
  m_p.t["X",  M_t == "NH"] <- p.NX[t]
  
  m_p.t["CH",M_t == "CH"] <- (1-p.CX[t,bc1])*(1-rr.CH.CD*p.HD[t,bc1]-p.CF[t,bc1])
  m_p.t["FH",M_t == "CH"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
  m_p.t["CD",M_t == "CH"] <- (1-p.CX[t,bc1])*rr.CH.CD*p.HD[t,bc1]
  m_p.t["X",  M_t == "CH"] <- p.CX[t,bc1]	
  
  m_p.t["FH", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v_ysq[M_t=="FH"]])*(1 -p.HD[t,bc1])
  m_p.t["FD", M_t == "FH"] <- (1-a_p.FX.ysq[t,bc1,v_ysq[M_t=="FH"]])*p.HD[t,bc1]
  m_p.t["X", M_t =="FH"] <-	a_p.FX.ysq[t, bc1, v_ysq[M_t=="FH"]] 
  
  # Depressed
  m_p.t["ND", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*(1-rr.ND.CD*p.NC[t,bc1]-p.DR[t])
  m_p.t["CD", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*rr.ND.CD*p.NC[t,bc1]
  m_p.t["NR", M_t == "ND"] <- (1-rr.DX[t]*p.NX[t,bc1])*p.DR[t]
  m_p.t["X", M_t == "ND"] <- rr.DX[t]*p.NX[t,bc1]
  
  m_p.t["CD",M_t == "CD"] <- (1-rr.DX[t]*p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-rr.CD.CR*p.DR[t])
  m_p.t["FD",M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*rr.CD.FD*p.CF[t,bc1]
  m_p.t["CR",M_t == "CD"] <-  (1-rr.DX[t]*p.CX[t,bc1])*rr.CD.CR*p.DR[t]
  m_p.t["X",M_t == "CD"] <-  rr.DX[t]*p.CX[t,bc1]	
  
  m_p.t["FD", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v_ysq[M_t=="FD"]])*(1-p.DR[t])
  m_p.t["FR", M_t == "FD"] <- (1-rr.DX[t]*a_p.FX.ysq[t, bc1, v_ysq[M_t=="FD"]])*p.DR[t]
  m_p.t["X", M_t == "FD"] <- rr.DX[t]*a_p.FX.ysq[t, bc1, v_ysq[M_t=="FD"]]
  
  # Recovered
  m_p.t["ND", M_t == "NR"] <- (1-p.NX[t,bc1])*p.RD[t]
  m_p.t["NR", M_t == "NR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.RD[t])
  m_p.t["CR", M_t == "NR"] <- (1-p.NX[t,bc1])*p.NC[t,bc1]
  m_p.t["X", M_t == "NR"] <- p.NX[t,bc1]
  
  m_p.t["CD",M_t == "CR"] <- (1-p.CX[t,bc1])*rr.CR.CD*p.RD[t]
  m_p.t["CR",M_t == "CR"] <- (1-p.CX[t,bc1])*(1-rr.CR.CD*p.RD[t]-p.CF[t,bc1])
  m_p.t["FR",M_t == "CR"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]	
  m_p.t["X",M_t == "CR"] <-   p.CX[t,bc1]  	
  
  m_p.t["FD",M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v_ysq[M_t=="FR"]])*p.RD[t]
  m_p.t["FR",M_t == "FR"] <- (1-a_p.FX.ysq[t, bc1, v_ysq[M_t=="FR"]])*(1-p.RD[t])
  m_p.t["X",M_t == "FR"] <- a_p.FX.ysq[t, bc1, v_ysq[M_t=="FR"]]	
  
  m_p.t["X", M_t == "X"] <-  1		
  
 
  # print birth cohort and age for debugging problematic transition probabilities
  # print(paste0("bc: ", bc, ", age: ",t))
  check_transition_probability(m_p.t,verbose=FALSE)
  check_sum_of_transition_array(m_p.t, n_rows=n_i, n_cycles= n_t, verbose = FALSE)
  return(t(m_p.t)) 
}       


## MODEL PREVALENCE RESULTS ------------------------------------------------
get_prevs <- function(state,m_cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m_m_prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m_cohortbyyear[(n_i*(cohortmin-1900)+1):(n_i*(cohortmax-1900)+n_i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na_rm=TRUE)
      dead <- sum(select=="X",na_rm=TRUE) 
      counts <- sum(str_count(select,state),na_rm=TRUE)
      prev <- sum(str_count(select,state),na_rm=TRUE)/sum(select!="X",na_rm=TRUE)
      m_m_prevs<-rbind(m_m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m_m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m_m_prevs) 
}

get_subgroup_prevs <- function(state,denom, m_cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m_m_prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m_cohortbyyear[(n_i*(cohortmin-1900)+1):(n_i*(cohortmax-1900)+n_i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(str_count(select,denom),na_rm=TRUE)
      counts <- sum(str_count(select,state),na_rm=TRUE)
      dead <- sum(select=="X",na_rm=TRUE) 
      prev <- sum(str_count(select,state),na_rm=TRUE)/sum(str_count(select,denom),na_rm=TRUE)
      m_m_prevs<-rbind(m_m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m_m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m_m_prevs) 
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