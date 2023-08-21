##################################### Functions ###########################################

# The microsim function keeps track of what happens to each individual during each cycle. 
# Arguments:  
# v.M_1:   vector of initial states for individuals 
# n.i:     number of individuals
# n.t:     total number of cycles to run the model
# v.n:     vector of health state names
# TR.out:  should the output include a microsimulation trace? (default is TRUE)
# TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
# Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
# Makes use of:
# Probs:   function for the estimation of transition probabilities

mds_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
  # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- matrix(nrow = n.i, ncol = n.t + 1, 
                dimnames = list(paste(bc, 1:n.i, sep = "."), # each individual, year of birth
                                paste(0:n.t, sep = " ")))  
  m.M[, 1] <- v.M_1                                         # indicate the initial health state   
  
  for (i in 1:n.i) {
    set.seed(seed + i)                                      # set the seed for every individual for the random number generator
    for (t in 1:n.t) {
      if ((bc+t>2100)|(m.M[i, t]=="X")){ # exit for loop if going past the year 2100
        break
      }
      if (m.M[i,t]=="FH"|m.M[i,t]=="FD"|m.M[i,t]=="FR"){ # if former smoker, 
        ysq <- sum((m.M[i,]=="FH"|m.M[i,]=="FD"|m.M[i,]=="FR"), na.rm=TRUE) +1 # count the number of years since quitting for this individual
      } else{
        ysq = 1 # set years since quitting as 1 unless individual is a former smoker  
      }
      v.p <- probs(bc, t, ysq, m.M[i, t])           # calculate the transition probabilities at cycle t 
      m.M[i, t + 1] <- sample(v.n, size=1, prob = v.p)      # sample the next health state and store that state in matrix m.M 
    }                                                       # close the loop for the time points 
    if (i/100 == round(i/100,0)) {                          # display the progress of the simulation
      cat('\r', paste(i/n.i * 100, "% done, birth cohort:",bc, sep = " "))
    }
  } # close the loop for the individuals 
  
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
  
  results <- list(m.M = m.M, TS = TS, TR = TR) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


## PROBABILITY FUNCTION ----------------------------------------------------

probs <- function(bc, t, ysq, M_it) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  # ysq: years since quitting if individual is former smoker
  
  bc1 = bc-1899
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  ysq[ysq>40]<-40 # if quit more than 40 years ago, set ysq at 40 years since quitting
  
  # update v.p.it with the probabilities   
  # interaction effects: rr.ND.CD, rr.CH.CD, rr.CR.CD, rr.CD.CR, rr.CD.FD)
  
  # Happy
  v.p.it[M_it == "NH"] <- 
    c((1-p.NC[t,bc1]-p.HD[t,bc1]-p.NX[t,bc1]), p.NC[t,bc1], 0, # NH , CH, FH
      p.HD[t],0,0, 	# ND, CD, FD
      0,0,0,				# NR, CR, FR
      p.NX[t]) 			# X
  
  v.p.it[M_it == "CH"] <- 
    c(0,(1-p.CF[t,bc1]-rr.CH.CD*p.HD[t,bc1]-p.CX[t,bc1]), p.CF[t,bc1], 
      0,rr.CH.CD*p.HD[t,bc1],0, 
      0,0,0, 				
      p.CX[t,bc1]) 			
  
  v.p.it[M_it == "FH"] <- 
    c(0,0, (1 -p.HD[t,bc1] - p.FX.ysq[[ysq]][t,bc1]), 
      0,0, p.HD[t,bc1],	
      0,0,0,				
      p.FX.ysq[[ysq]][t,bc1])			
  
  # Depressed
  v.p.it[M_it == "ND"] <- 
    c(0,0,0, 	
      (1-rr.ND.CD*p.NC[t,bc1]-p.DR[t]-rr.DX[t]*p.NX[t,bc1]), rr.ND.CD*p.NC[t,bc1], 0, 
      p.DR[t],0,0, 		
      rr.DX[t]*p.NX[t,bc1]) 		
  
  v.p.it[M_it == "CD"] <- 
    c(0,0,0,				
      0,(1-rr.CD.FD*p.CF[t,bc1]-rr.CD.CR*p.DR[t]-rr.DX[t]*p.CX[t,bc1]), rr.CD.FD*p.CF[t,bc1], 
      0,rr.CD.CR*p.DR[t],0,			
      rr.DX[t]*p.CX[t,bc1])  	
  
  v.p.it[M_it == "FD"] <- 
    c(0,0,0,				
      0,0,(1-p.DR[t]-rr.DX[t]*p.FX.ysq[[ysq]][t,bc1]),
      0,0,p.DR[t],			
      rr.DX[t]*p.FX.ysq[[ysq]][t,bc1])		
  # Recovered
  v.p.it[M_it == "NR"] <- 
    c(0,0,0,				
      p.RD[t],0,0, 			
      (1-p.NC[t,bc1]-p.RD[t]-p.NX[t,bc1]), p.NC[t,bc1],0,
      p.NX[t,bc1]) 		
  
  v.p.it[M_it == "CR"] <- 
    c(0,0,0,
      0,rr.CR.CD*p.RD[t],0,			
      0,(1-rr.CR.CD*p.RD[t]-p.CF[t,bc1]-p.CX[t,bc1]), p.CF[t,bc1],	
      p.CX[t,bc1])  	
  
  v.p.it[M_it == "FR"] <- 
    c(0,0,0,				
      0,0,p.RD[t],
      0,0,(1-p.RD[t]-p.FX.ysq[[ysq]][t,bc1]),
      p.FX.ysq[[ysq]][t,bc1])		
  
  v.p.it[M_it == "X"] <- 
    c(0,0,0,				
      0,0,0,
      0,0,0,
      1)		
  
  v.p.it[v.p.it>1]<-1 # if any probabilities are greater than 1, replace with 1
  v.p.it[v.p.it<0]<-0 # if any probabilities are negative, replace with zero
  
  # return the transition probabilities or produce an error
  ifelse(any(is.na(v.p.it)), print(paste0(paste0(v.p.it,collapse=", ")," - NA probability! bc: ", bc,", age: ",t,", M_it: ",M_it)),return(v.p.it)) 
  ifelse(any(v.p.it<0),print(paste0(paste0(v.p.it,collapse=", ")," - Negative probability! bc: ", bc, ", age: ",t,", M_it: ", M_it)),return(v.p.it))
  ifelse(round(sum(v.p.it),8) == 1, return(v.p.it), print(paste("Probabilities do not sum to 1:", sum(v.p.it), "bc:",bc,"age:",t,"M_it:",M_it))) # rounds off to the eigth digit because otherwise you get 0.000000001 instead of 0
  return(v.p.it) 
}       


## MODEL PREVALENCE RESULTS ------------------------------------------------
get_prevs <- function(state,m.cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m.M.prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
      m.M.prevs<-rbind(m.M.prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}

get_subgroup_prevs <- function(state,denom, m.cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m.M.prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(str_count(select,denom),na.rm=TRUE)
      counts <- sum(str_count(select,state),na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(str_count(select,denom),na.rm=TRUE)
      m.M.prevs<-rbind(m.M.prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
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

## PROBABILITY CHECKS ------------------------------------------------------
# for (M_it in v.n){
#   for (bc in 1900:2100){
#     if (bc+t>2100){ # exit for loop if going past the year 2100
#             break
#           }
#     for (t in 1:100){
#       for (ysq in 1:40){
#         probs(bc, t, ysq, M_it)  
#       }
#     }
#   }
# }
 
  # p.NC = smk_init*c(rep(v_params[1],13),rep(v_params[2],3),rep(v_params[3],3),rep(v_params[4],3),
#                   rep(v_params[5],28),rep(1,50))
# p.CF = smk_cess#*c(rep(1,35),rep(v_params[4],15),rep(v_params[5],15),rep(v_params[6],35))
# 
# for (bc in cohorts){
#   bc1 = bc-1899
#   for (t in c(1:n.t)){
#     if (bc1+t>2100){ # exit for loop if going past the year 2100
#       break
#     }
# 
#     N = c((1-p.NX[t,bc1] - p.NC[t,bc1]), #N to N
#           p.NC[t,bc1],       #N to C
#           0, 	                               #N to F
#           p.NX[t,bc1])
#     C = c(0,                                 #C to N
#           (1-p.CX[t,bc1]- p.CF[t,bc1]),  #C to C
#           p.CF[t,bc1],       #C to F
#           p.CX[t,bc1])
#     for (ysq in c(1:40)){
#       F = c(0,                                 #F to N
#             0,                                 #F to C
#             (1 - p.FX.ysq[[ysq]][t,bc1]),                 #F to F - former smoker mortality based on years since quit (ysq)
#             p.FX.ysq[[ysq]][t,bc1]) 
#       
#       allprobs = rbind(N, C, F)
#       # Check for any negative, missing probabilities, or probability sets that do not sum to 1
#       if(any(is.na(allprobs))){
#         print(paste("NA probability! bc: ", bc, ", age: ",t, ", ysq: ",ysq))
#         print(allprobs)
#       }
#       if(any(allprobs<0)){
#         print(paste("Negative probability! bc: ", bc, ", age: ",t, ", ysq: ",ysq))
#         print(allprobs)
#       }
#       if(any(round(rowSums(allprobs),8) != 1)){
#         print(paste("Probabilities do not sum to 1! ", "bc:",bc,"age:",t, ", ysq: ",ysq))
#         print (rowSums(allprobs))
#       }
#     }
#     
#   }
# }