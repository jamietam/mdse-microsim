############################################################################################
## The microsimulation model code was adapted from the DARTH workgroup (www.darthworkgroup.com). 
# 	See Appendix A of the article: 
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
#   Microsimulation modeling for health decision sciences using R: A tutorial. 
#   Med Decis Making. 2018;38(3):400-22.
# For more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################
## MICROSIMULATION MODEL FUNCTION ----------------------------------------------------						  
dep_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
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
      v.p <- dep_probs(bc, t, m.M[i, t])           # calculate the transition probabilities at cycle t 
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
} # end of the dep_microsim function  

## PROBABILITY FUNCTION ----------------------------------------------------
dep_probs <- function(bc, t, M_it) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  bc1 = bc-1899
  
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # Never MD "H" (Happy)
  v.p.it[M_it == "H"] <- 
    c((1-p.HX[t,bc1]- p.HD[t,bc1]), #H to H
      p.HD[t,bc1],      #H to D
      0,                         #H to R
      p.HX[t,bc1])                       #H to X
  
  # Current MD "D" 
  v.p.it[M_it == "D"] <- 
    c(0,                                 #D to H
      (1-p.DX[t,bc1]- p.DR[t]),  #D to D
      p.DR[t],       #D to R
      p.DX[t,bc1])              #D to X
  
  # Former MD "R" 
  v.p.it[M_it == "R"] <- 
    c(0,                                 #R to H
      p.RD[t],   #R to D
      (1 - p.RX[t,bc1]-p.RD[t]), #R to R
      p.RX[t])                       #R to X
  
  v.p.it[M_it == "X"]  <- c(0,0,0,0, 1)		 #X to X = DEAD
  
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
      if(cohortmin<min(cohorts)) {next}
      cohortmax = year-agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-min(cohorts)+1)-(n.i-1)):(n.i*(cohortmax-min(cohorts))+n.i),paste(year)] # select rows for the birth cohorts who comprise that year's prevalence estimate
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
      m.M.prevs<-rbind(m.M.prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("agegroup","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}