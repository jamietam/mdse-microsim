############################################################################################
## The microsimulation model code was adapted from the DARTH workgroup (www.darthworkgroup.com). 
# 	See Appendix A of the article: 
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
#   Microsimulation modeling for health decision sciences using R: A tutorial. 
#   Med Decis Making. 2018;38(3):400-22.
# For more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################

rm(list = ls())  # remove any variables in R's memory
library(stringr)
library(doParallel) # set up model to run in parallel
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)

mainDir <- "C:/Users/JT936/Dropbox/GitHub/mds-microsim"
setwd(file.path(mainDir))

## Inputs
whichgender <- "females"
load(paste0("data/smk_inputs_",whichgender,".RData")) # Load all smoking and mortality inputs as matrices
smk_init = smk_init_cisnet 
smk_cess = smk_cess_cisnet 

n.i   <- 10                      # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 
cohorts <- 1900:2100


## MICROSIMULATION MODEL FUNCTION
smk_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
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
      v.p <- smk_probs(bc, t, m.M[i, t])           # calculate the transition probabilities at cycle t 
      m.M[i, t + 1] <- sample(v.n, size=1, prob = v.p)      # sample the next health state and store that state in matrix m.M 
    }                                                       # close the loop for the time points 
    if (i/100 == round(i/100,0)) {                          # display the progress of the simulation
      cat('\r', paste(i/n.i * 100, "% done", sep = " "))
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
} # end of the smk_microsim function  

## PROBABILITY FUNCTION 
smk_probs <- function(bc, t, M_it) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  
  # Transition probabilities (per cycle) by birth cohort
  p.NC <- diag(smk_init[,(bc-1899):201]) # probability to become Current smoker when Never smoker
  p.CF <- diag(smk_cess[,(bc-1899):201]) # probability to become Former smoker when Current smoker
  p.NX <- diag(death_ns[,(bc-1899):201]) # probability to die when Never smoker
  p.CX <- diag(death_cs[,(bc-1899):201]) # probability to die when Current smoker
  p.FX <- diag(death_fs[,(bc-1899):201]) # probability to die when Former smoker
  
  p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
  p.NC[100] <- p.CF[100] <- 0 
  
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # Never
  v.p.it[M_it == "N"] <- 
    c((1-p.NX[t])*(1 - p.NC[t]), 
      (1-p.NX[t])*p.NC[t], 
      0, 	
      p.NX[t]) 
  
  v.p.it[M_it == "C"] <- 
    c(0, 
      (1-p.CX[t])*(1- p.CF[t]),
      (1-p.CX[t])*p.CF[t],
      p.CX[t]) 
  
  v.p.it[M_it == "F"] <- 
    c(0,
      0,
      (1 - p.FX[t]),
      p.FX[t])
  
  v.p.it[M_it == "X"]  <- c(0,0,0, 1)					#X = DEAD
  # return the transition probabilities or produce an error
  ifelse(any(is.na(v.p.it)), print(paste0(paste0(v.p.it,collapse=", ")," - NA probability! bc: ", bc,", age: ",t,", M_it: ",M_it)),return(v.p.it)) 
  ifelse(any(v.p.it<0),print(paste0(paste0(v.p.it,collapse=", ")," - Negative probability! bc: ", bc, ", age: ",t,", M_it: ", M_it)),return(v.p.it))
  ifelse(round(sum(v.p.it),8) == 1, return(v.p.it), print(paste("Probabilities do not sum to 1:", sum(v.p.it), "bc:",bc,"age:",t,"M_it:",M_it))) # rounds off to the eigth digit because otherwise you get 0.000000001 instead of 0
  return(v.p.it) 
}       

## GET MODEL PREVALENCE RESULTS
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
  colnames(m.M.prevs)<-c("agegroup","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}



## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------

t_init <- Sys.time() # Start timer

# Simulate for each birth cohort with parallelization
m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                        .export=c('smk_microsim','smk_probs','get_prevs', 
                                  'smk_init','smk_cess','death_cs','death_ns','death_fs',
                                  'n.i','n.t','v.n','n.s','v.M_1')) %dopar%
  {
    smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
  }

# Convert matrix from persons-age to persons-year
m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = 301)
for (b in 1:length(cohorts)){
  m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
}
colnames(m.cohortbyyear) <- c(1900:2200)

# Output prevalence results as a list
model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=2020)
names(model_res) <- v.n
model_res$N <- model_res$N[order(model_res$N[,"agegroup"],decreasing=FALSE),]
model_res$C <- model_res$C[order(model_res$C[,"agegroup"],decreasing=FALSE),]
model_res$F <- model_res$F[order(model_res$F[,"agegroup"],decreasing=FALSE),]

Sys.time() - t_init # End timer


## PROBABILITY CHECKS ------------------------------------------------------
# cohorts = c(1900:2100)
# for (bc in cohorts){
# p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
# p.CF <- round(diag(as.matrix(smk_cess)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
# p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
# p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
# p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
# 
# p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
# p.NC[100] <- p.CF[100] <- 0
# 
#   for (t in c(1:n.t)){
#     if (bc+t>2100){ # exit for loop if going past the year 2100
#       break
#     }
# 
#     N = c((1-p.NX[t])*(1 - p.NC[t]),
#           (1-p.NX[t])*p.NC[t],
#            0,
#            p.NX[t])
#     C = c( 0,
#           (1-p.CX[t])*(1- p.CF[t]),
#           (1-p.CX[t])*p.CF[t],
#            p.CX[t])
#     F = c( 0,
#            0,
#           (1 - p.FX[t]),
#            p.FX[t])
# 
# 
#     allprobs = rbind(N, C, F)
#     # Check for any negative, missing probabilities, or probability sets that do not sum to 1
#     if(any(is.na(allprobs))){
#       print(paste("NA probability! bc: ", bc, ", age: ",t))
#       print(allprobs)
#     }
#     if(any(allprobs<0)){
#       print(paste("Negative probability! bc: ", bc, ", age: ",t))
#       print(allprobs)
#     }
#     if(any(round(rowSums(allprobs),8) != 1)){
#       print(paste("Probabilities do not sum to 1! ", "bc:",bc,"age:",t))
#       print (rowSums(allprobs))
#     }
#   }
# }