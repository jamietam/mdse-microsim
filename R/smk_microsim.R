############################################################################################
## The microsimulation model code was adapted from the DARTH workgroup (www.darthworkgroup.com). 
# 	See Appendix A of the article: 
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
#   Microsimulation modeling for health decision sciences using R: A tutorial. 
#   Med Decis Making. 2018;38(3):400-22.
# For more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################
## MICROSIMULATION MODEL FUNCTION ----------------------------------------------------						  
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
  
## PROBABILITY FUNCTION ----------------------------------------------------
smk_probs <- function(bc, t, M_it) { # updates the transition probabilities of every cycle
    # bc:   birth cohort
    # t:    time in model / age
    # M_it: health state occupied by individual i at cycle t (character variable)
    
    # Transition probabilities (per cycle) by birth cohort
    p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
    p.CF <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
    p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
    p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
    p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
    
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
  
#   for (t in c(1:n.t)){
#     if (bc+t>2100){ # exit for loop if going past the year 2100
#       break
#     }
      
    # N = c((1-p.NX[t])*(1 - p.NC[t]), 
          # (1-p.NX[t])*p.NC[t], 
           # 0, 	
           # p.NX[t]) 
    # C = c( 0,
          # (1-p.CX[t])*(1- p.CF[t]), 
          # (1-p.CX[t])*p.CF[t], 
           # p.CX[t]) 
    # F = c( 0,
           # 0, 
          # (1 - p.FX[t]), 
           # p.FX[t])
    
    
    # allprobs = rbind(N, C, F)
    # # Check for any negative, missing probabilities, or probability sets that do not sum to 1
    # if(any(is.na(allprobs))){
      # print(paste("NA probability! bc: ", bc, ", age: ",t))
      # print(allprobs)
    # }
    # if(any(allprobs<0)){
      # print(paste("Negative probability! bc: ", bc, ", age: ",t))
      # print(allprobs)
      # }
    # if(any(round(rowSums(allprobs),8) != 1)){
      # print(paste("Probabilities do not sum to 1! ", "bc:",bc,"age:",t))
      # print (rowSums(allprobs))
      # }
    # }
# }