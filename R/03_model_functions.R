##################################### Functions ###########################################
## MICROSIM FUNCTION ----------------------------------------------------
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
## Makes use of:
# Probs:   function for the estimation of transition probabilities
# Costs:   function for the estimation of cost state values
# Effs:    function for the estimation of state specific health outcomes (QALYs)

mds_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE) {

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
    v.ysq <- ifelse(grepl("F", m.M[, t]), v.ysq + 1, 0) # if health state contains the letter 'F', add 1 year since quitting
    v.ysq <- ifelse(v.ysq>40 , 40, v.ysq) # Fix mortality after 40 years since quitting
    
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
  yr = bc + t - 1899
  
  # create matrix of state transition probabilities
  m.p_t <- matrix(data = 0, nrow = length(v.n), ncol = n.i)  
  # give the state names to the rows
  rownames(m.p_t) <-  v.n                               
  
  ##transition probabilities calculations
  #from NHO state
  m.p_t["NOH", M_t == "NOH"] <- (1-p.NX[t,yr])*(1-p.NC[t,yr]-p.NO.NE[t,yr]-p.HD[t,yr])
  m.p_t["COH", M_t == "NOH"] <- (1-p.NX[t,yr])*(p.NC[t,yr])
  m.p_t["NEH", M_t == "NOH"] <- (1-p.NX[t,yr])*(p.NO.NE[t,yr])
  m.p_t["NOD", M_t == "NOH"] <- (1-p.NX[t,yr])*(p.HD[t,yr])
  m.p_t["X", M_t == "NOH"] <- p.NX[t,yr]
  
  #from CHO state
  m.p_t["COH", M_t == "COH"] <- (1-p.CX[t,yr])*(1-p.CF[t,yr]-p.CO.CE[t,yr]-p.HD[t,yr]-p.CO.FE[t,yr])
  m.p_t["FOH", M_t == "COH"] <- (1-p.CX[t,yr])*(p.CF[t,yr])
  m.p_t["FEH", M_t == "COH"] <- (1-p.CX[t,yr])*(p.CO.FE[t,yr])
  m.p_t["CEH", M_t == "COH"] <- (1-p.CX[t,yr])*(p.CO.CE[t,yr])
  m.p_t["COD", M_t == "COH"] <- (1-p.CX[t,yr])*(p.HD[t,yr])
  m.p_t["X", M_t == "COH"] <- p.CX[t,yr]
  
  ##from FHO state
  m.p_t["FOH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOH"]])*(1-p.FO.FE[t,yr]-p.HD[t,yr])
  m.p_t["FEH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOH"]])*(p.FO.FE[t,yr])
  m.p_t["FOD", M_t == "FOH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOH"]])*(p.HD[t,yr])
  m.p_t["X", M_t == "FOH"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FOH"]]
  
  ##from NHE state
  m.p_t["NEH", M_t == "NEH"] <- (1-p.EX[t,yr])*(1-p.NC[t,yr]-p.NE.NQ[t,yr]-p.HD[t,yr])
  m.p_t["CEH", M_t == "NEH"] <- (1-p.EX[t,yr])*(p.NC[t,yr])
  m.p_t["NQH", M_t == "NEH"] <- (1-p.EX[t,yr])*(p.NE.NQ[t,yr])
  m.p_t["NED", M_t == "NEH"] <- (1-p.EX[t,yr])*(p.HD[t,yr])
  m.p_t["X" , M_t == "NEH"] <- p.EX[t,yr]
  
  ##from CHE state
  m.p_t["CEH", M_t == "CEH"] <- (1-p.CX[t,yr])*(1-p.CE.CQ[t,yr]-p.CF[t,yr]-rr.CH.CD*p.HD[t,yr])
  m.p_t["FEH", M_t == "CEH"] <- (1-p.CX[t,yr])*(p.CF[t,yr])
  m.p_t["CQH", M_t == "CEH"] <- (1-p.CX[t,yr])*(p.CE.CQ[t,yr])
  m.p_t["CED", M_t == "CEH"] <- (1-p.CX[t,yr])*(rr.CH.CD*p.HD[t,yr])
  m.p_t["X", M_t == "CEH"] <- p.CX[t,yr]
  
  ##from FHE state
  m.p_t["FEH", M_t == "FEH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FEH"]])*(1-p.FE.FQ[t,yr]-p.HD[t,yr])
  m.p_t["FQH", M_t == "FEH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FEH"]])*(p.FE.FQ[t,yr])
  m.p_t["FED", M_t == "FEH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FEH"]])*(p.HD[t,yr])
  m.p_t["X", M_t == "FEH"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FEH"]]
  
  ##from NHQ state
  m.p_t["NQH", M_t == "NQH"] <- (1-p.NX[t,yr])*(1-p.NC[t,yr]-p.NQ.NE[t,yr]-p.HD[t,yr])
  m.p_t["CQH", M_t == "NQH"] <- (1-p.NX[t,yr])*(p.NC[t,yr])
  m.p_t["NEH", M_t == "NQH"] <- (1-p.NX[t,yr])*(p.NQ.NE[t,yr])
  m.p_t["NQD", M_t == "NQH"] <- (1-p.NX[t,yr])*(p.HD[t,yr])
  m.p_t["X", M_t == "NQH"] <- p.NX[t,yr]
  
  ##from CHQ state
  m.p_t["CQH", M_t == "CQH"] <- (1-p.CX[t,yr])*(1-p.CF[t,yr]-p.HD[t,yr])
  m.p_t["FQH", M_t == "CQH"] <- (1-p.CX[t,yr])*p.CF[t,yr]
  m.p_t["CQD", M_t == "CQH"] <- (1-p.CX[t,yr])*p.HD[t,yr]
  m.p_t["X", M_t == "CQH"] <- p.CX[t,yr]
  
  ##from FHQ state
  m.p_t["FQH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQH"]])*(1-p.FQ.FE[t,yr]-p.HD[t,yr])
  m.p_t["FEH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQH"]])*(p.FQ.FE[t,yr])
  m.p_t["FQD", M_t == "FQH"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQH"]])*(p.HD[t,yr])
  m.p_t["X", M_t == "FQH"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FQH"]]
  
  ##from NDO state
  m.p_t["NOD", M_t == "NOD"] <- (1-p.NX[t,yr])*(1-p.NC_D[t,yr]-rr.OD.ED*p.NO.NE[t,yr]-p.DR[t])
  m.p_t["COD", M_t == "NOD"] <- (1-p.NX[t,yr])*(p.NC_D[t,yr])
  m.p_t["NED", M_t == "NOD"] <- (1-p.NX[t,yr])*(rr.OD.ED*p.NO.NE[t,yr])
  m.p_t["NOR", M_t == "NOD"] <- (1-p.NX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "NOD"] <- p.NX[t,yr]
  
  ##from CDO state
  m.p_t["COD", M_t == "COD"] <- (1-p.CX[t,yr])*(1-rr.CD.FD*p.CF[t,yr]-rr.OD.ED*p.CO.CE[t,yr]-p.DR[t]-p.CO.FE[t,yr])
  m.p_t["FOD", M_t == "COD"] <- (1-p.CX[t,yr])*(rr.CD.FD*p.CF[t,yr])
  m.p_t["FED", M_t == "COD"] <- (1-p.CX[t,yr])*(p.CO.FE[t,yr])
  m.p_t["CED", M_t == "COD"] <- (1-p.CX[t,yr])*(rr.OD.ED*p.CO.CE[t,yr])
  m.p_t["COR", M_t == "COD"] <- (1-p.CX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "COD"] <- p.CX[t,yr]

  ##from FDO state
  m.p_t["FOD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOD"]])*(1-rr.OD.ED*p.FO.FE[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "FOD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOD"]])*(rr.OD.ED*p.FO.FE[t,yr])
  m.p_t["FOR", M_t == "FOD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOD"]])*(p.DR[t])
  m.p_t["X", M_t == "FOD"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FOD"]]
  
  ##from NDE state
  m.p_t["NED", M_t == "NED"] <- (1-p.EX[t,yr])*(1-p.NC_D[t,yr]-p.NE.NQ[t,yr]-p.DR[t])
  m.p_t["CED", M_t == "NED"] <- (1-p.EX[t,yr])*(p.NC_D[t,yr])
  m.p_t["NQD", M_t == "NED"] <- (1-p.EX[t,yr])*(p.NE.NQ[t,yr])
  m.p_t["NER", M_t == "NED"] <- (1-p.EX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "NED"] <- p.EX[t,yr]
  
  ##from CDE state
  m.p_t["CED", M_t == "CED"] <- (1-p.CX[t,yr])*(1-rr.CD.FD*p.CF[t,yr]-p.CE.CQ[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "CED"] <- (1-p.CX[t,yr])*(rr.CD.FD*p.CF[t,yr])
  m.p_t["CQD", M_t == "CED"] <- (1-p.CX[t,yr])*(p.CE.CQ[t,yr])
  m.p_t["CER", M_t == "CED"] <- (1-p.CX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "CED"] <- p.CX[t,yr]
  
  ##from FDE state
  m.p_t["FED", M_t =="FED"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FED"]])*(1-p.FE.FQ[t,yr]-p.DR[t])
  m.p_t["FQD", M_t =="FED"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FED"]])*(p.FE.FQ[t,yr])
  m.p_t["FER", M_t =="FED"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FED"]])*(p.DR[t])
  m.p_t["X", M_t == "FED"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FED"]]
  
  ##from NDQ state
  m.p_t["NQD", M_t == "NQD"] <- (1-p.NX[t,yr])*(1-p.NQ.NE[t,yr]-p.NC_D[t,yr]-p.DR[t])
  m.p_t["NED", M_t == "NQD"] <- (1-p.NX[t,yr])*(p.NQ.NE[t,yr])
  m.p_t["CQD", M_t == "NQD"] <- (1-p.NX[t,yr])*(p.NC_D[t,yr])
  m.p_t["NQR", M_t == "NQD"] <- (1-p.NX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "NQD"] <- p.NX[t,yr]
  
  ##from CDQ state
  m.p_t["CQD", M_t == "CQD"] <- (1-p.CX[t,yr])*(1-rr.CD.FD*p.CF[t,yr]-p.CQ.CE[t,yr]-p.DR[t])
  m.p_t["FQD", M_t == "CQD"] <- (1-p.CX[t,yr])*rr.CD.FD*p.CF[t,yr]
  m.p_t["CED", M_t == "CQD"] <- (1-p.CX[t,yr])*(p.CQ.CE[t,yr])
  m.p_t["CQR", M_t == "CQD"] <- (1-p.CX[t,yr])*(p.DR[t])
  m.p_t["X", M_t == "CQD"] <- p.CX[t,yr]
  
  ##from FDQ state
  m.p_t["FQD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQD"]])*(1-p.FQ.FE[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "FQD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQD"]])*(p.FQ.FE[t,yr])
  m.p_t["FQR", M_t == "FQD"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQD"]])*(p.DR[t])
  m.p_t["X", M_t == "FQD"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FQD"]]
  
  ##from NRO state
  m.p_t["NOR", M_t == "NOR"] <- (1-p.NX[t,yr])*(1-p.NC[t,yr]-p.NO.NE[t,yr]-p.RD[t])
  m.p_t["COR", M_t == "NOR"] <- (1-p.NX[t,yr])*(p.NC[t,yr])
  m.p_t["NER", M_t == "NOR"] <- (1-p.NX[t,yr])*(p.NO.NE[t,yr])
  m.p_t["NOD", M_t == "NOR"] <- (1-p.NX[t,yr])*(p.RD[t])
  m.p_t["X", M_t == "NOR"] <- p.NX[t,yr]
  
  ##from CRO state
  m.p_t["COR", M_t == "COR"] <- (1-p.CX[t,yr])*(1-p.CF[t,yr]-p.CO.CE[t,yr]-rr.CR.CD*p.RD[t]-p.CO.FE[t,yr])
  m.p_t["FOR", M_t == "COR"] <- (1-p.CX[t,yr])*(p.CF[t,yr])
  m.p_t["FER", M_t == "COR"] <- (1-p.CX[t,yr])*(p.CO.FE[t,yr])
  m.p_t["CER", M_t == "COR"] <- (1-p.CX[t,yr])*(p.CO.CE[t,yr])
  m.p_t["COD", M_t == "COR"] <- (1-p.CX[t,yr])*(rr.CR.CD*p.RD[t])
  m.p_t["X", M_t == "COR"] <- p.CX[t,yr]
  
  ##from FRO state
  m.p_t["FOR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOR"]])*(1-p.FO.FE[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "FOR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOR"]])*(p.FO.FE[t,yr])
  m.p_t["FOD", M_t == "FOR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FOR"]])*(p.RD[t])
  m.p_t["X", M_t == "FOR"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FOR"]]
  
  ##from NRE state
  m.p_t["NER", M_t == "NER"] <- (1-p.EX[t,yr])*(1-p.NC[t,yr]-p.NE.NQ[t,yr]-p.RD[t])
  m.p_t["CER", M_t == "NER"] <- (1-p.EX[t,yr])*(p.NC[t,yr])
  m.p_t["NQR", M_t == "NER"] <- (1-p.EX[t,yr])*(p.NE.NQ[t,yr])
  m.p_t["NED", M_t == "NER"] <- (1-p.EX[t,yr])*(p.RD[t])
  m.p_t["X", M_t == "NER"] <- p.EX[t,yr]
  
  ##from CRE state
  m.p_t["CER", M_t == "CER"] <- (1-p.CX[t,yr])*(1-p.CF[t,yr]-p.CE.CQ[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "CER"] <- (1-p.CX[t,yr])*(p.CF[t,yr])
  m.p_t["CQR", M_t == "CER"] <- (1-p.CX[t,yr])*(p.CE.CQ[t,yr])
  m.p_t["CED", M_t == "CER"] <- (1-p.CX[t,yr])*(p.RD[t])
  m.p_t["X", M_t == "CER"] <- p.CX[t,yr]
  
  ##from FRE state
  m.p_t["FER", M_t == "FER"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FER"]])*(1-p.FE.FQ[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "FER"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FER"]])*(p.FE.FQ[t,yr])
  m.p_t["FED", M_t == "FER"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FER"]])*(p.RD[t])
  m.p_t["X", M_t == "FER"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FER"]]
  
  ##from NRQ state
  m.p_t["NQR", M_t == "NQR"] <- (1-p.NX[t,yr])*(1-p.NC[t,yr]-p.NQ.NE[t,yr]-p.RD[t])
  m.p_t["CQR", M_t == "NQR"] <- (1-p.NX[t,yr])*(p.NC[t,yr])
  m.p_t["NER", M_t == "NQR"] <- (1-p.NX[t,yr])*(p.NQ.NE[t,yr])
  m.p_t["NQD", M_t == "NQR"] <- (1-p.NX[t,yr])*(p.RD[t])
  m.p_t["X", M_t == "NQR"] <- p.NX[t,yr]
  
  ##from CRQ state
  m.p_t["CQR", M_t == "CQR"] <- (1-p.CX[t,yr])*(1-p.CF[t,yr]-p.CQ.CE[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "CQR"] <- (1-p.CX[t,yr])*p.CF[t,yr]
  m.p_t["CER", M_t == "CQR"] <- (1-p.CX[t,yr])*p.CQ.CE[t,yr]
  m.p_t["CQD", M_t == "CQR"] <- (1-p.CX[t,yr])*p.RD[t]
  m.p_t["X", M_t == "CQR"] <- p.CX[t,yr]
  
  ##from FRQ state
  m.p_t["FQR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQR"]])*(1-p.FQ.FE[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "FQR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQR"]])*(p.FQ.FE[t,yr])
  m.p_t["FQD", M_t == "FQR"] <- (1-a_p.FX.ysq[t,yr,v.ysq[M_t == "FQR"]])*(p.RD[t])
  m.p_t["X", M_t == "FQR"] <- a_p.FX.ysq[t,yr,v.ysq[M_t == "FQR"]]
  
  m.p_t["X" , M_t == "X"] <-  1		
  
  
  # print birth cohort and age for debugging problematic transition probabilities
  check_transition_probability(m.p_t,verbose=FALSE)
  # if(any(colSums(m.p_t))!=1){
  #   print(m.p_t)
  #   #print(paste0("bc: ", bc, ", age: ",t, " year: ", yr, " M_t: ", M_t))
  # }
  # print(m.p_t)
  # print(paste0("bc: ", bc, ", age: ",t, " year: ", yr, " M_t: ", M_t))
  # print(colSums(m.p_t))
  check_sum_of_transition_array(t(m.p_t), n_rows=n.i, n_cycles= n.t, verbose = TRUE)
  
  return(t(m.p_t)) 
}       

## COSTS FUNCTION ----------------------------------------------------

costs <- function(M_t,t) { # gets the health care costs for each person based on health state M and age t
  
  m.c_t <- matrix(data = 0, nrow = n.i, ncol = 1)  
  
  m.c_t[M_t =="NOH"] <- m.c_t[M_t =="NEH"] <- m.c_t[M_t =="NQH"] <- c.NH[t] 
  m.c_t[M_t =="COH"] <- m.c_t[M_t =="CEH"] <- m.c_t[M_t =="CQH"] <- c.CH[t]
  m.c_t[M_t =="FOH"] <- m.c_t[M_t =="FEH"] <- m.c_t[M_t =="FQH"] <- c.FH[t]
  m.c_t[M_t =="NOD"] <- m.c_t[M_t =="NED"] <- m.c_t[M_t =="NQD"] <- c.ND[t]
  m.c_t[M_t =="COD"] <- m.c_t[M_t =="CED"] <- m.c_t[M_t =="CQD"] <- c.CD[t]
  m.c_t[M_t =="FOD"] <- m.c_t[M_t =="FED"] <- m.c_t[M_t =="FQD"] <- c.FD[t]
  m.c_t[M_t =="NOR"] <- m.c_t[M_t =="NER"] <- m.c_t[M_t =="NQR"] <- c.NR[t]
  m.c_t[M_t =="COR"] <- m.c_t[M_t =="CER"] <- m.c_t[M_t =="CQR"] <- c.CR[t]
  m.c_t[M_t =="FOR"] <- m.c_t[M_t =="FER"] <- m.c_t[M_t =="FQR"] <- c.FR[t]
  m.c_t[M_t =="X"] <- 0
  
  return(m.c_t) 
}      

## CONSUMER EXPENDITURES ----------------------------------------------------
cons_exp <- function(M_t,t) { # gets consumer expenditures for each person based on health state M and age t
  
  m.c_t <- matrix(data = 0, nrow = n.i, ncol = 1)  
  
  m.c_t[M_t =="NOH"] <- m.c_t[M_t =="NEH"] <- m.c_t[M_t =="NQH"] <- c.nonhealth[t] # non-healthcare expenditures
  m.c_t[M_t =="COH"] <- m.c_t[M_t =="CEH"] <- m.c_t[M_t =="CQH"] <- c.nonhealth[t]
  m.c_t[M_t =="FOH"] <- m.c_t[M_t =="FEH"] <- m.c_t[M_t =="FQH"] <- c.nonhealth[t]
  m.c_t[M_t =="NOD"] <- m.c_t[M_t =="NED"] <- m.c_t[M_t =="NQD"] <- c.nonhealth[t]
  m.c_t[M_t =="COD"] <- m.c_t[M_t =="CED"] <- m.c_t[M_t =="CQD"] <- c.nonhealth[t]
  m.c_t[M_t =="FOD"] <- m.c_t[M_t =="FED"] <- m.c_t[M_t =="FQD"] <- c.nonhealth[t]
  m.c_t[M_t =="NOR"] <- m.c_t[M_t =="NER"] <- m.c_t[M_t =="NQR"] <- c.nonhealth[t]
  m.c_t[M_t =="COR"] <- m.c_t[M_t =="CER"] <- m.c_t[M_t =="CQR"] <- c.nonhealth[t]
  m.c_t[M_t =="FOR"] <- m.c_t[M_t =="FER"] <- m.c_t[M_t =="FQR"] <- c.nonhealth[t]
  m.c_t[M_t =="X"] <- 0
  
  return(m.c_t) 
}      
## UTILITIES FUNCTION ----------------------------------------------------
utils <- function(M_t,t) { # gets the utilities for each person based on health state M and age t
  
  m.u_t <- matrix(data= NA, nrow = n.i, ncol = 1)  
  
  m.u_t[M_t =="NOH"] <- u.NOH[t]
  m.u_t[M_t =="NEH"] <- u.NEH[t]
  m.u_t[M_t =="NQH"] <- u.NQH[t] 
  m.u_t[M_t =="COH"] <- u.COH[t]
  m.u_t[M_t =="CEH"] <- u.CEH[t]
  m.u_t[M_t =="CQH"] <- u.CQH[t]
  m.u_t[M_t =="FOH"] <- u.FOH[t]
  m.u_t[M_t =="FEH"] <- u.FEH[t]
  m.u_t[M_t =="FQH"] <- u.FQH[t]
  m.u_t[M_t =="NOD"] <- u.NOD[t]
  m.u_t[M_t =="NED"] <- u.NED[t]
  m.u_t[M_t =="NQD"] <- u.NQD[t]
  m.u_t[M_t =="COD"] <- u.COD[t]
  m.u_t[M_t =="CED"] <- u.CED[t]
  m.u_t[M_t =="CQD"] <- u.CQD[t]
  m.u_t[M_t =="FOD"] <- u.FOD[t]
  m.u_t[M_t =="FED"] <- u.FED[t]
  m.u_t[M_t =="FQD"] <- u.FQD[t]
  m.u_t[M_t =="NOR"] <- u.NOR[t]
  m.u_t[M_t =="NER"] <- u.NER[t]
  m.u_t[M_t =="NQR"] <- u.NQR[t]
  m.u_t[M_t =="COR"] <- u.COR[t]
  m.u_t[M_t =="CER"] <- u.CER[t]
  m.u_t[M_t =="CQR"] <- u.CQR[t]
  m.u_t[M_t =="FOR"] <- u.FOR[t]
  m.u_t[M_t =="FER"] <- u.FER[t]
  m.u_t[M_t =="FQR"] <- u.FQR[t]
  m.u_t[M_t =="X"] <- 0
  
  return(m.u_t) 
}      

## PRODUCTIVITIES FUNCTION ----------------------------------------------------
prods <- function(M_t,t) { # gets the work productivity for each person based on health state M and age t
  
  m.w_t <- matrix(data= NA, nrow = n.i, ncol = 1)  
  
  m.w_t[M_t !="X"] <- w[t] 
  m.w_t[M_t =="X"] <- 0
  
  return(m.w_t) 
}      

## MODEL PREVALENCE RESULTS ------------------------------------------------
get_prevs_combined <- function(state,m.cohortbyyear,minyear,maxyear, denom=NULL, age_single_yr = FALSE){ # Get counts/prevalence of individuals in a health state by age group and year
  
  if(age_single_yr==TRUE){
    v.agerownames <- v.agegroupstart <- v.agegroupend <- c(0:99)
  } else {
    v.agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
    v.agegroupstart <- c(18, 18,26,35,50,65)
    v.agegroupend <- c(99,25,34,49,64,99,99)  
  }
  
  m.m_prevs <- NULL 
  for (age in 1:length(v.agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-v.agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-v.agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      dead <- sum(select=="X",na.rm=TRUE) 
      if(sum(select!="X",na.rm=TRUE)==0){
        prev==0  
      }
      numerator <- sum(str_count(select,state),na.rm=TRUE)
      if(is.null(denom)){ # if there is no specified subgroup, sum across the whole population that is alive
        denominator <- sum(select!="X",na.rm=TRUE)
      } else { # otherwise sum across the subpopulation with the specified health state(s)
        denominator <- sum(sapply(denom, function(x) str_count(select,x)), na.rm=TRUE)
      }
      prev <- numerator/denominator  
      
      m.m_prevs<-rbind(m.m_prevs,c(v.agerownames[age],year, prev,numerator,denominator,dead))
    }
  }
  colnames(m.m_prevs)<-c("age","year", "prev","counts","alive","dead")
  m.m_prevs[m.m_prevs=="NaN"] <- 0
  return(m.m_prevs) 
}

get_Fprevs1 <- function(ysq,m.F_cy,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  age1 <- c(0:99)
  m.F_prevs <- NULL 
  for (a in 1:length(age1)){
    for (year in minyear:maxyear){
      cohortmin = year-age1[a]
      if(cohortmin<1900) {next}
      cohortmax = year-age1[a]
      select = m.F_cy[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      counts <- sum(select==ysq,na.rm=TRUE)
      denom <- sum(select!=0 ,na.rm=TRUE) 
      prev <- counts/denom
      m.F_prevs<-rbind(m.F_prevs,c(age1[a],year, prev,counts,denom))
    }
  }
  colnames(m.F_prevs)<-c("age","year", "prev","counts","denom")
  m.F_prevs[m.F_prevs=="NaN"] <- 0
  return(m.F_prevs) 
}

# Function to extract and format prevalence data
extract_prevalence <- function(l.model_prevs1, state,v.age_range, v.year_range) {
  prev <- xtabs(prev ~ age + year, data = l.model_prevs1[[state]])
  attr(prev, "class") <- attr(prev, "call") <- NULL
  prev[as.character(v.age_range), as.character(v.year_range)]
}

# Convert matrix of health states from cohort-age to cohort-calendaryear (cy)
cohortage_to_cohortyear <- function(m.M){
  m.M_cy <- matrix(nrow = n.i*length(cohorts), ncol = (length(cohorts)+100), dimnames = list(NULL,c(min(cohorts):(max(cohorts)+100))))
  for (b in 1:length(cohorts)){
    m.M_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.M[(n.i*(b-1)+1):(n.i*b),]
  }
  return(m.M_cy)
}


reorder_by_age <- function(data) {
  data[order(data[,"age"], decreasing = FALSE),]
}

keep_X_with_left_D <- function(mat) {
  # Get matrix dimensions
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)
  # Create a matrix of NAs to store the result
  result <- matrix(NA, nrow = nrow_mat, ncol = ncol_mat)
  # Identify positions where mat == "X"
  X_positions <- which(mat == "X", arr.ind = TRUE)
  # Check if the left-adjacent cell contains "D" in its string
  valid_positions <- X_positions[X_positions[, 2] > 1 & grepl("D", mat[cbind(X_positions[, 1], X_positions[, 2] - 1)]), ]
  # Retain only valid "X" positions
  if (nrow(valid_positions) > 0) {
    result[cbind(valid_positions[, 1], valid_positions[, 2])] <- "X"
  }
  return(result)
}
## CALIBRATION FUNCTIONS ---------------------------------------------------

# Function to handle the repetitive task of looking up calib_inputs by parameter name
get_value <- function(param_name, v.params) {
  ifelse(m.calib_inputs[param_name, "calib"] == 1, v.params[param_name], m.calib_inputs[param_name, "value"])
}

# get log likelihood of the model output given the targets, sum likelihoods together
gof_norm_loglike <- function(target_mean, target_sd, model_output){
  sum(dnorm(x = target_mean,
            mean = model_output,
            sd = target_sd,
            log = TRUE))
}

# Apply filtering to relevant elements - drop age groups 50+ for e-cig use among depressed due to NA in log likelihood
filter_under_50 <- function(df) df[df[, "age"] < 50, ]

# Write goodness-of-fit function to pass to calibration algorithm
f_gof <- function(v.params){
  
  l.model_prevs <- main_calib(v.params)[[2]]
  
  # Apply filtering to relevant e-cig states ages <50 to avoid NA / Inf log likelihood values
  l.calib_targets[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")] <- lapply(l.calib_targets[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")], filter_under_50)
  l.model_prevs[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")] <- lapply(l.model_prevs[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")], filter_under_50)
  
  v.gof <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
 
  for (r in 1:length(l.calib_targets)){ # use log likelihood as metric
    gof = gof_norm_loglike(target_mean = l.calib_targets[[r]][,"prev"],
                           model_output = subset(l.model_prevs[[r]],l.model_prevs[[r]][,"year"]>=calib_startyear & l.model_prevs[[r]][,"year"]<=endyear)[,"prev"],
                           target_sd = l.calib_targets[[r]][,"se"])
    v.gof[r] <-gof
  }
  
  # OVERALL
  v.weights <- c(rep(1,length(1:n.target))) # can assign targets different weights
  # weighted sum
  GOF_overall <- sum(v.gof[1:n.target] * v.weights)
  cat(GOF_overall)
  # return GOF
  return(GOF_overall)
}

main_calib <- function(v.params,l.policy_effects=NULL) { # v.params: run model for parameter calibration; no policy effects
  t_init <- Sys.time() # Start timer

  # Loop over parameter names and assign values dynamically
  for (param in rownames(m.calib_inputs)) {
    assign(param, get_value(param,v.params))
  }
  
  yearinc_p.HD <- round(yearinc_p.HD)
  
  # Recovery
  p.DR=NULL
  p.DR[1:12] <- p.DR[100] <- 0 # final value = 0 because mortality prob = 1
  p.DR[13:65] <- p.DR_12.64
  p.DR[66:99] <- p.DR_65.99
  
  ## Incidence
  # scale up incidence by year for youth and young adults ages 12-34 from 2016-2100 vs 2016-2022
 
  if (is.null(l.policy_effects) || l.policy_effects[["s.HD_2100"]]==1) {
    p.HD[1:18,117:201] <- s.HD_12.17*p.HD[1:18,116] # ages <18, apply increase from 2016 (col 117) onwards
    p.HD[19:26,117:201] <- s.HD_18.25*p.HD[19:26,116] # ages 18-25, apply increase from 2016 (col 117) onwards
    p.HD[27:35,117:201] <- s.HD_26.34*p.HD[27:35,116] # ages 26-34, apply increase from 2016 (col 117) onwards
  } else {
    p.HD[1:18,117:123] <- s.HD_12.17*p.HD[1:18,116] # ages <18, apply increase from 2016-2022 (col 117 to 123) onwards 
    p.HD[19:26,117:123] <- s.HD_18.25*p.HD[19:26,116] # ages 18-25, apply increase from 2016 (col 117) onwards 
    p.HD[27:35,117:123] <- s.HD_26.34*p.HD[27:35,116] # ages 26-34, apply increase from 2016 (col 117) onwards 
  }
  if (whichgender=="females") {
    p.HD[13:22,1:116] <-p.HD[13:22,1:116]+calib.HD_2005_2015 #minor adjustment to incidence pre-2016
  } else {
    p.HD[13:29,1:116] <-p.HD[13:29,1:116]+calib.HD_2005_2015
  }
  # "calib.HD_2005_2015"
  #Apply policy effects here
  #Initiation and Cessation for Healthy
  ## Initiation - No initiation after 25
  p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(s.NC_26.34,9),rep(0,65))
  p.CF = smk_cess*c(rep(0,16),rep(s.CF_15.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
  ## Cessation - No cessation before 18
  p.NC[,119:201] = smk_init[,119:201]*c(rep(s.NC_18.23_9.17,18),rep(s.NC_18.23_18.25,8),rep(s.NC_18.23_26.34,9),rep(0,65))
  p.CF[,119:201] = smk_cess[,119:201]*c(rep(0,16),rep(s.CF_18.23_15.25,10), rep(s.CF_18.23_26.34,9),rep(s.CF_18.23_35.49,15),rep(s.CF_18.23_50.64,15),rep(s.CF_18.23_65.99,35))
  ## Initiation and Cessation for Depressed scaling factors - No initiation after 25 
  ## Cessation - No cessation before 18
  p.NC_D = smk_init*c(rep(s.NC_D_9.17,18),rep(s.NC_D_18.25,8),rep(s.NC_D_26.34,9),rep(0,65))
  # Vaping transition probabilities
  p.NO.NE[13:18,c("2020","2021")] <- p.NO.NE_20.21_12.17
  p.NO.NE[13:18,paste0(2022:endyear)] <- p.NO.NE_22.23_12.17
  
  p.NO.NE[19:26,c("2020","2021")] <- p.NO.NE_20.21_18.25
  p.NO.NE[19:26,paste0(2022:endyear)] <- p.NO.NE_22.23_18.25
  
  p.CO.CE[19:26,paste0(2022:endyear)] <- p.CO.CE_22.23_18.25
  p.CO.CE[27:35,paste0(2022:endyear)] <- p.CO.CE_22.23_26.34
  p.CO.CE[36:50,paste0(2022:endyear)] <- p.CO.CE_22.23_35.49
  p.CO.CE[51:65,paste0(2022:endyear)] <- p.CO.CE_22.23_50.64
  
  p.FO.FE[27:35,paste0(2022:endyear)] <- p.FO.FE_22.23_26.34
  p.FO.FE[36:50,paste0(2022:endyear)] <- p.FO.FE_22.23_35.49
  
  #need to keep a version that is no policy for vaping initiation calculation below
  p.NC_nopolicy=p.NC 
  
  #initiation policy effects
  if (!is.null(l.policy_effects) && l.policy_effects[["rr.init_1"]]!=1){ 
    p.NC[,paste0(policyyear)] <- p.NC[,paste0(policyyear)]*l.policy_effects[["rr.init_1"]]
    p.NC[,paste0((policyyear+1):2100)] <- p.NC[,paste0((policyyear+1):2100)]*l.policy_effects[["rr.init_s"]]
    p.NC_D[,paste0(policyyear)] <- p.NC_D[,paste0(policyyear)]*l.policy_effects[["rr.init_1"]]
    p.NC_D[,paste0((policyyear+1):2100)] <- p.NC_D[,paste0((policyyear+1):2100)]*l.policy_effects[["rr.init_s"]]
  }
  #cessation policy effects
  if (!is.null(l.policy_effects) && l.policy_effects[["rr.cess_1"]]!=1){
    p.CF[16:100,paste0(policyyear)] <- l.policy_effects[["rr.cess_1"]]
    p.CF[,paste0((policyyear+1):2100)] <- l.policy_effects[["rr.cess_s"]]
  }
  #Vaping initiation
  if (!is.null(l.policy_effects) && l.policy_effects[["p.NO.NE_1"]]!=1){
  p.NO.NE[13:91,paste0(policyyear)] <- p.NO.NE[13:91,paste0(policyyear)]+(l.policy_effects[["p.NO.NE_1"]]* p.NC_nopolicy[13:91,paste0(policyyear)])
  p.NO.NE[13:91,paste0((policyyear+1):endyear)] <- as.matrix(p.NO.NE[13:91,paste0((policyyear+1):endyear)]+(l.policy_effects[["p.NO.NE_s"]]* p.NC_nopolicy[13:91,paste0((policyyear+1):endyear)]))
  }
  #Smokers taking up vaping
  if (!is.null(l.policy_effects) && l.policy_effects[["p.CO.CE_1"]]!=1){
  p.CO.CE[13:91,paste0(policyyear)] <- l.policy_effects[["p.CO.CE_1"]]
  p.CO.CE[13:91,paste0((policyyear+1):endyear)] <- l.policy_effects[["p.CO.CE_s"]]
  }
  #Switching products
  if (!is.null(l.policy_effects) && l.policy_effects[["p.CO.FE_1"]]!=1){
  p.CO.FE[13:91,paste0(policyyear)] <- l.policy_effects[["p.CO.FE_1"]]
  p.CO.FE[13:91,paste0((policyyear+1):endyear)] <- l.policy_effects[["p.CO.FE_s"]]
  }
  
  #Vaping mortality effects. Baseline scenario assumes .1
  if (is.null(l.policy_effects)){
    p.EX<-p.NX+((p.NX-p.CX)*0.1)
  }else{ 
    p.EX<-p.NX+((p.NX-p.CX)*l.policy_effects[["s.EX"]])
  }
  
  p.CF[p.CF > 1] <- 1 # replace any cessation probabilities that are greater than 1 with 1

  # Simulate for each birth cohort with parallelization: row = each person within birth cohort, columns = ages 0:99
  m.M <- foreach(i = cohorts, .combine = 'rbind', .packages = 'darthtools',
                 .options.RNG = seednew,
                 .export = c('mds_microsim','probs','get_prevs_combined',
                             'n.i','n.t','v.n','n.s','v.M_1',
                             'p.NC','p.CF','p.NC_D','p.NX','p.EX','p.CX','a_p.FX.ysq',
                             'p.HD', 'p.DR', 'p.RD',
                             'rr.CH.CD','rr.CR.CD','rr.CD.FD',
                             'p.NO.NE', 'p.CO.CE', 'p.FO.FE',
                             'p.NE.NQ', 'p.CE.CQ', 'p.FE.FQ',
                             'p.NQ.NE', 'p.CQ.CE', 'p.FQ.FE',
                             'p.CO.FE','rr.OD.ED','seednew')) %dorng% {
                               mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
                             }
  
  # To run in serial for debugging purposes, uncomment the line below, and comment out the 'foreach' loop above
  # m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Convert matrix from cohort-age to cohort-calendaryear (cy)
  m.M_cy <- cohortage_to_cohortyear(m.M)
  colnames(m.M_cy) <- c(min(cohorts):(max(cohorts)+100))
  
  # Output prevalence results as a list
  l.model_prevs <- lapply(c("N","C","F","D"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom=NULL, minyear=calib_startyear, maxyear=endyear) # denominator is everyone still alive
  l.model_prevs_E <- lapply(c("E","NE","CE","FE"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom=NULL, minyear=2020, maxyear=endyear) # e-cig data available from 2020 onwards
  l.model_prevs_D <- lapply(c("N.D","C.D","F.D"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom="D", minyear=calib_startyear, maxyear=endyear) # denominator is everyone still alive
  l.model_prevs_D_E <- lapply(c("ED","NED","CED","FED"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom="D", minyear=2020, maxyear=endyear) # denominator is everyone still alive
  l.model_prevs <- c(l.model_prevs,l.model_prevs_E, l.model_prevs_D, l.model_prevs_D_E)
  names(l.model_prevs) <- c("N","C","F","D","E","NE","CE","FE","N_D","C_D","F_D","E_D","NE_D","CE_D","FE_D")
  
  l.model_prevs <- lapply(l.model_prevs, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc

  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer

  return(list(m.M,l.model_prevs,p.NC,p.CF,p.NC_D,rr.CD.FD, p.EX))
  
}

## MAIN POLICY FUNCTIONS ------------------------------------------
main <- function(v.params, l.policy_effects=NULL, policy) { # v.params: run model for parameter calibration; l.policy_effects: policy effects
  t_init <- Sys.time() 
  l.main_calib_outputs<-main_calib(v.params,l.policy_effects)
  m.M<-l.main_calib_outputs[[1]]
  l.model_prevs<-l.main_calib_outputs[[2]]
  #p.NC,p.CF,p.NC_D,rr.CD.FD. for outputs later
  p.NC<-l.main_calib_outputs[[3]]
  p.CF<-l.main_calib_outputs[[4]]
  p.NC_D<-l.main_calib_outputs[[5]]
  rr.CD.FD<-l.main_calib_outputs[[6]]
  p.EX<-l.main_calib_outputs[[7]]

  # To run in serial for debugging purposes, uncomment the line below, and comment out the 'foreach' loop above
  # m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Count each death only once by removing repeated 'X' values
  firstX = apply(m.M,1,function(x) min(which(x=="X"))) # find first cell where each individual dies by iterating across each person (row = 1)
  for (i in 1:nrow(m.M)){
    if(firstX[i]!=101 & firstX[i]!=Inf){
      m.M[i, (firstX[i]+1):101]<-NA
    }
  }
  
  m.M_D <- m.M # create a matrix specifically for people with current depression "D"
  m.M_D[grepl(pattern = "H|R|X", x = m.M_D)] <- NA # remove people without current depression and dead people
  m.M_notD <- m.M
  m.M_notD[grepl(pattern = "D|X", x = m.M_notD)] <- NA # remove people without current depression and dead people
  
  # Calculate costs, utilities, and productivity at each person's age
  #here we are just making the matrix to hold costs and utilities. then below in for loop we caluclate
  m.C <- m.U <- m.W <- m.B <- m.F <- m.C_D <- m.U_D <- m.W_D <- m.B_D <- m.F_D <- m.C_notD <- m.U_notD <- m.W_notD <- m.B_notD <- m.F_notD <- matrix(nrow = n.i*length(cohorts), ncol = n.t + 1, 
                                            dimnames = list(paste(rep(cohorts,each=n.i), 1:n.i, sep = "."), # each individual, year of birth
                                                            paste(0:n.t, sep = " ")))  
  for (t in 1:n.t) {
    # general population
    m.C[, t] <- costs(m.M[, t],t)
    m.U[, t] <- utils(m.M[, t],t)
    m.W[, t] <- prods(m.M[, t],t)
    m.B[, t] <- cons_exp(m.M[, t],t) # all non-med consumer expenditures
    # population with current MDE
    m.C_D[, t] <- costs(m.M_D[, t],t)
    m.U_D[, t] <- utils(m.M_D[, t],t)
    m.W_D[, t] <- prods(m.M_D[, t],t)
    m.B_D[, t] <- cons_exp(m.M_D[, t],t)
    # population with NO current MDE
    m.C_notD[, t] <- costs(m.M_notD[, t],t)
    m.U_notD[, t] <- utils(m.M_notD[, t],t)
    m.W_notD[, t] <- prods(m.M_notD[, t],t)
    m.B_notD[, t] <- cons_exp(m.M_notD[, t],t)
  }
  
  # Create new matrix to keep track of years since quitting among former smoking persons
  for (r in 1:nrow(m.F)){
    m.F[r,] <- cumsum(str_count(m.M[r,],"F")) # general population
    m.F_D[r,] <- cumsum(str_count(m.M_D[r,],"F")) # population with current MDE
    m.F_notD[r,] <- cumsum(str_count(m.M_notD[r,],"F")) # population with current MDE
  }
  m.F[m.F>40] <- 40 # maxes out at 40 years since quitting
  m.F_D[m.F_D>40] <- 40 # maxes out at 40 years since quitting
  m.F_notD[m.F_notD>40] <- 40 # maxes out at 40 years since quitting
  
  # Convert matrix from cohort-age to cohort-calendaryear (cy)
  m.M <- cohortage_to_cohortyear(m.M) # general population
  m.C <- cohortage_to_cohortyear(m.C)
  m.U <- cohortage_to_cohortyear(m.U)
  m.W <- cohortage_to_cohortyear(m.W)
  m.B <- cohortage_to_cohortyear(m.B)
  m.F <- cohortage_to_cohortyear(m.F)
  
  m.M_D <- cohortage_to_cohortyear(m.M_D) # population with current MDE
  m.C_D <- cohortage_to_cohortyear(m.C_D)
  m.U_D <- cohortage_to_cohortyear(m.U_D)
  m.W_D <- cohortage_to_cohortyear(m.W_D)
  m.B_D <- cohortage_to_cohortyear(m.B_D)
  m.F_D <- cohortage_to_cohortyear(m.F_D)
  
  m.M_notD <- cohortage_to_cohortyear(m.M_notD) # population with NO current MDE
  m.C_notD <- cohortage_to_cohortyear(m.C_notD)
  m.U_notD <- cohortage_to_cohortyear(m.U_notD)
  m.W_notD <- cohortage_to_cohortyear(m.W_notD)
  m.B_notD <- cohortage_to_cohortyear(m.B_notD)
  m.F_notD <- cohortage_to_cohortyear(m.F_notD)
  
  #check difference between baseline and scenario, if deaths difference is less than 2 set deaths to 0.
  #if life years differen
  if (is.null(l.policy_effects)) {
    v.X = apply(m.M,2,function(x) sum(x=="X" ,na.rm=TRUE))
    v.X_D = apply(keep_X_with_left_D(m.M),2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's just among people who are depressed
    baselinev.X <<-v.X
    baselinev.X_D <<-v.X_D
    
  } else {
    
    v.X = apply(m.M,2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's
    v.X_D = apply(keep_X_with_left_D(m.M),2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's just among people who are depressed
    small_diff <- abs(baselinev.X - v.X) < 0
    small_diff_D <- abs(baselinev.X_D - v.X_D) < 0
    
    v.X[small_diff] <- baselinev.X[small_diff]
    v.X_D[small_diff_D] <- baselinev.X_D[small_diff_D]
    }
  
  ## Get mortality counts by year, scaled to US population estimates of mortality
  s.X = (deaths / v.X["2023"]) # Total number of US deaths in 2023: 3,279,857, so scale up the number of deaths by the 2022 ratio to reflect all US deaths. Each X = 4298.644 deaths
  v.X_totalpop = v.X * s.X
  v.X_Dpop = v.X_D * s.X
  v.X_notDpop = v.X_totalpop - v.X_Dpop
  v.X_totalpop =  v.X_totalpop[paste0(d.year:max(cohorts))]  # keep only years of interest
  v.X_Dpop = v.X_Dpop[paste0(d.year:max(cohorts))]
  v.X_notDpop=v.X_notDpop[paste0(d.year:max(cohorts))]
  
  ## Get total person life-years by year, scaled to US population estimates of annual births 
  m.personyears <- m.M[,paste0(cohorts)]
  m.personyears[!is.na(m.personyears)] <- 1
  m.personyears <- apply(m.personyears, 2, as.numeric)
  
  m.personyears_D <- m.M_D[,paste0(cohorts)]
  m.personyears_D[!is.na(m.personyears_D)] <- 1
  m.personyears_D <- apply(m.personyears_D, 2, as.numeric)
  
  row_groups <- rep(1:length(cohorts), each=n.i) # group rows for each birth cohort
  
  # get number of personyears for each birth cohort as a row
  m.personyears_bc <- rowsum(m.personyears, group=row_groups,na.rm=TRUE)
  m.personyears_totalpop = m.personyears_bc * births[paste0(cohorts),] / n.i # scale up personyears for each birth cohort based on the number of actual births
  
  m.personyears_bc_D <- rowsum(m.personyears_D, group=row_groups,na.rm=TRUE)
  m.personyears_Dpop <- m.personyears_bc_D* births[paste0(cohorts),] / n.i 
  m.personyears_notDpop <- m.personyears_totalpop-m.personyears_Dpop
  
  # Person life-years scaled to US population estimates
  #if life years differen
  if (is.null(l.policy_effects)) {
    v.lifeyears_totalpop  = colSums(m.personyears_totalpop)[paste0(d.year:max(cohorts))]
    v.lifeyears_Dpop  = colSums(m.personyears_Dpop)[paste0(d.year:max(cohorts))]
    v.lifeyears_notDpop  = colSums(m.personyears_notDpop)[paste0(d.year:max(cohorts))]
    
    #save baseline scenarios values
    baselinev.lifeyears_totalpop<<-v.lifeyears_totalpop
    baselinev.lifeyears_Dpop<<-v.lifeyears_Dpop
    baselinev.lifeyears_notDpop<<-v.lifeyears_notDpop
  } else {
    v.lifeyears_totalpop = colSums(m.personyears_totalpop)[paste0(d.year:max(cohorts))]
    v.lifeyears_Dpop = colSums(m.personyears_Dpop)[paste0(d.year:max(cohorts))]
    v.lifeyears_notDpop = colSums(m.personyears_notDpop)[paste0(d.year:max(cohorts))]
    
    small_diff <- abs(baselinev.lifeyears_totalpop - v.lifeyears_totalpop) < 0
    small_diff_D <- abs(baselinev.lifeyears_Dpop - v.lifeyears_Dpop) < 0
    small_diff_notDpop <- abs(baselinev.lifeyears_notDpop - v.lifeyears_notDpop) < 0
    
    
    v.lifeyears_totalpop[small_diff] <- baselinev.lifeyears_totalpop[small_diff]
    v.lifeyears_Dpop[small_diff_D] <- baselinev.lifeyears_Dpop[small_diff_D]
    v.lifeyears_notDpop[small_diff_notDpop] <- baselinev.lifeyears_notDpop[small_diff_notDpop]
  }
  
  # Person life-years, NOT scaled
  v.lifeyears = apply(m.M,2,function(x) sum(x!="X",na.rm=TRUE))[paste0(d.year:max(cohorts))]
  v.lifeyears_D = apply(m.M_D,2,function(x) sum(x!="X",na.rm=TRUE))[paste0(d.year:max(cohorts))]
  v.lifeyears_notD = apply(m.M_notD,2,function(x) sum(x!="X",na.rm=TRUE))[paste0(d.year:max(cohorts))]
  
  #Get mortality rate by year for each state.Check this with Jamie
  v.deathrate=v.X[paste0(d.year:max(cohorts))]/v.lifeyears
  v.deathrate_D=v.X_D[paste0(d.year:max(cohorts))]/v.lifeyears_D
  v.deathrate_notD=(v.X[paste0(d.year:max(cohorts))] - v.X_D[paste0(d.year:max(cohorts))])/v.lifeyears_notD
  
  
  # Output prevalence results as a list -------------------------------------
  #l.model_prevs <- lapply(c("X"), get_prevs_combined, m.cohortbyyear=m.M, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  
  # by age group
  l.model_prevs <- lapply(c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q"), get_prevs_combined, m.cohortbyyear=m.M, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs) <- c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q")
  l.model_prevs <- lapply(l.model_prevs, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  l.model_prevs_D <- lapply(c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q"), get_prevs_combined, m.cohortbyyear=m.M_D, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs_D) <- c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q")
  l.model_prevs_D <- lapply(l.model_prevs_D, reorder_by_age) # Reorder age groups for both lists in a single lapply
  
  l.model_prevs_notD <- lapply(c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q"), get_prevs_combined, m.cohortbyyear=m.M_notD, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs_notD) <- c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q")
  l.model_prevs_notD <- lapply(l.model_prevs_notD, reorder_by_age) # Reorder age groups for both lists in a single lapply

  # by single year for general population
  l.model_prevs1 <- lapply(c("D","N","C","F","E","CE"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M, minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1 <- c(l.model_prevs1, lapply(c(1:40), get_Fprevs1, 
                                     m.F_cy=m.F, minyear=calib_startyear, 
                                     maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1) <- c("D","N","C","F","E","CE",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1 <- lapply(l.model_prevs1, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  # by single year for depressed population
  l.model_prevs1_D <- lapply(c("N.D","C.D","F.D","E.D"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M, denom="D", minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1_D <- c(l.model_prevs1_D, lapply(c(1:40), get_Fprevs1, 
                                     m.F_cy=m.F_D, minyear=calib_startyear, 
                                     maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1_D) <- c("N","C","F","E",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1_D <- lapply(l.model_prevs1_D, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  # by single year for NOT depressed population
  l.model_prevs1_notD <- lapply(c("N","C","F","E"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M_notD, minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1_notD <- c(l.model_prevs1_notD, lapply(c(1:40), get_Fprevs1, 
                                                 m.F_cy=m.F_notD, minyear=calib_startyear, 
                                                 maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1_notD) <- c("N","C","F","E",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1_notD <- lapply(l.model_prevs1_notD, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  

  # Economic outcomes -------------------------------------------------------
  v.year_range <- d.year:max(cohorts) # Specify years of interest
  v.age_range <- 0:99
  # Calculate costs, utilities, and productivity for each health state by age in the population
  m.total_cuw <- rbind(colSums(m.C[,paste0(v.year_range)],na.rm=TRUE),colSums(m.U[,paste0(v.year_range)],na.rm=TRUE),colSums(m.W[,paste0(v.year_range)],na.rm=TRUE),colSums(m.B[,paste0(v.year_range)],na.rm=TRUE),
                     colSums(m.C[,paste0(v.year_range)],na.rm=TRUE)+colSums(m.B[,paste0(v.year_range)],na.rm=TRUE)-colSums(m.W[,paste0(v.year_range)],na.rm=TRUE),v.lifeyears) # societal costs = medical costs + consumer expenditures (non-medical) -  productivity 
  
  m.total_cuw_D <- rbind(colSums(m.C_D[,paste0(v.year_range)],na.rm=TRUE),colSums(m.U_D[,paste0(v.year_range)],na.rm=TRUE),colSums(m.W_D[,paste0(v.year_range)],na.rm=TRUE),colSums(m.B_D[,paste0(v.year_range)],na.rm=TRUE),
                     colSums(m.C_D[,paste0(v.year_range)],na.rm=TRUE)+colSums(m.B_D[,paste0(v.year_range)],na.rm=TRUE)-colSums(m.W_D[,paste0(v.year_range)],na.rm=TRUE),v.lifeyears_D) # societal costs = medical costs + consumer expenditures (non-medical) -  productivity 
  
  m.total_cuw_notD <- m.total_cuw - m.total_cuw_D
  rownames(m.total_cuw) <- rownames(m.total_cuw_D) <- rownames(m.total_cuw_notD) <- c("med_costs","QALYs","productivity","consumer_exp","soc_costs","lifeyears")
  
  # Assume d.year is the starting year for discounting purposes, calculate discount weight based on the discount rate d.c
  v.d = c( c(1 / (1 + d.c) ^ (0:(ncol(m.total_cuw)-1)))) # vector of discount weights
  
  # Total discounted costs, QALYs, and productivity
  m.d_total_cuw <- m.total_cuw %*% diag(v.d) # multiple each row of the cuw matrix by the discounting vector
  #we wouldn't want dicsounting vector for qaly or life years right?
  m.d_total_cuw_D <- m.total_cuw_D %*% diag(v.d) # depressed pop
  m.d_total_cuw_notD <- m.total_cuw_notD %*% diag(v.d) # not depressed pop
  colnames(m.d_total_cuw) <- colnames(m.d_total_cuw_D)  <- colnames(m.d_total_cuw_notD) <- d.year:(max(cohorts))
  m.cuw <- t(m.d_total_cuw[,as.character(v.year_range)])
  m.cuw_D <- t(m.d_total_cuw_D[,as.character(v.year_range)])
  m.cuw_notD <- t(m.d_total_cuw_notD[,as.character(v.year_range)])
  
  #Dsicounting  life years and deaths for clarity.
  #new SAD
  v.X_totalpop_disc=v.X_totalpop*v.d
  v.X_Dpop_disc=  v.X_Dpop*v.d
  v.X_notDpop_disc=v.X_notDpop*v.d
  #new lifeyear metric
  v.lifeyears_totalpop_disc <- v.lifeyears_totalpop*v.d
  v.lifeyears_Dpop_disc <- v.lifeyears_Dpop*v.d
  v.lifeyears_notDpop_disc <- v.lifeyears_notDpop*v.d
  
  v.X_notD=v.X-v.X_D
  
  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer

  # Output results as two lists: one for general population, and one for depressed population
  l.results <- list(l.model_prevs = l.model_prevs, m.cuw=m.cuw, 
                    v.lifeyears=v.lifeyears,
                    v.SAD_new=v.X_totalpop, v.lifeyears_pop_new = v.lifeyears_totalpop, v.SAD_new_disc=v.X_totalpop_disc, v.lifeyears_pop_new_disc = v.lifeyears_totalpop_disc, 
                    init = p.NC, cess = p.CF, v.deathrate= v.deathrate,
                    v.X=v.X)

  l.results_D <- list(l.model_prevs = l.model_prevs_D, m.cuw=m.cuw_D, 
                      v.lifeyears=v.lifeyears_D,
                      v.SAD_new=v.X_Dpop, v.lifeyears_pop_new = v.lifeyears_Dpop, v.SAD_new_disc=v.X_Dpop_disc, v.lifeyears_pop_new_disc = v.lifeyears_Dpop_disc,
                      init = p.NC_D, cess = rr.CD.FD*p.CF, v.deathrate_D= v.deathrate_D,
                      v.X=v.X_D)

  l.results_notD <- list(l.model_prevs=l.model_prevs_notD, m.cuw=m.cuw_notD , 
                         v.lifeyears = v.lifeyears_notD, 
                         v.SAD_new=v.X_notDpop, v.lifeyears_pop_new = v.lifeyears_notDpop, v.SAD_new_disc=v.X_notDpop_disc, v.lifeyears_pop_new_disc = v.lifeyears_notDpop_disc,
                         init = p.NC, cess = p.CF,  v.deathrate_notD= v.deathrate_notD,
                         v.X=v.X)

  return(list(l.results=l.results, l.results_D = l.results_D, l.results_notD=l.results_notD))
}

# Keep only deaths among people who were depressed at the previous time step
keep_X_with_left_D <- function(mat) {
  # Create a matrix of NAs to store the result
  result <- mat
  result[,] <- NA
  # Identify positions where mat == "X"
  X_positions <- which(mat == "X", arr.ind = TRUE)
  # Check if the left-adjacent cell contains "D" in its string
  valid_positions <- X_positions[X_positions[, 2] > 1 & grepl("D", mat[cbind(X_positions[, 1], X_positions[, 2] - 1)]), ]
  # Retain only valid "X" positions
  if (nrow(valid_positions) > 0) {
    result[cbind(valid_positions[, 1], valid_positions[, 2])] <- "X"
  }
  return(result)
}

run_policy <- function(policy) {
  cat(paste0("\n  Scenario: ", policy))
  if (is.null(params[[policy]][1])){l.policy_effects <- NULL
  }else{
    l.policy_effects <- apply_policy(params[[policy]][1], params[[policy]][2],params[[policy]][3], params[[policy]][4],
                                     params[[policy]][5], params[[policy]][6],params[[policy]][7], params[[policy]][8],
                                     params[[policy]][9], params[[policy]][10],params[[policy]][11],params[[policy]][12], 
                                     policyyear, v.affected_ages)}
  output <- main(v.params, l.policy_effects, policy)
  return(output)
}

apply_policy <- function(rr.init_1,rr.init_s,rr.cess_1,rr.cess_s, p.CO.CE_1,p.CO.CE_s,p.CO.FE_1,p.CO.FE_s,p.NO.NE_1,p.NO.NE_s,s.EX, s.HD_2100, policyyear, v.affected_ages) {
  
  l.policy_effects <- list(rr.init_1=as.numeric(rr.init_1),rr.init_s=as.numeric(rr.init_s),
                           rr.cess_1=as.numeric(rr.cess_1),rr.cess_s=as.numeric(rr.cess_s),
                           p.CO.CE_1=as.numeric(p.CO.CE_1),p.CO.CE_s=as.numeric(p.CO.CE_s),
                           p.CO.FE_1=as.numeric(p.CO.FE_1),p.CO.FE_s=as.numeric(p.CO.FE_s),
                           p.NO.NE_1=as.numeric(p.NO.NE_1),p.NO.NE_s=as.numeric(p.NO.NE_s),
                           s.EX=as.numeric(s.EX), s.HD_2100=as.numeric(s.HD_2100))
  return(l.policy_effects)
}

## REFORMAT PPOLICY ANALYSIS DATA FOR VISUALIZATION ------------------------------------
reformat_model_outputs <- function(l.results){
  # Combine model prevalences for all health states and all scenarios into one dataframe
  df.model_prevs <- do.call(rbind, lapply(names(l.results), function(name) {
    v.health_states <- names(l.results[[name]]$l.model_prevs)
    df.model_prevs <- do.call(rbind, lapply(v.health_states, function(state) {
      cbind(data.frame(l.results[[name]]$l.model_prevs[[state]]), status = state)
    }))
    df.model_prevs$scenario <- name
    return(df.model_prevs)
  }))
  
  # Smoking initiation and cessation
  #this is just smoking init and cessation for the year 2100
  smkprobs <- do.call(rbind, lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    data.frame(cbind(policy$init[, endyear - 1899], policy$cess[, endyear - 1899], 0:99, name))
  }))
  colnames(smkprobs) <- c("init", "cess", "age", "scenario")
  smkprobs[, c("init", "cess", "age")] <- sapply(smkprobs[, c("init", "cess", "age")], as.numeric)
  
  # Mortality (X), life-years (ly), and cost-utility data for policy year to max cohort year (2100)
  combined_data <- lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    cuw <- policy$m.cuw
    print(name)
    data.frame(
      year = as.numeric(names(policy$v.lifeyears[paste0(policyyear:max(cohorts))])) ,
       scenario = name,
      aMort_perLY= policy$v.deathrate[paste0(policyyear:max(cohorts))],
      cMort_perLY= cumsum(policy$v.deathrate[paste0(policyyear:max(cohorts))]),
      aLY = policy$v.lifeyears[paste0(policyyear:max(cohorts))],
      cLY = cumsum(policy$v.lifeyears[paste0(policyyear:max(cohorts))]),
      #costs
      aQALYs_disc = cuw[paste0(policyyear:max(cohorts)), "QALYs"],
      cQALYs_disc = cumsum(cuw[paste0(policyyear:max(cohorts)), "QALYs"]),
      aMedCosts = cuw[paste0(policyyear:max(cohorts)), "med_costs"], #medical costs
      cMedCosts = cumsum(cuw[paste0(policyyear:max(cohorts)), "med_costs"]),
      aProd = cuw[paste0(policyyear:max(cohorts)), "productivity"],
      cProd = cumsum(cuw[paste0(policyyear:max(cohorts)), "productivity"]),
      aNonhealth = cuw[paste0(policyyear:max(cohorts)), "consumer_exp"],
      cNonhealth = cumsum(cuw[paste0(policyyear:max(cohorts)), "consumer_exp"]),
      aSocCosts = cuw[paste0(policyyear:max(cohorts)), "soc_costs"],
      cSocCosts = cumsum(cuw[paste0(policyyear:max(cohorts)), "soc_costs"]) ,

      #newly calculated mortality and life years
      #notdisc
      aLYpop_new= policy$v.lifeyears_pop_new[paste0(policyyear:max(cohorts))],
      cLYpop_new= cumsum(policy$v.lifeyears_pop_new[paste0(policyyear:max(cohorts))]),
      aSAD_new= policy$v.SAD_new[paste0(policyyear:max(cohorts))],
      cSAD_new= cumsum(policy$v.SAD_new[paste0(policyyear:max(cohorts))]),
      #disc
      aLYpop_new_disc= policy$v.lifeyears_pop_new_disc[paste0(policyyear:max(cohorts))],
      cLYpop_new_disc= cumsum(policy$v.lifeyears_pop_new_disc[paste0(policyyear:max(cohorts))]),
      aSAD_new_disc= policy$v.SAD_new_disc[paste0(policyyear:max(cohorts))],
      cSAD_new_disc= cumsum(policy$v.SAD_new_disc[paste0(policyyear:max(cohorts))])
    )
  }) %>%
    bind_rows()
  
  # Ensure numeric columns are indeed numeric
  combined_data <- combined_data %>%
    mutate(across(c(year, aLY, cLY, aMort_perLY, cMort_perLY,,aQALYs_disc,cQALYs_disc,
                    aMedCosts,cMedCosts, aProd,cProd,aNonhealth,cNonhealth,aSocCosts,cSocCosts,
                    aLYpop_new,cLYpop_new,aSAD_new,cSAD_new,
                    aLYpop_new_disc,cLYpop_new_disc,aSAD_new_disc,cSAD_new_disc
                    ), as.numeric))
  
  # Extract baseline values
  baseline_values <- combined_data %>%
    filter(scenario == "baseline") %>%
    select(year, aLY, cLY, aMort_perLY, cMort_perLY,,aQALYs_disc,cQALYs_disc,
           aMedCosts,cMedCosts, aProd,cProd,aNonhealth,cNonhealth,aSocCosts,cSocCosts,
           aLYpop_new,cLYpop_new,aSAD_new,cSAD_new,
           aLYpop_new_disc,cLYpop_new_disc,aSAD_new_disc,cSAD_new_disc
           ) %>%
    dplyr::rename(
      baseline_aLY= aLY, 
      baseline_cLY=cLY, 
      baseline_aMort_perLY=aMort_perLY, 
      baseline_cMort_perLY=cMort_perLY,
      baseline_aQALYs_disc=aQALYs_disc,
      baseline_cQALYs_disc=cQALYs_disc,
      baseline_aMedCosts=aMedCosts,
      baseline_cMedCosts=cMedCosts, 
      baseline_aProd=aProd,
      baseline_cProd=cProd,
      baseline_aNonhealth=aNonhealth,
      baseline_cNonhealth=cNonhealth,
      baseline_aSocCosts=aSocCosts,
      baseline_cSocCosts=cSocCosts,
      baseline_aLYpop_new=aLYpop_new,
      baseline_cLYpop_new=cLYpop_new,
      baseline_aSAD_new=aSAD_new,
      baseline_cSAD_new=cSAD_new,
      baseline_aLYpop_new_disc=aLYpop_new_disc,
      baseline_cLYpop_new_disc=cLYpop_new_disc,
      baseline_aSAD_new_disc=aSAD_new_disc,
      baseline_cSAD_new_disc=cSAD_new_disc
    )
  
  # Join baseline values with main data frame and calculate difference in values
  # Mortality and other outcomes
  
  combined_data <- combined_data %>%
    left_join(baseline_values, by = "year") %>%
    mutate(
      
      #new mortality outcomes
      #not discounted
      aLYG_new= aLYpop_new-baseline_aLYpop_new,
      cLYG_new=cLYpop_new-baseline_cLYpop_new,
      aSAD_averted_new=baseline_aSAD_new-aSAD_new,
      cSAD_averted_new=baseline_cSAD_new-cSAD_new,
      #discounted
      aLYG_new_disc=aLYpop_new_disc-baseline_aLYpop_new_disc,
      cLYG_new_disc=cLYpop_new_disc-baseline_cLYpop_new_disc,
      aSAD_averted_new_disc=baseline_aSAD_new_disc-aSAD_new_disc,
      cSAD_averted_new_disc=baseline_cSAD_new_disc-cSAD_new_disc
      
    ) %>%
    select( -baseline_aLY, -baseline_cLY, -baseline_aMort_perLY, -baseline_cMort_perLY, -baseline_aQALYs_disc, 
            -baseline_cQALYs_disc, -baseline_aMedCosts, -baseline_cMedCosts, -baseline_aProd, -baseline_cProd, -baseline_aNonhealth,
            -baseline_cNonhealth, -baseline_aSocCosts, -baseline_cSocCosts, 
            
            -baseline_aLYpop_new, -baseline_cLYpop_new, -baseline_aSAD_new, -baseline_cSAD_new, -baseline_aLYpop_new_disc, 
            -baseline_cLYpop_new_disc, -baseline_aSAD_new_disc, -baseline_cSAD_new_disc)  # Remove temporary baseline columns
  
  # Calculate ICER
  cea_data <- lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    cuw <- policy$m.cuw
    data.frame(scenario = name,
               avg_med_costs = mean(cuw[paste0(policyyear:max(cohorts)), "med_costs"]),
               avg_cons_exp = mean(cuw[paste0(policyyear:max(cohorts)), "consumer_exp"]),
               avg_prod = mean(cuw[paste0(policyyear:max(cohorts)), "productivity"]),
               avg_soc_costs = mean(cuw[paste0(policyyear:max(cohorts)), "soc_costs"]),
               avg_QALYs_disc = mean(cuw[paste0(policyyear:max(cohorts)), "QALYs"]),
               avg_LYs_disc = mean(cuw[paste0(policyyear:max(cohorts)), "lifeyears"])
    )
  }) %>%
    bind_rows()
  
  df.cea <- calc_icers(cea_data)
  return(list(df.model_prevs, smkprobs, combined_data, df.cea))
}


calc_icers <- function(cea_data) {
  if (nrow(cea_data) > 1) {
    # cea_data[1, "icer"] <- NA # First scenario "baseline" is the reference case
    for (i in 2:nrow(cea_data)) {
      inc_med_cost <- (cea_data[i, "avg_med_costs"] - cea_data[1, "avg_med_costs"])
      inc_soc_cost <- (cea_data[i, "avg_soc_costs"] - cea_data[1, "avg_soc_costs"])
      inc_cons_exp <- (cea_data[i, "avg_cons_exp"] - cea_data[1, "avg_cons_exp"])
      inc_prod <- (cea_data[i, "avg_prod"] - cea_data[1, "avg_prod"])
      inc_effectQALY_disc <- (cea_data[i, "avg_QALYs_disc"] - cea_data[1, "avg_QALYs_disc"])
      inc_effectLY_disc <- (cea_data[i, "avg_LYs_disc"] - cea_data[1, "avg_LYs_disc"]) #these are discounted life years

      cea_data[i, "inc_med_cost"] <- inc_med_cost
      cea_data[i, "inc_cons_exp"] <- inc_cons_exp
      cea_data[i, "inc_prod"] <- inc_prod
      cea_data[i, "inc_soc_cost"] <- inc_soc_cost
      cea_data[i, "inc_effectQALY_disc"] <- inc_effectQALY_disc
      cea_data[i, "inc_effectLY_disc"] <- inc_effectLY_disc
      cea_data[i, "icer_medLY"] <- round(inc_med_cost / inc_effectLY_disc,0)
      cea_data[i, "icer_medQALY"] <- round(inc_med_cost / inc_effectQALY_disc,0)
      cea_data[i, "icer_socLY"] <- round(inc_soc_cost / inc_effectLY_disc,0)
      cea_data[i, "icer_socQALY"] <- round(inc_soc_cost / inc_effectQALY_disc,0)
      cea_data[i, "icer_consLY"] <- round(inc_cons_exp / inc_effectLY_disc,0)
      cea_data[i, "icer_consQALY"] <- round(inc_cons_exp / inc_effectQALY_disc,0)
      cea_data[i, "icer_prodLY"] <- round(inc_prod / inc_effectLY_disc,0)
      cea_data[i, "icer_prodQALY"] <- round(inc_prod / inc_effectQALY_disc,0)
    }
  } 
  return(cea_data)
}

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
