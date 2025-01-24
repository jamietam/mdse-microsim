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
  
  bc1 = bc-1899
  yr = bc + t - 1899
  
  # create matrix of state transition probabilities
  m.p_t <- matrix(data = 0, nrow = length(v.n), ncol = n.i)  
  # give the state names to the rows
  rownames(m.p_t) <-  v.n                               
  
  ##transition probabilities calculations
  #from NHO state
  m.p_t["NOH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NO.NE[t,yr]-p.HD[t,bc1])
  m.p_t["COH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NEH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NO.NE[t,yr])
  m.p_t["NOD", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "NOH"] <- p.NX[t,bc1]
  
  #from CHO state
  m.p_t["COH", M_t == "COH"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CO.CE[t,yr]-p.HD[t,bc1]-p.CO.FE[t,bc1])
  m.p_t["FOH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["FEH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CO.FE[t,bc1])
  m.p_t["CEH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CO.CE[t,yr])
  m.p_t["COD", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "COH"] <- p.CX[t,bc1]
  
  ##from FHO state
  m.p_t["FOH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(1-p.FO.FE[t,yr]-p.HD[t,bc1])
  m.p_t["FEH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.FO.FE[t,yr])
  m.p_t["FOD", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FOH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]]
  
  ##from NHE state
  m.p_t["NEH", M_t == "NEH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NE.NQ[t,yr]-p.HD[t, bc1])
  m.p_t["CEH", M_t == "NEH"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NQH", M_t == "NEH"] <- (1-p.NX[t,bc1])*(p.NE.NQ[t,yr])
  m.p_t["NED", M_t == "NEH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X" , M_t == "NEH"] <- p.NX[t,bc1]
  
  ##from CHE state
  m.p_t["CEH", M_t == "CEH"] <- (1-p.CX[t,bc1])*(1-p.CE.CQ[t,yr]-p.CF[t,bc1]-rr.CH.CD*p.HD[t,bc1])
  m.p_t["FEH", M_t == "CEH"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["CQH", M_t == "CEH"] <- (1-p.CX[t,bc1])*(p.CE.CQ[t,yr])
  m.p_t["CED", M_t == "CEH"] <- (1-p.CX[t,bc1])*(rr.CH.CD*p.HD[t,bc1])
  m.p_t["X", M_t == "CEH"] <- p.CX[t,bc1]
  
  ##from FHE state
  m.p_t["FEH", M_t == "FEH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FEH"]])*(1-p.FE.FQ[t,yr]-p.HD[t,bc1])
  m.p_t["FQH", M_t == "FEH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FEH"]])*(p.FE.FQ[t,yr])
  m.p_t["FED", M_t == "FEH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FEH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FEH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FEH"]]
  
  ##from NHQ state
  m.p_t["NQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NQ.NE[t,yr]-p.HD[t,bc1])
  m.p_t["CQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NEH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQ.NE[t,yr])
  m.p_t["NQD", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "NQH"] <- p.NX[t,bc1]
  
  ##from CHQ state
  m.p_t["CQH", M_t == "CQH"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.HD[t,bc1])
  m.p_t["FQH", M_t == "CQH"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
  m.p_t["CQD", M_t == "CQH"] <- (1-p.CX[t,bc1])*p.HD[t,bc1]
  m.p_t["X", M_t == "CQH"] <- p.CX[t,bc1]
  
  ##from FHQ state
  m.p_t["FQH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(1-p.FQ.FE[t,yr]-p.HD[t,bc1])
  m.p_t["FEH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.FQ.FE[t,yr])
  m.p_t["FQD", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FQH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]]
  
  ##from NDO state
  m.p_t["NOD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(1-p.NC_D[t,bc1]-p.NO.NE[t,yr]-p.DR[t])
  m.p_t["COD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NC_D[t,bc1])
  m.p_t["NED", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NO.NE[t,yr])
  m.p_t["NOR", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NOD"] <- p.NX[t,bc1]
  
  ##from CDO state
  m.p_t["COD", M_t == "COD"] <- (1-p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-p.CO.CE[t,yr]-p.DR[t]-p.CO.FE[t,bc1])
  m.p_t["FOD", M_t == "COD"] <- (1-p.CX[t,bc1])*(rr.CD.FD*p.CF[t,bc1])
  m.p_t["FED", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.CO.FE[t,bc1])
  m.p_t["CED", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.CO.CE[t,yr])
  m.p_t["COR", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "COD"] <- p.CX[t,bc1]

  ##from FDO state
  m.p_t["FOD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(1-p.FO.FE[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.FO.FE[t,yr])
  m.p_t["FOR", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.DR[t])
  m.p_t["X", M_t == "FOD"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]]
  
  ##from NDE state
  m.p_t["NED", M_t == "NED"] <- (1-p.NX[t,bc1])*(1-p.NC_D[t,bc1]-p.NE.NQ[t,yr]-p.DR[t])
  m.p_t["CED", M_t == "NED"] <- (1-p.NX[t,bc1])*(p.NC_D[t,bc1])
  m.p_t["NQD", M_t == "NED"] <- (1-p.NX[t,bc1])*(p.NE.NQ[t,yr])
  m.p_t["NER", M_t == "NED"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NED"] <- p.NX[t,bc1]
  
  ##from CDE state
  m.p_t["CED", M_t == "CED"] <- (1-p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-p.CE.CQ[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "CED"] <- (1-p.CX[t,bc1])*(rr.CD.FD*p.CF[t,bc1])
  m.p_t["CQD", M_t == "CED"] <- (1-p.CX[t,bc1])*(p.CE.CQ[t,yr])
  m.p_t["CER", M_t == "CED"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "CED"] <- p.CX[t,bc1]
  
  ##from FDE state
  m.p_t["FED", M_t =="FED"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FED"]])*(1-p.FE.FQ[t,yr]-p.DR[t])
  m.p_t["FQD", M_t =="FED"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FED"]])*(p.FE.FQ[t,yr])
  m.p_t["FER", M_t =="FED"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FED"]])*(p.DR[t])
  m.p_t["X", M_t == "FED"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FED"]]
  
  ##from NDQ state
  m.p_t["NQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(1-p.NQ.NE[t,yr]-p.NC_D[t,bc1]-p.DR[t])
  m.p_t["NED", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQ.NE[t,yr])
  m.p_t["CQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NC_D[t,bc1])
  m.p_t["NQR", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NQD"] <- p.NX[t,bc1]
  
  ##from CDQ state
  m.p_t["CQD", M_t == "CQD"] <- (1-p.CX[t,bc1])*(1-rr.CD.FD*p.CF[t,bc1]-p.CQ.CE[t,yr]-p.DR[t])
  m.p_t["FQD", M_t == "CQD"] <- (1-p.CX[t,bc1])*rr.CD.FD*p.CF[t,bc1]
  m.p_t["CED", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.CQ.CE[t,yr])
  m.p_t["CQR", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "CQD"] <- p.CX[t,bc1]
  
  ##from FDQ state
  m.p_t["FQD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(1-p.FQ.FE[t,yr]-p.DR[t])
  m.p_t["FED", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.FQ.FE[t,yr])
  m.p_t["FQR", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.DR[t])
  m.p_t["X", M_t == "FQD"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]]
  
  ##from NRO state
  m.p_t["NOR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NO.NE[t,yr]-p.RD[t])
  m.p_t["COR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NER", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NO.NE[t,yr])
  m.p_t["NOD", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NOR"] <- p.NX[t,bc1]
  
  ##from CRO state
  m.p_t["COR", M_t == "COR"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CO.CE[t,yr]-rr.CR.CD*p.RD[t]-p.CO.FE[t,bc1])
  m.p_t["FOR", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["FER", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CO.FE[t,bc1])
  m.p_t["CER", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CO.CE[t,yr])
  m.p_t["COD", M_t == "COR"] <- (1-p.CX[t,bc1])*(rr.CR.CD*p.RD[t])
  m.p_t["X", M_t == "COR"] <- p.CX[t,bc1]
  
  ##from FRO state
  m.p_t["FOR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(1-p.FO.FE[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.FO.FE[t,yr])
  m.p_t["FOD", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.RD[t])
  m.p_t["X", M_t == "FOR"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]]
  
  ##from NRE state
  m.p_t["NER", M_t == "NER"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NE.NQ[t,yr]-p.RD[t])
  m.p_t["CER", M_t == "NER"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NQR", M_t == "NER"] <- (1-p.NX[t,bc1])*(p.NE.NQ[t,yr])
  m.p_t["NED", M_t == "NER"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NER"] <- p.NX[t,bc1]
  
  ##from CRE state
  m.p_t["CER", M_t == "CER"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CE.CQ[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "CER"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["CQR", M_t == "CER"] <- (1-p.CX[t,bc1])*(p.CE.CQ[t,yr])
  m.p_t["CED", M_t == "CER"] <- (1-p.CX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "CER"] <- p.CX[t,bc1]
  
  ##from FRE state
  m.p_t["FER", M_t == "FER"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FER"]])*(1-p.FE.FQ[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "FER"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FER"]])*(p.FE.FQ[t,yr])
  m.p_t["FED", M_t == "FER"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FER"]])*(p.RD[t])
  m.p_t["X", M_t == "FER"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FER"]]
  
  ##from NRQ state
  m.p_t["NQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NQ.NE[t,yr]-p.RD[t])
  m.p_t["CQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["NER", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQ.NE[t,yr])
  m.p_t["NQD", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NQR"] <- p.NX[t,bc1]
  
  ##from CRQ state
  m.p_t["CQR", M_t == "CQR"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CQ.CE[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "CQR"] <- (1-p.CX[t,bc1])*p.CF[t,bc1]
  m.p_t["CER", M_t == "CQR"] <- (1-p.CX[t,bc1])*p.CQ.CE[t,yr]
  m.p_t["CQD", M_t == "CQR"] <- (1-p.CX[t,bc1])*p.RD[t]
  m.p_t["X", M_t == "CQR"] <- p.CX[t,bc1]
  
  ##from FRQ state
  m.p_t["FQR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(1-p.FQ.FE[t,yr]-p.RD[t])
  m.p_t["FER", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.FQ.FE[t,yr])
  m.p_t["FQD", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.RD[t])
  m.p_t["X", M_t == "FQR"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]]
  
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
  
  m.u_t[M_t =="NOH"] <- m.u_t[M_t =="NEH"] <- m.u_t[M_t =="NQH"] <- u.NH[t] 
  m.u_t[M_t =="COH"] <- m.u_t[M_t =="CEH"] <- m.u_t[M_t =="CQH"] <- u.CH[t]
  m.u_t[M_t =="FOH"] <- m.u_t[M_t =="FEH"] <- m.u_t[M_t =="FQH"] <- u.FH[t]
  m.u_t[M_t =="NOD"] <- m.u_t[M_t =="NED"] <- m.u_t[M_t =="NQD"] <- u.ND[t]
  m.u_t[M_t =="COD"] <- m.u_t[M_t =="CED"] <- m.u_t[M_t =="CQD"] <- u.CD[t]
  m.u_t[M_t =="FOD"] <- m.u_t[M_t =="FED"] <- m.u_t[M_t =="FQD"] <- u.FD[t]
  m.u_t[M_t =="NOR"] <- m.u_t[M_t =="NER"] <- m.u_t[M_t =="NQR"] <- u.NR[t]
  m.u_t[M_t =="COR"] <- m.u_t[M_t =="CER"] <- m.u_t[M_t =="CQR"] <- u.CR[t]
  m.u_t[M_t =="FOR"] <- m.u_t[M_t =="FER"] <- m.u_t[M_t =="FQR"] <- u.FR[t]
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

# Write goodness-of-fit function to pass to calibration algorithm
f_gof <- function(v.params){
  
  l.model_prevs <- main_calib(v.params)[[2]]
  
  if( any(grepl("p.NO.NE", names(v.params))) | any(grepl("p.CO.CE", names(v.params))) | any(grepl("p.FO.FE", names(v.params))) ){
    print("E-cig calibration for ages 18-49 only (drop age 50+)")
    # Only keep prevalence values for ages 18-49 for e-cig calibration purposes
    l.model_prevs <- lapply(l.model_prevs, function(mat) {
      mat[mat[, "age"] < 50, ]
    })
    l.calib_targets <- lapply(l.calib_targets, function(mat) {
      mat[mat[, "age"] < 50, ]
    })
  }

  v.gof <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
  # Calibrate to N, C, F, D and ND/D, CD/D, FD/D prevalences

  for (r in 1:length(l.calib_targets)){ # use log likelihood as metric
    gof = gof_norm_loglike(target_mean = l.calib_targets[[r]][,"prev"],
                           model_output = subset(l.model_prevs[[r]],l.model_prevs[[r]][,"year"]>=calib_startyear & l.model_prevs[[r]][,"year"]<=calib_endyear)[,"prev"],
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
  for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
    bc1 = bc-1899
    for (age in 0:34){ # increase applies to youth and young adults ages 0-25
      if ((bc+age)>=yearinc_p.HD & age>=18 & age<=25){ # starting in 2016
        p.HD[(age+1),bc1] = s.HD_18.25*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=yearinc_p.HD & age<18){
        p.HD[(age+1),bc1] = s.HD_12.17*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=yearinc_p.HD & age>=26){
        p.HD[(age+1),bc1] = s.HD_26.34*p.HD[(age+1),bc1]
      }
    }
  }
  
  if (is.null(l.policy_effects)){
    #Initiation and Cessation for Healthy
        ## Initiation - No initiation after 25
        p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))
        ## Cessation - No cessation before 18
        p.CF = smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
    ## Initiation and Cessation for Depressed scaling factors - No initiation after 25 
        p.NC_D = smk_init*c(rep(s.NC_D_9.17,18),rep(s.NC_D_18.25,8),rep(s.NC_D_26.34,9),rep(0,65))
        ## Cessation - No cessation before 18
        
  } else {
    p.NC = unname(l.policy_effects[["m.initeff"]])*smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))
    p.CF = unname(l.policy_effects[["m.cesseff"]])*smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
    p.NC_D = unname(l.policy_effects[["m.initeff"]])*smk_init*c(rep(s.NC_D_9.17,18),rep(s.NC_D_18.25,8),rep(0,74))
    
  }
  p.CF[p.CF > 1] <- 1 # replace any cessation probabilities that are greater than 1 with 1
  
  # Vaping transition probabilities
  p.NO.NE[19:26,c("2020","2021")] <- p.NO.NE_20.21_18.25
  p.NO.NE[19:26,paste0(2022:calib_endyear)] <- p.NO.NE_22.23_18.25
  
  p.CO.CE[27:35,paste0(2022:calib_endyear)] <- p.CO.CE_22.23_26.34
  p.CO.CE[36:50,paste0(2022:calib_endyear)] <- p.CO.CE_22.23_35.49
  
  p.FO.FE[27:35,paste0(2022:calib_endyear)] <- p.FO.FE_22.23_26.34
  p.FO.FE[36:50,paste0(2022:calib_endyear)] <- p.FO.FE_22.23_35.49
  
  # Simulate for each birth cohort with parallelization: row = each person within birth cohort, columns = ages 0:99
  m.M <-foreach (i=cohorts, .combine='rbind', .packages='darthtools',
                 .export=c('mds_microsim','probs','get_prevs_combined',
                           'n.i','n.t','v.n','n.s','v.M_1',
                           'p.NC','p.CF','p.NC_D','p.NX','p.CX','a_p.FX.ysq',
                           'p.HD', 'p.DR', 'p.RD',
                           'rr.CH.CD','rr.CR.CD','rr.CD.FD',
                           'p.NO.NE', 'p.CO.CE', 'p.FO.FE',
                           'p.NE.NQ', 'p.CE.CQ', 'p.FE.FQ',
                           'p.NQ.NE', 'p.CQ.CE', 'p.FQ.FE')) %dopar% {
                             mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
                           }
  
  # To run in serial for debugging purposes, uncomment the line below, and comment out the 'foreach' loop above
  # m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Convert matrix from cohort-age to cohort-calendaryear (cy)
  m.M_cy <- cohortage_to_cohortyear(m.M)
  colnames(m.M_cy) <- c(min(cohorts):(max(cohorts)+100))
  
  # Output prevalence results as a list
  l.model_prevs <- lapply(c("N","C","F","D"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom=NULL, minyear=calib_startyear, maxyear=calib_endyear) # denominator is everyone still alive
  l.model_prevs_E <- lapply(c("E","NE","CE","FE"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom=NULL, minyear=2020, maxyear=calib_endyear) # e-cig data available from 2020 onwards
  l.model_prevs_D <- lapply(c("N.D","C.D","F.D"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom="D", minyear=calib_startyear, maxyear=calib_endyear) # denominator is everyone still alive
  l.model_prevs_D_E <- lapply(c("ED","NED","CED","FED"), get_prevs_combined, m.cohortbyyear=m.M_cy, denom="D", minyear=2020, maxyear=calib_endyear) # denominator is everyone still alive
  l.model_prevs <- c(l.model_prevs,l.model_prevs_E, l.model_prevs_D, l.model_prevs_D_E)
  names(l.model_prevs) <- c("N","C","F","D","E","NE","CE","FE","N_D","C_D","F_D","E_D","NE_D","CE_D","FE_D")
  
  l.model_prevs <- lapply(l.model_prevs, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc

  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer

  return(list(m.M,l.model_prevs,p.NC,p.CF,p.NC_D,rr.CD.FD))
  
}

## MAIN POLICY FUNCTIONS ------------------------------------------

apply_policy <- function(rr.init, rr.cess, policyyear, v.affected_ages) {
  m.initeff <- matrix(1, nrow = dim(smk_init)[1], ncol = dim(smk_init)[2])
  m.cesseff <- matrix(1, nrow = dim(smk_cess)[1], ncol = dim(smk_cess)[2])
  
  for (age in v.affected_ages) {
    m.initeff[row(m.initeff) + col(m.initeff) > (policyyear-1899) & row(m.initeff) == age] <- rr.init
    m.cesseff[row(m.cesseff) + col(m.cesseff) > (policyyear-1899) & row(m.cesseff) == age] <- rr.cess
  }
  
  l.policy_effects <- list(m.initeff = m.initeff, m.cesseff = m.cesseff)
  return(l.policy_effects)
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

main <- function(v.params, l.policy_effects=NULL) { # v.params: run model for parameter calibration; l.policy_effects: policy effects
  t_init <- Sys.time() 
  l.main_calib_outputs<-main_calib(v.params,l.policy_effects)
  m.M<-l.main_calib_outputs[[1]]
  l.model_prevs<-l.main_calib_outputs[[2]]
  #p.NC,p.CF,p.NC_D,rr.CD.FD. for outputs later
  p.NC<-l.main_calib_outputs[[3]]
  p.CF<-l.main_calib_outputs[[4]]
  p.NC_D<-l.main_calib_outputs[[5]]
  rr.CD.FD<-l.main_calib_outputs[[6]]

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
  
  # Get mortality counts by year
   n.X = apply(m.M,2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's
   n.X_D = apply(m.M_D,2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's
   n.X_notD = apply(m.M_notD,2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's
   
   n.X =  n.X [paste0(d.year:max(cohorts))]
   n.X_D = n.X_D[paste0(d.year:max(cohorts))]
   n.X_notD=n.X_notD[paste0(d.year:max(cohorts))]
   
  # Get total person life-years by year
  v.lifeyears = apply(m.M,2,function(x) sum(x!="X",na.rm=TRUE))
  v.lifeyears_D = apply(m.M_D,2,function(x) sum(x!="X",na.rm=TRUE))
  v.lifeyears_notD = apply(m.M_notD,2,function(x) sum(x!="X",na.rm=TRUE))
  
  v.lifeyears <- v.lifeyears[paste0(d.year:max(cohorts))]
  v.lifeyears_D <- v.lifeyears_D[paste0(d.year:max(cohorts))]
  v.lifeyears_notD <- v.lifeyears_notD[paste0(d.year:max(cohorts))]
  
  
  #Get mortality rate by year for each state.Check this with Jamie
  v.deathrate=n.X/v.lifeyears
  v.deathrate_D=n.X_D/v.lifeyears_D
  v.deathrate_notD=n.X_notD/v.lifeyears_notD
  
  
  
  # Output prevalence results as a list -------------------------------------
  
  # by age group
  l.model_prevs <- lapply(c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q"), get_prevs_combined, m.cohortbyyear=m.M, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs) <- c("N", "C","F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q")
  l.model_prevs <- lapply(l.model_prevs, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  l.model_prevs_D <- lapply(c("N.D", "C.D","F.D", "OD", "ED", "QD"), get_prevs_combined, m.cohortbyyear=m.M, denom="D", minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs_D) <- c("N","C","F","O","E","Q")
  l.model_prevs_D <- lapply(l.model_prevs_D, reorder_by_age) # Reorder age groups for both lists in a single lapply
  
  l.model_prevs_notD <- lapply(c("N","C","F", "O", "E", "Q"), get_prevs_combined, m.cohortbyyear=m.M_notD, denom=NULL, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  names(l.model_prevs_notD) <- c("N","C","F","O","E","Q")
  l.model_prevs_notD <- lapply(l.model_prevs_notD, reorder_by_age) # Reorder age groups for both lists in a single lapply

  # by single year for general population
  l.model_prevs1 <- lapply(c("D","N","C","F"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M, minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1 <- c(l.model_prevs1, lapply(c(1:40), get_Fprevs1, 
                                     m.F_cy=m.F, minyear=calib_startyear, 
                                     maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1) <- c("D","N","C","F",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1 <- lapply(l.model_prevs1, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  # by single year for depressed population
  l.model_prevs1_D <- lapply(c("N.D","C.D","F.D"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M, denom="D", minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1_D <- c(l.model_prevs1_D, lapply(c(1:40), get_Fprevs1, 
                                     m.F_cy=m.F_D, minyear=calib_startyear, 
                                     maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1_D) <- c("N","C","F",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1_D <- lapply(l.model_prevs1_D, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  # by single year for NOT depressed population
  l.model_prevs1_notD <- lapply(c("N","C","F"), get_prevs_combined, age_single_yr = TRUE, m.cohortbyyear=m.M_notD, minyear=calib_startyear, maxyear=max(cohorts)) # get prevs by single year of age
  l.model_prevs1_notD <- c(l.model_prevs1_notD, lapply(c(1:40), get_Fprevs1, 
                                                 m.F_cy=m.F_notD, minyear=calib_startyear, 
                                                 maxyear=max(cohorts))) # get prevs by single year of age
  names(l.model_prevs1_notD) <- c("N","C","F",paste0("q",str_pad(1:40,2,pad="0"))) # track former prevalence by years since quitting
  l.model_prevs1_notD <- lapply(l.model_prevs1_notD, reorder_by_age) # re-order the age groups from 18.25, 18.99, 26.34, etc
  
  # Calculate smoking-attributable mortality from d.year to 2100 --------
  v.year_range <- d.year:max(cohorts) # Specify years of interest
  v.age_range <- 0:99
  
  m.prev_N <- extract_prevalence(l.model_prevs1, "N", v.age_range, v.year_range)
  m.prev_C <- extract_prevalence(l.model_prevs1, "C", v.age_range, v.year_range) # Extract prevalence data for current smokers and former smokers
  m.prev_F <- extract_prevalence(l.model_prevs1, "F", v.age_range, v.year_range) # Average FX, not by years since quit
  m.prev_D <- extract_prevalence(l.model_prevs1, "D", v.age_range, v.year_range) 
  m.prev_N_D <- extract_prevalence(l.model_prevs1_D, "N", v.age_range, v.year_range)
  m.prev_C_D <- extract_prevalence(l.model_prevs1_D, "C", v.age_range, v.year_range)
  m.prev_F_D <- extract_prevalence(l.model_prevs1_D, "F", v.age_range, v.year_range)
  m.prev_N_notD <- extract_prevalence(l.model_prevs1_notD, "N", v.age_range, v.year_range)
  m.prev_C_notD <- extract_prevalence(l.model_prevs1_notD, "C", v.age_range, v.year_range)
  m.prev_F_notD <- extract_prevalence(l.model_prevs1_notD, "F", v.age_range, v.year_range)
  
  # Extract prevalence data for former smoker years since quit
  l.prev_fs.ysq <- lapply(paste0("q", sprintf("%02d", 1:40)), function(state) extract_prevalence(l.model_prevs1, state, v.age_range, v.year_range))
  
  # Calculate smoking-attributable deaths for current smokers
  m.SADcs <- pop[, as.character(v.year_range)] * (m.prev_C * (p.CX_cy[v.age_range + 1, v.year_range - 1899] - p.NX_cy[v.age_range + 1, v.year_range - 1899]))
  m.SADcs_D <- pop[, as.character(v.year_range)] * m.prev_D *
    (m.prev_C_D * (p.CX_cy[v.age_range + 1, v.year_range - 1899] - p.NX_cy[v.age_range + 1, v.year_range - 1899]))
  
  v.SADcs <- colSums(m.SADcs)
  v.SADcs_D <- colSums(m.SADcs_D)

  # Calculate smoking-attributable deaths for former smokers
  m.SADfs.ysq <- pop[, as.character(v.year_range)] * m.prev_F *
                           Reduce(`+`, lapply(1:40, function(i) l.prev_fs.ysq[[i]] * 
                                                (a_p.FX.ysq_cy[, , i][v.age_range + 1, v.year_range - 1899] - p.NX_cy[v.age_range + 1, v.year_range - 1899])))
  m.SADfs.ysq_D <- pop[, as.character(v.year_range)] * m.prev_D * m.prev_F_D *
    Reduce(`+`, lapply(1:40, function(i) l.prev_fs.ysq[[i]] * 
                         (a_p.FX.ysq_cy[, , i][v.age_range + 1, v.year_range - 1899] - p.NX_cy[v.age_range + 1, v.year_range - 1899])))
  v.SADfs.ysq <- colSums(m.SADfs.ysq)
  v.SADfs.ysq_D <- colSums(m.SADfs.ysq_D)
  
  # Total smoking-attributable deaths
  v.SAD <- v.SADfs.ysq + v.SADcs # total pop
  v.SAD_D <- v.SADfs.ysq_D + v.SADcs_D # depressed pop
  v.SAD_notD <- v.SAD - v.SAD_D # not depressed pop
  
  # Calculate years of life lost - multiply each SAD by the remaining life expectancy of someone at that age who had never smoked
  v.yll = colSums(le_N[,as.character(v.year_range)]*m.SADcs) + colSums(le_N[,as.character(v.year_range)]*m.SADfs.ysq)
  v.yll_D = colSums(le_N[,as.character(v.year_range)]*m.SADcs_D)+ colSums(le_N[,as.character(v.year_range)]*m.SADfs.ysq_D)
  v.yll_notD <- v.yll - v.yll_D

  # Economic outcomes -------------------------------------------------------
  
  # Calculate costs, utilities, and productivity for each health state by age in the population
  m.total_cuw <- rbind(colSums(m.C,na.rm=TRUE),colSums(m.U,na.rm=TRUE),colSums(m.W,na.rm=TRUE),colSums(m.B,na.rm=TRUE),
                     colSums(m.C,na.rm=TRUE)+colSums(m.B,na.rm=TRUE)-colSums(m.W,na.rm=TRUE),v.lifeyears,v.yll) # societal costs = medical costs + consumer expenditures (non-medical) -  productivity 
  
  m.total_cuw_D <- rbind(colSums(m.C_D,na.rm=TRUE),colSums(m.U_D,na.rm=TRUE),colSums(m.W_D,na.rm=TRUE),colSums(m.B_D,na.rm=TRUE),
                     colSums(m.C_D,na.rm=TRUE)+colSums(m.B_D,na.rm=TRUE)-colSums(m.W_D,na.rm=TRUE),v.lifeyears_D,v.yll_D) # societal costs = medical costs + consumer expenditures (non-medical) -  productivity 
  
  m.total_cuw_notD <- m.total_cuw - m.total_cuw_D
  rownames(m.total_cuw) <- rownames(m.total_cuw_D) <- rownames(m.total_cuw_notD) <- c("med_costs","QALYs","productivity","consumer_exp","soc_costs","lifeyears","YLL")
  colnames(m.total_cuw) <- colnames(m.C) <- colnames(m.U) <- colnames(m.W) <- colnames(m.B) <- colnames(m.C_D) <- colnames(m.U_D) <- colnames(m.W_D) <- colnames(m.B_D) <- c(min(cohorts):(max(cohorts)+100))
  
  # Assume 2023 is the starting year for discounting purposes, calculate discount weight based on the discount rate d.c
  v.d = c(rep(1,d.year-1900), c(1 / (1 + d.c) ^ (0:(ncol(m.C)-(d.year-1899))))) # vector of discount weights
  
  # Total discounted costs, QALYs, and productivity
  m.d_total_cuw <- m.total_cuw %*% diag(v.d) # multiple each row of the cuw matrix by the discounting vector
  #we wouldn't want dicsounting vector for qaly or life years right?
  m.d_total_cuw_D <- m.total_cuw_D %*% diag(v.d) # depressed pop
  m.d_total_cuw_notD <- m.total_cuw_notD %*% diag(v.d) # not depressed pop
  colnames(m.d_total_cuw) <- colnames(m.d_total_cuw_D)  <- colnames(m.d_total_cuw_notD) <- min(cohorts):(max(cohorts)+100)
  m.cuw <- t(m.d_total_cuw[,as.character(v.year_range)])
  m.cuw_D <- t(m.d_total_cuw_D[,as.character(v.year_range)])
  m.cuw_notD <- t(m.d_total_cuw_notD[,as.character(v.year_range)])
  
  # Get average discounted costs and effects to calculate cost-effectiveness ratio
  v.cea = c(avg_med_costs = mean(m.d_total_cuw["med_costs",as.character(v.year_range)]),
            avg_cons_exp = mean(m.d_total_cuw["consumer_exp",as.character(v.year_range)]),
            avg_prod = mean(m.d_total_cuw["productivity",as.character(v.year_range)]),
            avg_soc_costs = mean(m.d_total_cuw["soc_costs",as.character(v.year_range)]),            
            avg_QALYs = mean(m.d_total_cuw["QALYs",as.character(v.year_range)]),
            avg_LYs= mean(m.d_total_cuw["lifeyears",as.character(v.year_range)]))
  
  v.cea_D = c(avg_med_costs = mean(m.d_total_cuw_D["med_costs",as.character(v.year_range)]),
            avg_cons_exp = mean(m.d_total_cuw_D["consumer_exp",as.character(v.year_range)]),
            avg_prod = mean(m.d_total_cuw_D["productivity",as.character(v.year_range)]),
            avg_soc_costs = mean(m.d_total_cuw_D["soc_costs",as.character(v.year_range)]),            
            avg_QALYs = mean(m.d_total_cuw_D["QALYs",as.character(v.year_range)]),
            avg_LYs= mean(m.d_total_cuw_D["lifeyears",as.character(v.year_range)]))
  
  v.cea_notD = c(avg_med_costs = mean(m.d_total_cuw_notD["med_costs",as.character(v.year_range)]),
              avg_cons_exp = mean(m.d_total_cuw_notD["consumer_exp",as.character(v.year_range)]),
              avg_prod = mean(m.d_total_cuw_notD["productivity",as.character(v.year_range)]),
              avg_soc_costs = mean(m.d_total_cuw_notD["soc_costs",as.character(v.year_range)]),            
              avg_QALYs = mean(m.d_total_cuw_notD["QALYs",as.character(v.year_range)]),
              avg_LYs= mean(m.d_total_cuw_notD["lifeyears",as.character(v.year_range)]))
  
  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer

  # Output results as two lists: one for general population, and one for depressed population
  l.results <- list(l.model_prevs = l.model_prevs, m.cuw=m.cuw, v.lifeyears=v.lifeyears, v.SAD=v.SAD, v.yll = v.yll,v.cea=v.cea,
                        init = p.NC, cess = p.CF, m.prev_C=m.prev_C, m.prev_F = m.prev_F, m.prev_N=m.prev_N,  v.deathrate= v.deathrate)
    
  l.results_D <- list(l.model_prevs = l.model_prevs_D, m.cuw=m.cuw_D, v.lifeyears=v.lifeyears_D, v.SAD=v.SAD_D,  v.yll = v.yll_D, v.cea=v.cea_D,
                        init = p.NC_D, cess = rr.CD.FD*p.CF, m.prev_C=m.prev_C_D, m.prev_F = m.prev_F_D, m.prev_N=m.prev_N_D, v.deathrate_D= v.deathrate_D)
  
  l.results_notD <- list(l.model_prevs=l.model_prevs_notD, m.cuw=m.cuw_notD , v.lifeyears = v.lifeyears_notD, v.SAD=v.SAD_notD, v.yll = v.yll_notD, v.cea=v.cea_notD,
                         init = p.NC, cess = p.CF, m.prev_C=m.prev_C_notD, m.prev_F = m.prev_F_notD, m.prev_N=m.prev_N_notD, v.deathrate_notD= v.deathrate_notD)
  
  return(list(l.results=l.results, l.results_D = l.results_D, l.results_notD=l.results_notD))
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
