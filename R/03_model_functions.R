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
    v.ysq <- ifelse(grepl("F", m.M[, t]), v.ysq + 1, 0)
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
  m.p_t["NOH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NO.CV[t,yr]-p.NO.NV[t,yr]-p.HD[t,bc1])
  m.p_t["COH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["CVH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NO.CV[t,yr])
  m.p_t["NVH", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.NO.NV[t,yr])
  m.p_t["NOD", M_t == "NOH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "NOH"] <- p.NX[t,bc1]
  
  #from CHO state
  m.p_t["COH", M_t == "COH"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CO.FV[t,yr]-p.CO.CV[t,yr]-p.HD[t,bc1])
  m.p_t["FOH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["FVH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CO.FV[t,yr])
  m.p_t["CVH", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.CO.CV[t,yr])
  m.p_t["COD", M_t == "COH"] <- (1-p.CX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "COH"] <- p.CX[t,bc1]
  
  ##from FHO state
  m.p_t["FOH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(1-p.FO.CO[t,yr]-p.FO.FV[t,yr]-p.FO.CV[t,yr]-p.HD[t,bc1])
  m.p_t["COH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.FO.CO[t,yr])
  m.p_t["FVH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.FO.FV[t,yr])
  m.p_t["CVH", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.FO.CV[t,yr])
  m.p_t["FOD", M_t == "FOH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FOH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOH"]]
  
  ##from NHV state
  m.p_t["NVH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(1-p.NV.CV[t,yr]-p.NV.NQ[t,yr]-p.NV.CQ[t,yr]-p.HD[t, bc1])
  m.p_t["CVH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NV.CV[t,yr])
  m.p_t["NQH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NV.NQ[t,yr])
  m.p_t["CQH", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.NV.CQ[t,yr])
  m.p_t["NVD", M_t == "NVH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X" , M_t == "NVH"] <- p.NX[t,bc1]
  
  ##from CHV state
  m.p_t["CVH", M_t == "CVH"] <- (1-p.CX[t,bc1])*(1-p.CV.CQ[t,yr]-p.CV.FV[t,yr]-p.CV.FQ[t,yr]-p.HD[t,bc1])
  m.p_t["FVH", M_t == "CVH"] <- (1-p.CX[t,bc1])*(p.CV.FV[t,yr])
  m.p_t["FQH", M_t == "CVH"] <- (1-p.CX[t,bc1])*(p.CV.FQ[t,yr])
  m.p_t["CQH", M_t == "CVH"] <- (1-p.CX[t,bc1])*(p.CV.CQ[t,yr])
  m.p_t["CVD", M_t == "CVH"] <- (1-p.CX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "CVH"] <- p.CX[t,bc1]
  
  ##from FHV state
  m.p_t["FVH", M_t == "FVH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]])*(1-p.FV.FQ[t,yr]-p.FV.CQ[t,yr]-p.FV.CV[t,yr]-p.HD[t,bc1])
  m.p_t["FQH", M_t == "FVH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]])*(p.FV.FQ[t,yr])
  m.p_t["CQH", M_t == "FVH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]])*(p.FV.CQ[t,yr])
  m.p_t["CVH", M_t == "FVH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]])*(p.FV.CV[t,yr])
  m.p_t["FVD", M_t == "FVH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FVH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVH"]]
  
  ##from NHQ state
  m.p_t["NQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(1-p.NQ.CQ[t,yr]-p.NQ.CV[t,yr]-p.NQ.NV[t,yr]-p.HD[t,bc1])
  m.p_t["CQH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQ.CQ[t,yr])
  m.p_t["CVH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQ.CV[t,yr])
  m.p_t["NVH", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.NQ.NV[t,yr])
  m.p_t["NQD", M_t == "NQH"] <- (1-p.NX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "NQH"] <- p.NX[t,bc1]
  
  ##from CHQ state
  m.p_t["CQH", M_t == "CQH"] <- (1-p.CX[t,bc1])*(1-p.CQ.FQ[t,yr]-p.CQ.FV[t,yr]-p.CQ.CV[t,yr]-p.HD[t,bc1])
  m.p_t["FQH", M_t == "CQH"] <- (1-p.CX[t,bc1])*(p.CQ.FQ[t,yr])
  m.p_t["FVH", M_t == "CQH"] <- (1-p.CX[t,bc1])*(p.CQ.FV[t,yr])
  m.p_t["CVH", M_t == "CQH"] <- (1-p.CX[t,bc1])*(p.CQ.CV[t,yr])
  m.p_t["CQD", M_t == "CQH"] <- (1-p.CX[t,bc1])*(p.HD[t,bc1])
  m.p_t["X", M_t == "CQH"] <- p.CX[t,bc1]
  
  ##from FHQ state
  m.p_t["FQH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(1-p.FQ.FV[t,yr]-p.FQ.CV[t,yr]-p.FQ.CQ[t,yr]-p.HD[t,bc1])
  m.p_t["FVH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.FQ.FV[t,yr])
  m.p_t["CVH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.FQ.CV[t,yr])
  m.p_t["CQH", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.FQ.CQ[t,yr])
  m.p_t["FQD", M_t == "FQH"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]])*(p.HD[t,bc1])
  m.p_t["X", M_t == "FQH"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQH"]]
  
  ##from NDO state
  m.p_t["NOD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NO.CV[t,yr]-p.NO.NV[t,yr]-p.DR[t])
  m.p_t["COD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["CVD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NO.CV[t,yr])
  m.p_t["NVD", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.NO.NV[t,yr])
  m.p_t["NOR", M_t == "NOD"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NOD"] <- p.NX[t,bc1]
  
  ##from CDO state
  m.p_t["COD", M_t == "COD"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CO.FV[t,yr]-p.CO.CV[t,yr]-p.DR[t])
  m.p_t["FOD", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["FVD", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.CO.FV[t,yr])
  m.p_t["CVD", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.CO.CV[t,yr])
  m.p_t["COR", M_t == "COD"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "COD"] <- p.CX[t,bc1]
  
  ##from FDO state
  m.p_t["FOD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(1-p.FO.FV[t,yr]-p.FO.CV[t,yr]-p.FO.CO[t,yr]-p.DR[t])
  m.p_t["FVD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.FO.FV[t,yr])
  m.p_t["CVD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.FO.CV[t,yr])
  m.p_t["COD", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.FO.CO[t,yr])
  m.p_t["FOR", M_t == "FOD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]])*(p.DR[t])
  m.p_t["X", M_t == "FOD"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOD"]]
  
  ##from NDV state
  m.p_t["NVD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(1-p.NV.CV[t,yr]-p.NV.NQ[t,yr]-p.NV.CQ[t,yr]-p.DR[t])
  m.p_t["CVD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NV.CV[t,yr])
  m.p_t["CQD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NV.CQ[t,yr])
  m.p_t["NQD", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.NV.NQ[t,yr])
  m.p_t["NVR", M_t == "NVD"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NVD"] <- p.NX[t,bc1]
  
  ##from CDV state
  m.p_t["CVD", M_t == "CVD"] <- (1-p.CX[t,bc1])*(1-p.CV.FV[t,yr]-p.CV.FQ[t,yr]-p.CV.CQ[t,yr]-p.DR[t])
  m.p_t["FVD", M_t == "CVD"] <- (1-p.CX[t,bc1])*(p.CV.FV[t,yr])
  m.p_t["FQD", M_t == "CVD"] <- (1-p.CX[t,bc1])*(p.CV.FQ[t,yr])
  m.p_t["CQD", M_t == "CVD"] <- (1-p.CX[t,bc1])*(p.CV.CQ[t,yr])
  m.p_t["CVR", M_t == "CVD"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "CVD"] <- p.CX[t,bc1]
  
  ##from FDV state
  m.p_t["FVD", M_t =="FVD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]])*(1-p.FV.FQ[t,yr]-p.FV.CQ[t,yr]-p.FV.CV[t,yr]-p.DR[t])
  m.p_t["FQD", M_t =="FVD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]])*(p.FV.FQ[t,yr])
  m.p_t["CQD", M_t =="FVD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]])*(p.FV.CQ[t,yr])
  m.p_t["CVD", M_t =="FVD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]])*(p.FV.CV[t,yr])
  m.p_t["FVR", M_t =="FVD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]])*(p.DR[t])
  m.p_t["X", M_t == "FVD"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVD"]]
  
  ##from NDQ state
  m.p_t["NQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(1-p.NQ.NV[t,yr]-p.NQ.CV[t,yr]-p.NQ.CQ[t,yr]-p.DR[t])
  m.p_t["NVD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQ.NV[t,yr])
  m.p_t["CVD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQ.CV[t,yr])
  m.p_t["CQD", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.NQ.CQ[t,yr])
  m.p_t["NQR", M_t == "NQD"] <- (1-p.NX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "NQD"] <- p.NX[t,bc1]
  
  ##from CDQ state
  m.p_t["CQD", M_t == "CQD"] <- (1-p.CX[t,bc1])*(1-p.CQ.FQ[t,yr]-p.CQ.FV[t,yr]-p.CQ.CV[t,yr]-p.DR[t])
  m.p_t["FQD", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.CQ.FQ[t,yr])
  m.p_t["FVD", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.CQ.FV[t,yr])
  m.p_t["CVD", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.CQ.CV[t,yr])
  m.p_t["CQR", M_t == "CQD"] <- (1-p.CX[t,bc1])*(p.DR[t])
  m.p_t["X", M_t == "CQD"] <- p.CX[t,bc1]
  
  ##from FDQ state
  m.p_t["FQD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(1-p.FQ.FV[t,yr]-p.FQ.CV[t,yr]-p.FQ.CQ[t,yr]-p.DR[t])
  m.p_t["FVD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.FQ.FV[t,yr])
  m.p_t["CVD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.FQ.CV[t,yr])
  m.p_t["CQD", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.FQ.CQ[t,yr])
  m.p_t["FQR", M_t == "FQD"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]])*(p.DR[t])
  m.p_t["X", M_t == "FQD"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQD"]]
  
  ##from NRO state
  m.p_t["NOR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(1-p.NC[t,bc1]-p.NO.CV[t,yr]-p.NO.NV[t,yr]-p.RD[t])
  m.p_t["COR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NC[t,bc1])
  m.p_t["CVR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NO.CV[t,yr])
  m.p_t["NVR", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.NO.NV[t,yr])
  m.p_t["NOD", M_t == "NOR"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NOR"] <- p.NX[t,bc1]
  
  ##from CRO state
  m.p_t["COR", M_t == "COR"] <- (1-p.CX[t,bc1])*(1-p.CF[t,bc1]-p.CO.FV[t,yr]-p.CO.CV[t,yr]-p.RD[t])
  m.p_t["FOR", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CF[t,bc1])
  m.p_t["FVR", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CO.FV[t,yr])
  m.p_t["CVR", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.CO.CV[t,yr])
  m.p_t["COD", M_t == "COR"] <- (1-p.CX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "COR"] <- p.CX[t,bc1]
  
  ##from FRO state
  m.p_t["FOR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(1-p.FO.FV[t,yr]-p.FO.CV[t,yr]-p.FO.CO[t,yr]-p.RD[t])
  m.p_t["FVR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.FO.FV[t,yr])
  m.p_t["CVR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.FO.CV[t,yr])
  m.p_t["COR", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.FO.CO[t,yr])
  m.p_t["FOD", M_t == "FOR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]])*(p.RD[t])
  m.p_t["X", M_t == "FOR"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FOR"]]
  
  ##from NRV state
  m.p_t["NVR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(1-p.NV.CV[t,yr]-p.NV.CQ[t,yr]-p.NV.NQ[t,yr]-p.RD[t])
  m.p_t["CVR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NV.CV[t,yr])
  m.p_t["CQR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NV.CQ[t,yr])
  m.p_t["NQR", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.NV.NQ[t,yr])
  m.p_t["NVD", M_t == "NVR"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NVR"] <- p.NX[t,bc1]
  
  ##from CRV state
  m.p_t["CVR", M_t == "CVR"] <- (1-p.CX[t,bc1])*(1-p.CV.FV[t,yr]-p.CV.FQ[t,yr]-p.CV.CQ[t,yr]-p.RD[t])
  m.p_t["FVR", M_t == "CVR"] <- (1-p.CX[t,bc1])*(p.CV.FV[t,yr])
  m.p_t["FQR", M_t == "CVR"] <- (1-p.CX[t,bc1])*(p.CV.FQ[t,yr])
  m.p_t["CQR", M_t == "CVR"] <- (1-p.CX[t,bc1])*(p.CV.CQ[t,yr])
  m.p_t["CVD", M_t == "CVR"] <- (1-p.CX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "CVR"] <- p.CX[t,bc1]
  
  ##from FRV state
  m.p_t["FVR", M_t == "FVR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]])*(1-p.FV.FQ[t,yr]-p.FV.CQ[t,yr]-p.FV.CV[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "FVR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]])*(p.FV.FQ[t,yr])
  m.p_t["CQR", M_t == "FVR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]])*(p.FV.CQ[t,yr])
  m.p_t["CVR", M_t == "FVR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]])*(p.FV.CV[t,yr])
  m.p_t["FVD", M_t == "FVR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]])*(p.RD[t])
  m.p_t["X", M_t == "FVR"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FVR"]]
  
  ##from NRQ state
  m.p_t["NQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(1-p.NQ.CQ[t,yr]-p.NQ.CV[t,yr]-p.NQ.NV[t,yr]-p.RD[t])
  m.p_t["CQR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQ.CQ[t,yr])
  m.p_t["CVR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQ.CV[t,yr])
  m.p_t["NVR", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.NQ.NV[t,yr])
  m.p_t["NQD", M_t == "NQR"] <- (1-p.NX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "NQR"] <- p.NX[t,bc1]
  
  ##from CRQ state
  m.p_t["CQR", M_t == "CQR"] <- (1-p.CX[t,bc1])*(1-p.CQ.FQ[t,yr]-p.CQ.FV[t,yr]-p.CQ.CV[t,yr]-p.RD[t])
  m.p_t["FQR", M_t == "CQR"] <- (1-p.CX[t,bc1])*(p.CQ.FQ[t,yr])
  m.p_t["FVR", M_t == "CQR"] <- (1-p.CX[t,bc1])*(p.CQ.FV[t,yr])
  m.p_t["CVR", M_t == "CQR"] <- (1-p.CX[t,bc1])*(p.CQ.CV[t,yr])
  m.p_t["CQD", M_t == "CQR"] <- (1-p.CX[t,bc1])*(p.RD[t])
  m.p_t["X", M_t == "CQR"] <- p.CX[t,bc1]
  
  ##from FRQ state
  m.p_t["FQR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(1-p.FQ.FV[t,yr]-p.FQ.CV[t,yr]-p.FQ.CQ[t,yr]-p.RD[t])
  m.p_t["FVR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.FQ.FV[t,yr])
  m.p_t["CVR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.FQ.CV[t,yr])
  m.p_t["CQR", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.FQ.CQ[t,yr])
  m.p_t["FQD", M_t == "FQR"] <- (1-a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]])*(p.RD[t])
  m.p_t["X", M_t == "FQR"] <- a_p.FX.ysq[t,bc1,v.ysq[M_t == "FQR"]]
  
  
  m.p_t["X" , M_t == "X"] <-  1		
  
 
  # print birth cohort and age for debugging problematic transition probabilities
  
  # check_transition_probability(m.p_t,verbose=FALSE)
  # if(any(colSums(m.p_t))!=1){
  #   print(m.p_t)
  #   #print(paste0("bc: ", bc, ", age: ",t, " year: ", yr, " M_t: ", M_t))
  # }
  # print(m.p_t)
  # print(paste0("bc: ", bc, ", age: ",t, " year: ", yr, " M_t: ", M_t))
  # print(colSums(m.p_t))
  # check_sum_of_transition_array(t(m.p_t), n_rows=n.i, n_cycles= n.t, verbose = TRUE)
  
  return(t(m.p_t)) 
}       

## COSTS FUNCTION ----------------------------------------------------
costs <- function(M_t,t) { # gets the costs for each person based on health state M and age t
    
    m.c_t <- matrix(data = 0, nrow = n.i, ncol = 1)  
  
    m.c_t[M_t =="NH"] <- c.NH[t] 
    m.c_t[M_t =="CH"] <- c.CH[t]
    m.c_t[M_t =="FH"] <- c.FH[t]
    m.c_t[M_t =="ND"] <- c.ND[t]
    m.c_t[M_t =="CD"] <- c.CD[t]
    m.c_t[M_t =="FD"] <- c.FD[t]
    m.c_t[M_t =="NR"] <- c.NR[t]
    m.c_t[M_t =="CR"] <- c.CR[t]
    m.c_t[M_t =="FR"] <- c.FR[t]
    m.c_t[M_t =="X"] <- 0
    
  return(m.c_t) 
}      

## UTILITIES FUNCTION ----------------------------------------------------
utils <- function(M_t,t) { # gets the utilities for each person based on health state M and age t
  
  m.u_t <- matrix(data= NA, nrow = n.i, ncol = 1)  
  
  m.u_t[M_t =="NH"] <- u.NH[t] 
  m.u_t[M_t =="CH"] <- u.CH[t]
  m.u_t[M_t =="FH"] <- u.FH[t]
  m.u_t[M_t =="ND"] <- u.ND[t]
  m.u_t[M_t =="CD"] <- u.CD[t]
  m.u_t[M_t =="FD"] <- u.FD[t]
  m.u_t[M_t =="NR"] <- u.NR[t]
  m.u_t[M_t =="CR"] <- u.CR[t]
  m.u_t[M_t =="FR"] <- u.FR[t]
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
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
      m.m_prevs<-rbind(m.m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.m_prevs) 
}

get_prevs1 <- function(state,m_cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18:99)
  agegroupstart <- c(18:99)
  agegroupend <- c(18:99)
  m.m_prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m_cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
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
      alive <- sum(str_count(select,denom),na.rm=TRUE)
      counts <- sum(str_count(select,state),na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      prev <- counts/alive
      m.m_prevs<-rbind(m.m_prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.m_prevs)<-c("age","year", "prev","counts","alive","dead")
  return(m.m_prevs) 
}


## CALIBRATION FUNCTIONS ---------------------------------------------------

# Write goodness-of-fit function to pass to calibration algorithm
f_gof <- function(v.params){
  
  model_res <- main_calib(v.params)   # Run model for parameter set "v.params"
  v.GOF <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
  
  # Calibrate to N, C, F, D and ND/D, CD/D, FD/D prevalences
  for (r in 1:length(lst_calibtargets)){ # sum of squared differences
  # for (r in 1:4){
    gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
    v.GOF[r] <-gof
  }
  
  # OVERALL
  v.weights <- c(1,1,1,3,1,1,1) # can assign targets different weights
  # weighted sum
  GOF_overall <- sum(v.GOF[1:n.target] * v.weights)
  cat(GOF_overall)
  # return GOF
  return(GOF_overall)
}

## Function to handle the repetitive task of looking up calib_inputs by parameter name
get_value <- function(param_name) {
  ifelse(calib_inputs[param_name, "calib"] == 1, v.params[param_name], calib_inputs[param_name, "value"])
}

main_calib = function(v.params) { # v.params: run model for parameter calibration; no policy effects
  
  t_init <- Sys.time() # Start timer
  
  # Loop over parameter names and assign values dynamically
  for (param in rownames(calib_inputs)) {
    assign(param, get_value(param))
  }
  
  yearinc_p.HD <- round(yearinc_p.HD)
  
  # Recovery
  p.DR=NULL
  p.DR[1:12] <- p.DR[100] <- 0 # final value = 0 because mortality prob = 1
  p.DR[13:18] <- p.DR_12.17
  p.DR[19:26] <- p.DR_18.25
  p.DR[27:35] <- p.DR_26.34
  p.DR[36:50] <- p.DR_35.49
  p.DR[51:65] <- p.DR_50_64
  p.DR[66:99] <- p.DR_65_99
  
  # Recurrence
  p.RD = NULL
  p.RD[1:12] <- p.RD[100] <- 0 # final value = 0 because mortality prob = 1
  p.RD[13:18] <- p.RD_12.17
  p.RD[19:26] <- p.RD_18.25
  p.RD[27:35] <- p.RD_26.34
  p.RD[36:50] <- p.RD_35.49
  p.RD[51:65] <- p.RD_50_64
  p.RD[66:99] <- p.RD_65_99
  
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
           'rr.ND.CD','rr.CH.CD','rr.CR.CD','rr.CD.FD',
           'p.NO.NO', 'p.NO.CO', 'p.NO.CV', 'p.NO.NV', 
           'p.NV.NV', 'p.NV.CQ', 'p.NV.CV', 'p.NV.NQ', 
           'p.FV.CQ', 'p.FV.FV', 'p.FV.CV', 'p.FV.FQ',
           'p.NQ.CQ', 'p.NQ.CV', 'p.NQ.NQ', 'p.NQ.NV', 
           'p.FO.CO', 'p.FO.CV', 'p.FO.FO', 'p.FO.FV',
           'p.FQ.CQ', 'p.FQ.CV', 'p.FQ.FQ', 'p.FQ.FV', 
           'p.CO.CO', 'p.CO.CV', 'p.CO.FO', 'p.CO.FV',
           'p.CQ.CQ', 'p.CQ.CV', 'p.CQ.FQ', 'p.CQ.FV',
           'p.CV.CQ', 'p.CV.CV', 'p.CV.FQ', 'p.CV.FV')) %dopar% {
             mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
 }
  # run in serial for debugging:
 #m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Convert matrix from cohort-age to cohort-calendaryear (cy)
  m.M_cy <- matrix(nrow = n.i*length(cohorts), ncol = (length(cohorts)+100))
  for (b in 1:length(cohorts)){
    m.M_cy[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.M[(n.i*(b-1)+1):(n.i*b),]
  }
  colnames(m.M_cy) <- c(min(cohorts):(max(cohorts)+100))
  
  # Output prevalence results as a list
  model_res <- lapply(c("N", "C","F", "D", "NO", "CO", "FO", "O", "NV", "CV", "FV", "V", "NQ", "CQ", "FQ", "Q"), get_prevs, m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  model_res <- c(model_res, lapply(c("N.D","C.D","F.D"), get_subgroup_prevs, denom="D",m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts))) # denominator is everyone in "D" subpopulation
  model_res <- c(model_res, lapply(c("OD","VD","QD"), get_subgroup_prevs, denom="D",m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts)))
  
  names(model_res) <- c("N", "C","F", "D", "NO", "CO", "FO", "O", "NV", "CV", "FV", "V", "NQ", "CQ", "FQ", "Q", "ND","CD","FD","OD","VD","QD")
  for (l in 1:length(model_res)){
    model_res[[l]] <- model_res[[l]][order(model_res[[l]][,"age"],decreasing=FALSE),] # re-order the age groups from 18.25, 18.99, 26.34, etc
  }
  
  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer
  return(model_res) # For calibration only
}

## MAIN POLICY FUNCTIONS ------------------------------------------

apply_policy <- function(rr.init, rr.cess, policyyear, v.affected_ages) {
  initeff <- matrix(1, nrow = dim(smk_init)[1], ncol = dim(smk_init)[2])
  cesseff <- matrix(1, nrow = dim(smk_cess)[1], ncol = dim(smk_cess)[2])
  
  for (age in v.affected_ages) {
    initeff[row(initeff) + col(initeff) > (policyyear-1899) & row(initeff) == age] <- rr.init
    cesseff[row(cesseff) + col(cesseff) > (policyyear-1899) & row(cesseff) == age] <- rr.cess
  }
  v.policy <- list(initeff = initeff, cesseff = cesseff)
  return(v.policy)
}
main = function(v.params, v.policy) { # v.params: run model for parameter calibration; v.policy: policy effects
  
  t_init <- Sys.time() # Start timer
  
  # Loop over parameter names and assign values dynamically
  for (param in rownames(calib_inputs)) {
    assign(param, get_value(param))
  }
  
  yearinc_p.HD <- round(yearinc_p.HD)
  
  # Recovery
  p.DR=NULL
  p.DR[1:12] <- p.DR[100] <- 0 # final value = 0 because mortality prob = 1
  p.DR[13:18] <- p.DR_12.17
  p.DR[19:26] <- p.DR_18.25
  p.DR[27:35] <- p.DR_26.34
  p.DR[36:50] <- p.DR_35.49
  p.DR[51:65] <- p.DR_50_64
  p.DR[66:99] <- p.DR_65_99
  
  # Recurrence
  p.RD = NULL
  p.RD[1:12] <- p.RD[100] <- 0 # final value = 0 because mortality prob = 1
  p.RD[13:18] <- p.RD_12.17
  p.RD[19:26] <- p.RD_18.25
  p.RD[27:35] <- p.RD_26.34
  p.RD[36:50] <- p.RD_35.49
  p.RD[51:65] <- p.RD_50_64
  p.RD[66:99] <- p.RD_65_99
  
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
  # Initiation - No initiation after 25
  p.NC = unname(v.policy[["initeff"]])*smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))
  
  # Cessation - No cessation before 18
  p.CF = unname(v.policy[["cesseff"]])*smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
  
  rr.DX = c(rep(1,18),rep(rr.DX_18.25,8),rep(rr.DX_26.34,9),rep(rr.DX_35.49,15),rep(rr.DX_50.64,15),rep(rr.DX_65.99,34),1)
  
  # Simulate for each birth cohort with parallelization: row = each person within birth cohort, columns = ages 0:99
  m.M <-foreach (i=cohorts, .combine='rbind', .packages='darthtools',
                 .export=c('mds_microsim','probs','get_prevs',
                           'n.i','n.t','v.n','n.s','v.M_1',
                           'p.NC','p.CF','p.NX','p.CX','a_p.FX.ysq',
                           'rr.DX','p.HD', 'p.DR', 'p.RD',
                           'rr.ND.CD','rr.CH.CD','rr.CR.CD','rr.CD.FD',
                           'p.NO.NO', 'p.NO.CO', 'p.NO.CV', 'p.NO.NV', 
                           'p.NV.NV', 'p.NV.CQ', 'p.NV.CV', 'p.NV.NQ', 
                           'p.FV.CQ', 'p.FV.FV', 'p.FV.CV', 'p.FV.FQ',
                           'p.NQ.CQ', 'p.NQ.CV', 'p.NQ.NQ', 'p.NQ.NV', 
                           'p.FO.CO', 'p.FO.CV', 'p.FO.FO', 'p.FO.FV',
                           'p.FQ.CQ', 'p.FQ.CV', 'p.FQ.FQ', 'p.FQ.FV', 
                           'p.CO.CO', 'p.CO.CV', 'p.CO.FO', 'p.CO.FV',
                           'p.CQ.CQ', 'p.CQ.CV', 'p.CQ.FQ', 'p.CQ.FV',
                           'p.CV.CQ', 'p.CV.CV', 'p.CV.FQ', 'p.CV.FV')) %dopar% {
                             mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
                           }
  # run in serial for debugging:
  #m.M <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Count each death only once by removing repeated 'X' values
  firstX = apply(m.M,1,function(x) min(which(x=="X"))) # find first cell where each individual dies by iterating across each person (row = 1)
  for (i in 1:nrow(m.M)){
    if(firstX[i]!=101 & firstX[i]!=Inf){
      m.M[i, (firstX[i]+1):101]<-NA
    }
  }
  
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
  
  # Get mortality counts by year
  n.X = apply(m.M_cy,2,function(x) sum(x=="X" ,na.rm=TRUE)) # iterate across each year (column=2) and sum up the X's
  # Get total person life-years by year
  n.lifeyears = apply(m.M_cy,2,function(x) sum(x!="X",na.rm=TRUE)) 
  
  # Output prevalence results as a list
  model_res <- lapply(c("N","C","F","D"), get_prevs, m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts)) # denominator is everyone still alive
  model_res <- c(model_res, lapply(c("ND","CD","FD"), get_subgroup_prevs, denom="D",m_cohortbyyear=m.M_cy, minyear=calib_startyear, maxyear=max(cohorts))) # denominator is everyone in "D" subpopulation
  model_res1 <- lapply(c("C","F"), get_prevs1, m_cohortbyyear=m.M_cy, minyear=policyyear, maxyear=max(cohorts)) # get prevs by single year of age
  
  names(model_res) <- c("N","C","F","D","ND","CD","FD")
  for (l in 1:length(model_res)){
    model_res[[l]] <- model_res[[l]][order(model_res[[l]][,"age"],decreasing=FALSE),] # re-order the age groups from 18.25, 18.99, 26.34, etc
  }
  names(model_res1) <- c("C","F")
  for (l in 1:length(model_res1)){
    model_res1[[l]] <- model_res1[[l]][order(model_res1[[l]][,"age"],decreasing=FALSE),] # re-order the age groups from 18.25, 18.99, 26.34, etc
  }
  
  # Calculate costs, utilities, and productivity for each health state by age in the population
  total_cuw <- rbind(colSums(m.C_cy,na.rm=TRUE),colSums(m.U_cy,na.rm=TRUE),colSums(m.W_cy,na.rm=TRUE))
  rownames(total_cuw)<-c("total costs","total QALYs","total productivity")
  colnames(total_cuw) <-colnames(m.C_cy) <- colnames(m.U_cy) <- colnames(m.W_cy) <- c(min(cohorts):(max(cohorts)+100))
  
  # Assume 2024 is the starting year for discounting purposes, calculate discount weight based on the discount rate d.c
  v.d = c(rep(1,policyyear-1900), c(1 / (1 + d.c) ^ (0:(ncol(m.C_cy)-(policyyear-1899))))) # vector of discount weights
  
  # Total discounted costs, QALYs, and productivity
  d_total_cuw <- total_cuw %*% diag(v.d) # multiple each row of the cuw matrix by the discounting vector
  colnames(d_total_cuw) <-colnames(total_cuw)
  cuw <- t(d_total_cuw)
  
  # Calculate smoking-attributable mortality 2022-2100
  age_range <- 18:99
  year_range <- policyyear:max(cohorts)
  prev_cs <- xtabs(prev ~ age + year, data = model_res1$C)
  prev_fs <- xtabs(prev ~ age + year, data = model_res1$F)
  attr(prev_cs, "class") <- attr(prev_cs, "call") <-  attr(prev_fs, "class") <- attr(prev_fs, "call") <- NULL
  prev_cs <- prev_cs[as.character(age_range), as.character(year_range)]
  prev_fs <- prev_fs[as.character(age_range), as.character(year_range)]
  SAD <- colSums(pop[,as.character(year_range)] * (prev_cs * (deathrates_cs[,as.character(year_range)] - deathrates_ns[,as.character(year_range)]) + 
                          prev_fs * (deathrates_fs[,as.character(year_range)] - deathrates_ns[,as.character(year_range)])))
  
  cat(paste0("\n  ", v.params," "))
  print(Sys.time() - t_init) # End timer
  return(list(model_res = model_res, cuw=cuw, n.X=n.X, n.lifeyears=n.lifeyears, SAD=SAD, init = p.NC, cess = p.CF))
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