## Clean up the workspace and set main working directory
rm(list = ls()) 
## RUN CALIBRATION
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/Users/srs249/Documents/GitHub/mds-microsim/"
mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mdse-microsim/" # Set working directory
hpc = 0# 1 = run using high performance computing clusters, 0 = run without
calibration = 0 # 1 = run with calibration, 0 = run without calibration
args <- `if`(hpc == 1, commandArgs(TRUE), c("females", 1000, 2023, 20)) # Parameters for HPV vs non-HPC setup

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo=FALSE) # microsimulation model and probability functions

v.target_names <- names(l.calib_targets) # number of calibration targets
n.target <- length(v.target_names)

if (calibration == 1) { ## For calibration runs - Run this section of code
  # Number of initial starting points
  n.init <- as.numeric(args[4])
  
  # Provide ranges for input search space
  v.lb <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"lower"] # lower bound
  v.ub <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"upper"]  # upper bound
  
  #  Select multiple random starting values with Latin Hypercube Sampling 
  set.seed(32788)
  
  v.params_init <- matrix(nrow=n.init,ncol=n.param)
  v.params_init[1,] <- v.params # replace first initial set with v.params to ensure initial best fit must be improved
  
  X <- randomLHS((n.init-1),length(v.params)) # LHS to cover parameter space evenly, see https://lhs.r-forge.r-project.org/lhs_questions.html
  
  for (i in 1:n.param){
    v.params_init[2:n.init,i] <- qunif(X[,i],min=v.lb[i],max=v.ub[i])
  }
  colnames(v.params_init) <- names(v.params)
  
  
  # Record start time of calibration
  t_init <- Sys.time()
  
  # Run optimization algorithm for each starting point  ###
  m.calib_res <- matrix(nrow = n.init, ncol = n.param+1)
  colnames(m.calib_res) <- c(names(v.params), "Overall_fit")
  for (j in 1:n.init){ # j <- 1
    
  # L-BFGS-B optimization method
  l.fit_optim <-   optim(v.params_init[j,], f_gof, method = "L-BFGS-B",lower=v.lb,upper=v.ub,
                      control = list(fnscale = -1, # fnscale = -1 switches from minimization to maximization
                                     maxit = 1000),
                      hessian = T)
  m.calib_res[j,] <- c(l.fit_optim$par, l.fit_optim$value)
  }
  
  # Calculate computation time
  comp_time <- Sys.time() - t_init
  
  # Arrange parameter sets in order of fit
  m.calib_res <- m.calib_res[order(m.calib_res[,"Overall_fit"],decreasing = TRUE),]
  
  v.params = m.calib_res[1,1:length(v.params)] # store best fit as v.params
  
  # print parameter initial values, calibrated estimates
  print(v.params_init)
  print(m.calib_res)
  print(whichgender)
}

## GENERATE OUTPUTS
source(paste0(mainDir,"R/05_validation.R"), echo=TRUE)
