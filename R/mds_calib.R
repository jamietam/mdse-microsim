## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to algorithm
f_gof <- function(v_params){
  
  model_res <- main(v_params)   # Run model for parameter set "v_params"
  v_GOF <- numeric(n_target)   # Calculate goodness-of-fit of model outputs to targets
  
  # Calibrate to N, C, F, D and ND/D, CD/D, FD/D prevalences
  for (r in 1:length(lst_calibtargets)){ # sum of squared differences
  # for (r in 1:4){
    gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
    v_GOF[r] <-gof
  }
  
  # OVERALL
  v_weights <- rep(1,n_target) # can assign targets different weights
  # weighted sum
  GOF_overall <- sum(v_GOF[1:n_target] * v_weights)
  cat(GOF_overall)
  # return GOF
  return(GOF_overall)
}

## Calibrate! --------------------------------------------------------------

##  Select multiple random starting values with Latin Hypercube Sampling ###
set.seed(32788)
X <- randomLHS(n_init,length(v_params)) # LHS to cover parameter space evenly

v_params_init <- matrix(nrow=n_init,ncol=n_param)

for (i in 1:n_param){
  v_params_init[,i] <- qunif(X[,i],min=lb[i],max=ub[i]) 
}
colnames(v_params_init) <- names(v_params)

# record start time of calibration
t_init <- Sys.time()

###  Run optimization algorithm for each starting point  ###
m_calib_res <- matrix(nrow = n_init, ncol = n_param+1)
colnames(m_calib_res) <- c(names(v_params), "Overall_fit")
for (j in 1:n_init){ # j <- 1
  
  # L-BFGS-B optimization method - minimization 
  fit_nm <- lbfgsb3c(par = v_params_init[j,], fn = f_gof, lower=lb, upper=ub)
  
  m_calib_res[j,] <- c(fit_nm$par, fit_nm$value)
  cat(paste("FINISHED INITIAL STARTING POINT: ",j))
}

# Calculate computation time
comp_time <- Sys.time() - t_init

### Arrange parameter sets in order of fit
m_calib_res <- m_calib_res[order(m_calib_res[,"Overall_fit"]),]

v_params = m_calib_res[1,1:length(v_params)] # store best fit as v_params

# print parameter initial values, calibrated estimates
print(v_params_init)
print(m_calib_res)
print(whichgender)