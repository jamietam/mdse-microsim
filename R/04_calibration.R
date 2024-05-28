## RUN CALIBRATION
source(paste0(mainDir,"R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

# Number of initial starting points
n.init <- 40

# Provide ranges for input search space
lb <- calib_inputs[calib_inputs[,"calib"]==1,][,"lower"] # lower bound
ub <- calib_inputs[calib_inputs[,"calib"]==1,][,"upper"]  # upper bound

v.target_names <- names(lst_calibtargets) # number of calibration targets
n.target <- length(v.target_names)

#  Select multiple random starting values with Latin Hypercube Sampling ###
set.seed(32788)
X <- randomLHS(n.init,length(v.params)) # LHS to cover parameter space evenly

v.params_init <- matrix(nrow=n.init,ncol=n.param)

for (i in 1:n.param){
  v.params_init[,i] <- qunif(X[,i],min=lb[i],max=ub[i])
}
colnames(v.params_init) <- names(v.params)

# v.params_init[1,] <- v.params # replace first initial set with v.params

# Record start time of calibration
t_init <- Sys.time()

# Run optimization algorithm for each starting point  ###
m.calib_res <- matrix(nrow = n.init, ncol = n.param+1)
colnames(m.calib_res) <- c(names(v.params), "Overall_fit")
for (j in 1:n.init){ # j <- 1

  # L-BFGS-B optimization method - minimization
  fit_nm <- lbfgsb3c(par = v.params_init[j,], fn = f_gof, lower=lb, upper=ub)

  m.calib_res[j,] <- c(fit_nm$par, fit_nm$value)
}

# Calculate computation time
comp_time <- Sys.time() - t_init

# Arrange parameter sets in order of fit
m.calib_res <- m.calib_res[order(m.calib_res[,"Overall_fit"]),]

v.params = m.calib_res[1,1:length(v.params)] # store best fit as v.params

# print parameter initial values, calibrated estimates
print(v.params_init)
print(m.calib_res)
print(whichgender)

## GENERATE OUTPUTS
source(paste0(mainDir,"R/05_validation.R"), echo=TRUE)


# Latin Hypercube Sampling Code: https://lhs.r-forge.r-project.org/lhs_questions.html
# Fit by age group # gof <- sum((lst_targets[[r]][lst_targets[[r]][,"age"]<=27,][,"prev"] - model_res[[r]][model_res[[r]][,"age"]<=27,][,"prev"])^2) # only fit to ages groups 18.25, 18.99, 26.34
