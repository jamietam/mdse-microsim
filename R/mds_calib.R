
library(here)
library(stringr)
library(lbfgsb3c)
library(splines)
library(foreach) # parallelization is in the foreach loop
library(ggplot2)
library(gridBase)
library(gridExtra)
library(grid)
library(lhs)
library(darthtools)
library(matrixStats)
# setwd(file.path("/Users/JT936/Dropbox/GitHub/mds-microsim/"))
setwd(file.path("/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/"))
here::i_am("R/mds_calib.R")

####### For HPC runs ###########################################################
library(doParallel) ## Run on a single node
n.cores = Sys.getenv("SLURM_CPUS_PER_TASK")
cl <- makeCluster(as.numeric(n.cores),type="FORK")
registerDoParallel(cl)

####### For Personal Computer and Open On Demand Interface runs ################
# library(doParallel) # set up model to run in parallel
# n.cores = Sys.getenv("SLURM_CPUS_PER_TASK")
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)

args <- commandArgs(TRUE)

## INPUTS 
whichgender <- args[1]

load(paste0("data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0("data/smk_precomputed_inputs_",whichgender,".RData")) #lst_smktargets
load(paste0("data/cuw_inputs_",whichgender,".RData"))
cohorts <- 1900:2022            # Change from 2015 to 2020
calib_startyear <-2005
n.i   <- 1000                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years
v.n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NH", n.i)         # everyone begins in the Never smoker Never MD state
d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%

## CALIBRATION PARAMETERS - Specify which parameters you want to calibrate (0 vs 1 in column 4), and provide upper and lower bounds for the search algorithm
if (whichgender == "males") {
  calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.05287465042159, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0.0848159716005523, 0, 1, 0),
    "s.CF_18.25" = c(0.758981274877442, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.616667582480333, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.746427943601139, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.912583380690363, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.720470979706167, 0.50, 1.0, 0),
    "p.DR" = c(0.28702367776264, 0.0, 1.0, 0),
    "s.HD_12.17" = c(7.078470, 0.0, 10.0, 0),
    "s.HD_18.25" = c(1.690519, 0.0, 10.0, 0), 
    "s.HD_26.34" = c(2.0, 0.0, 10.0, 0),
    "rr.DX_18.25" = c(1.08073196073528, 1.0, 8.0, 0),
    "rr.DX_26.34" = c(3.69236171640223, 1.0, 8.0, 0),
    "rr.DX_35.49" = c(3.77460941897007, 1.0, 8.0, 0),
    "rr.DX_50.64" = c(3.43143755495548, 1.0, 8.0, 0),
    "rr.DX_65.99" = c(2.49499687193893, 1.0, 8.03, 0),
    "rr.ND.CD" = c(2.70249504281674, 1.0, 4.0, 0),
    "rr.CH.CD" = c(1.4132489374518, 1.0, 4.0, 0),
    "rr.CR.CD" = c(2.41892186717596, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1.54728820101549, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2015, 2012.5, 2016.5, 0))
} else if (whichgender == "females") {
  calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.081761888, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0, 0, 1, 0),
    "s.CF_18.25" = c(0.993978012, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.564477195, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.87073767, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.654785173, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.664328015, 0.50, 1.0, 0),
    "p.DR" = c(0.3793009, 0.0, 1.0, 0),
    "s.HD_12.17" = c(2.1019429, 0.0, 10.0, 0),
    "s.HD_18.25" = c(3.6714343, 0.0, 10.0, 0), 
    "s.HD_26.34" = c(4.0, 0.0, 10.0, 0),
    "rr.DX_18.25" = c(7.284504694, 1.0, 8.0, 0),
    "rr.DX_26.34" = c(5.400727017, 1.0, 8.0, 0),
    "rr.DX_35.49" = c(5.518611922, 1.0, 8.0, 0),
    "rr.DX_50.64" = c(6.34692571, 1.0, 8.0, 0),
    "rr.DX_65.99" = c(3.801531806, 1.0, 8.0, 0),
    "rr.ND.CD" = c(1.271321945, 1.0, 4.0, 0),
    "rr.CH.CD" = c(1.848291364, 1.0, 4.0, 0),
    "rr.CR.CD" = c(1.528161447, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1.220915551, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2014, 2012.5, 2016.5, 0))
} 
colnames(calib_inputs) =c("value","lower","upper","calib")  
v.params <- calib_inputs[calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate

# Number of initial starting points
n.init <- 20 

# Provide ranges for input search space
lb <- calib_inputs[calib_inputs[,"calib"]==1,][,"lower"] # lower bound
ub <- calib_inputs[calib_inputs[,"calib"]==1,][,"upper"]  # upper bound

## CALIBRATION TARGETS
load(paste0("data/smk_calib_targets_",whichgender,".RData")) #lst_smktargets
load(paste0("data/dep_calib_targets_",whichgender,".RData")) #lst_deptargets
load(paste0("data/smkdep_calib_targets_",whichgender,".RData")) #lst_smkdeptargets
lst_targets <- c(lst_smktargets,lst_deptargets[2],lst_smkdeptargets)
lst_calibtargets <- lapply(lst_targets,function(x) x[x[,"survey_year"]<=max(cohorts) & x[,"survey_year"]>=calib_startyear,]) # keep survey years 2016-2020

v.target_names <- names(lst_calibtargets) # number of calibration targets
n.target <- length(v.target_names)

## MODEL FUNCTIONS
source("R/mds_microsim.R", echo = FALSE) # microsimulation model and probability functions

## RUN MODEL FOR ALL BIRTH COHORTS
source("R/mds_main.R", echo=FALSE)

## RUN CALIBRATION
# Calibrate! --------------------------------------------------------------

##  Select multiple random starting values with Latin Hypercube Sampling ###
set.seed(32788)
X <- randomLHS(n.init,length(v.params)) # LHS to cover parameter space evenly

v.params_init <- matrix(nrow=n.init,ncol=n.param)

for (i in 1:n.param){
  v.params_init[,i] <- qunif(X[,i],min=lb[i],max=ub[i]) 
}
colnames(v.params_init) <- names(v.params)

# v.params_init[1,] <- v.params # replace first initial set with v.params

# record start time of calibration
t_init <- Sys.time()

###  Run optimization algorithm for each starting point  ###
m.calib_res <- matrix(nrow = n.init, ncol = n.param+1)
colnames(m.calib_res) <- c(names(v.params), "Overall_fit")
for (j in 1:n.init){ # j <- 1
  
  # L-BFGS-B optimization method - minimization 
  fit_nm <- lbfgsb3c(par = v.params_init[j,], fn = f_gof, lower=lb, upper=ub)
  
  m.calib_res[j,] <- c(fit_nm$par, fit_nm$value)
}

# Calculate computation time
comp_time <- Sys.time() - t_init

### Arrange parameter sets in order of fit
m.calib_res <- m.calib_res[order(m.calib_res[,"Overall_fit"]),]

v.params = m.calib_res[1,1:length(v.params)] # store best fit as v.params

# print parameter initial values, calibrated estimates
print(v.params_init)
print(m.calib_res)
print(whichgender)

## GENERATE OUTPUTS
source("R/mds_outputs.R", echo=TRUE)
