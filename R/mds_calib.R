rm(list = ls())  # remove any variables in R's memory
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
setwd(file.path("/gpfs/gibbs/project/tam_jamie/jzl9/mds-microsim/"))
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
cohorts <- 1900:2100           # Change from 2015 to 2020
calib_startyear <-2005
n.i   <- 1000                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years
v.n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NH", n.i)         # everyone begins in the Never smoker Never MD state
# d.c <- d.u <- d.w <- 0.03              # equal discounting of costsand QALYs by 3
# 
# # Cost inputs 
# c.NH <- c.NR <- rep(2000, n.t)           # cost of remaining one cycle Never Smoking, No MD
# c.CH <- c.CR <- rep(4000, n.t)            # cost of remaining one cycle Current Smoking, No MD 
# c.FH <- c.FR <- rep(15000, n.t)           # cost of remaining one cycle Former Smoking, No MD
# c.ND <- rep(12000, n.t)                    # cost of remaining one cycle Never Smoking, MD            
# c.CD <- rep(20000, n.t)                    # cost of remaining one cycle Current Smoking, MD
# c.FD <- rep(15000, n.t)                    # cost of remaining one cycle Former Smoking, MD
# 
# # Utility inputs
# u.NH <- u.NR <- rep(1, n.t)        
# u.CH <- u.CR <- rep(0.75, n.t)     
# u.FH <- u.FR <- rep(0.5, n.t)      
# u.ND <- rep(0.95, n.t)                
# u.CD <- rep(0.6, n.t)
# u.FD <- rep(0.8, n.t)
# 
# # Productivity inputs
# w.NH <- w.NR <- rep(50000, n.t)
# w.CH <- w.CR <- rep(40000, n.t)
# w.FH <- w.FR <- rep(45000, n.t)
# w.ND <- rep(45000, n.t)
# w.CD <- rep(35000, n.t)
# w.FD <- rep(40000, n.t)

## CALIBRATION PARAMETERS
param_names <- c("s.NC_9.17","s.NC_18.25",
                 "s.CF_18.25", "s.CF_26.34", "s.CF_35.49" ,"s.CF_50.64"  ,"s.CF_65.99" , 
                 "p.DR","s.HD_12.17", "s.HD_18.25", "s.HD_26.34",
                 "rr.DX_18.25","rr.DX_26.34", "rr.DX_35.49","rr.DX_50.64","rr.DX_65.99", 
                 "rr.ND.CD", "rr.CH.CD","rr.CR.CD","rr.CD.FD", "yearinc_p.HD")

# Parameter Values
if (length(args) > 0) {
  if (args[1] == "males") {
    value <- c(2.05287465042159, 0.0848159716005523,
               0.758981274877442, 0.616667582480333, 0.746427943601139, 0.912583380690363, 0.720470979706167,
               0.28702367776264, 7.078470, 1.690519, 2,
               1.08073196073528, 3.69236171640223, 3.77460941897007, 3.43143755495548, 2.49499687193893,
               2.70249504281674, 1.4132489374518, 2.41892186717596, 1.54728820101549, 2015)
    } else if (args[1] == "females") {
      value <- c(2.081761888, 0,
               0.993978012, 0.564477195, 0.87073767, 0.654785173, 0.664328015,
               0.3793009, 2.1019429, 3.6714343, 4,
               7.284504694, 5.400727017, 5.518611922, 6.34692571, 3.801531806,
               1.271321945, 1.848291364, 1.528161447, 1.220915551, 2014)
  } else {
    stop("Invalid argument. Please use 'males' or 'females'.")
  }
} else {
  stop("No argument provided. Please specify 'males' or 'females'.")
}


upper <- c(2.5,1,
           rep(1,5),
           1,rep(10,3),
           rep(8,5),
           rep(3,4),
           2016.5)
lower <- c(2.0,0,
           rep(0.5,5),
           0,rep(0,3),
           rep(1,5),
           rep(1,4),
           2012.5)

## Specify which parameters you want to calibrate
calib <-c(0,0, 
          rep(0,5), 
          1, rep(1,3),
          rep(1,5),
          1,1,1,1,
          1)
calib_inputs <- cbind(value,lower,upper,calib)  
rownames(calib_inputs) <- param_names
v.params <- calib_inputs[calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate

# Number of initial starting points
n.init <- 40

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
# 
##  Select multiple random starting values with Latin Hypercube Sampling ###
# set.seed(32788)
# X <- randomLHS(n.init,length(v.params)) # LHS to cover parameter space evenly
# 
# v.params_init <- matrix(nrow=n.init,ncol=n.param)
# 
# for (i in 1:n.param){
#   v.params_init[,i] <- qunif(X[,i],min=lb[i],max=ub[i])
# }
# colnames(v.params_init) <- names(v.params)
# 
# # v.params_init[1,] <- v.params # replace first initial set with v.params
# 
# # record start time of calibration
# t_init <- Sys.time()
# 
# # ###  Run optimization algorithm for each starting point  ###
# m.calib_res <- matrix(nrow = n.init, ncol = n.param+1)
# colnames(m.calib_res) <- c(names(v.params), "Overall_fit")
# for (j in 1:n.init){ # j <- 1
# 
#   # L-BFGS-B optimization method - minimization
#   fit_nm <- lbfgsb3c(par = v.params_init[j,], fn = f_gof, lower=lb, upper=ub)
# 
#   m.calib_res[j,] <- c(fit_nm$par, fit_nm$value)
# }
# 
# # Calculate computation time
# comp_time <- Sys.time() - t_init
# 
# ### Arrange parameter sets in order of fit
# m.calib_res <- m.calib_res[order(m.calib_res[,"Overall_fit"]),]
# 
# v.params = m.calib_res[1,1:length(v.params)] # store best fit as v.params
# 
# # print parameter initial values, calibrated estimates
# print(v.params_init)
# print(m.calib_res)
# print(whichgender)

## GENERATE OUTPUTS
source("R/mds_outputs.R", echo=FALSE)

source("R/mds_outputs_policy.R", echo=FALSE)
