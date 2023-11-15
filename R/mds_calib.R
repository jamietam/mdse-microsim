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
setwd(file.path("/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/"))
# here::i_am("R/mds_calib.R")

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

## INPUTS 
whichgender <- "males"

load(paste0("data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0("data/smk_precomputed_inputs_",whichgender,".RData")) #lst_smktargets																			  
cohorts <- 1900:2020            # Change from 2015 to 2020
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
                 "p.DR","s.HD_12.17", "s.HD_18.25", 
                 "rr.DX_18.25","rr.DX_26.34", "rr.DX_35.49","rr.DX_50.64","rr.DX_65.99", 
                 "rr.ND.CD", "rr.CH.CD","rr.CR.CD","rr.CD.FD")
# value<-c(2.1991585, 0, 
#          0.7833367, 0.5510059,  0.6532555, 0.5790247,  0.6374476, 
#          0.173, 1,  1, 
#          5.5302388,  6.8444939, 4.7752103,  7.7882404,  3.7432371, 
#          1.5520095,  1.6584458,  2.1256031,  2.2640631)
value<-c(2.081761888, 0, 
         0.993978012, 0.564477195, 0.87073767, 0.654785173, 0.664328015,
         0.379283838, 3.500968429, 3.908561303,
         7.284504694, 5.400727017, 5.518611922, 6.34692571,3.801531806,
         1.271321945,1.848291364 ,1.528161447,1.220915551)

upper <- c(2.5,1,
           rep(1,5),
           1,rep(8,2),
           rep(8,5),
           rep(3,4))
lower <- c(2.0,0,
           rep(0.5,5),
           0,rep(1,2),
           rep(1,5),
           rep(1,4))

## Specify which parameters you want to calibrate
calib <-c(1,1, 
          rep(1,5), 1, rep(1,2),rep(1,5),1,1,1,1)
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
source("R/mds_outputs.R", echo=FALSE)
