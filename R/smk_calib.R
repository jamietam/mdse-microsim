rm(list = ls())  # remove any variables in R's memory
library(plyr)
library(matrixStats)
library(stringr)
library(lhs) # Load packages for calibration
library(matrixStats) # package used for summary statistics
library(ggplot2) # visualization
library(psych) # pair.panels
library(doParallel) # set up model to run in parallel
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

mainDir <- "C:/Users/JT936/Dropbox/GitHub/mds-microsim"
# mainDir <- "/gpfs/ysm/home/jt936/mds-microsim"
setwd(file.path(mainDir))
namethisrun <- "smk_calib_12.27.21"

## Inputs
whichgender <- "females"
load(paste0("data/smk_inputs_",whichgender,".RData")) # Load all smoking and mortality inputs as matrices
n.i   <- 10                      # number of simulated individuals per run (cohort)
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 

## Calibration Targets
load(paste0("data/smk_calib_targets_",whichgender,".RData"))

## Load model functions
source("R/smk_microsim.R", echo = FALSE) # microsimulation model and probability functions

## Run model for parameter calibration
smk_calib_out <- function(v_params) { #smkinit_SF, smkcess_SF

  smk_init = v_params[1]*smk_init_cisnet # adjusted smoking inputs
  smk_cess = v_params[2]*smk_cess_cisnet # how many parameters should I have here?
  
  cohorts <- 1900:2020
  
  # Simulate for each birth cohort
  m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                          .export=c('smk_microsim','smk_probs','get_prevs', 
                                    'smk_init','smk_cess','death_cs','death_ns','death_fs',
                                    'n.i','n.t','v.n','n.s','v.M_1')) %dopar%
    {
      smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
    }
  
  # Convert matrix from cohort-age to cohort-year
  m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = 301)
  for (b in 1:length(cohorts)){
    m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
  }
  colnames(m.cohortbyyear) <- c(1900:2200)

  # Output prevalence results as a list
  model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=2020)
  names(model_res) <- v.n
  model_res$N <- model_res$N[order(model_res$N[,"agegroup"],decreasing=FALSE),]
  model_res$C <- model_res$C[order(model_res$C[,"agegroup"],decreasing=FALSE),]
  model_res$F <- model_res$F[order(model_res$F[,"agegroup"],decreasing=FALSE),]
  return(model_res)
}

t_init <- Sys.time()
system.time(
  model_res<- smk_calib_out(v_params) #1.1686259 0.3283054
)
Sys.time() - t_init
																									
save(model_res, file=paste0(namethisrun,"_n",n.i,"_",whichgender,".Rdata"))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n_init <- 10

# names and number of input parameters to be calibrated
v_param_names <- c("smkinit_SF","smkcess_SF")
n_param <- length(v_param_names)

# range on input search space
lb <- c(smkinit_SF = 1, smkcess_SF = 0.05) # lower bound
ub <- c(smkinit_SF = 2, smkcess_SF = 1) # upper bound

# number of calibration targets
v_target_names <- names(lst_smktargets)
n_target <- length(v_target_names)

v_params <- c(smkinit_SF = 1, smkcess_SF=1)

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){
  
  # Run model for parameter set "v_params"
  model_res <- smk_calib_out(v_params)
  
  # Calculate goodness-of-fit of model outputs to targets
  v_GOF <- numeric(n_target)

  for (r in 1:length(lst_smktargets)){ # sum of squared differences
    v_GOF[r] <- sum((lst_smktargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2)
  }

  # OVERALL
  # can give different targets different weights
  v_weights <- rep(1,n_target)
  # weighted sum
  GOF_overall <- sum(v_GOF[1:n_target] * v_weights)
  
  # return GOF
  return(GOF_overall)
}


####################################################################
######  Calibrate!  ######
####################################################################

###  Sample multiple random starting values for Nelder-Mead  ###
v_params_init <- matrix(nrow=n_init,ncol=n_param)
for (i in 1:n_param){
  v_params_init[,i] <- runif(n_init,min=lb[i],max=ub[i]) # This should probably be LHS to cover parameter space evenly
} 
colnames(v_params_init) <- v_param_names

# record start time of calibration
t_init <- Sys.time()

###  Run Nelder-Mead for each starting point  ###
m_calib_res <- matrix(nrow = n_init, ncol = n_param+1)
colnames(m_calib_res) <- c(v_param_names, "Overall_fit")
for (j in 1:n_init){ # j <- 1
  
  # use optim() as Nelder-Mead, default is minimization
  fit_nm <- optim(v_params_init[j,], f_gof, hessian = T)
  m_calib_res[j,] <- c(fit_nm$par, fit_nm$value)
  
  
  v_params = c(1,1)
  fit_nm <- optim(v_params, f_gof,control = list(fnscale = 1, maxit = 1000), hessian = T)
  
}

# Calculate computation time
comp_time <- Sys.time() - t_init

####################################################################
######  Exploring best-fitting input sets  ######
####################################################################

# Arrange parameter sets in order of fit
m_calib_res <- m_calib_res[order(-m_calib_res[,"Overall_fit"]),]

# Examine the top 10 best-fitting sets
m_calib_res[1:10,]

# Plot the top 10 (top 10%)
plot(m_calib_res[1:10,1],m_calib_res[1:10,2],
     xlim=c(lb[1],ub[1]),ylim=c(lb[2],ub[2]),
     xlab = colnames(m_calib_res)[1],ylab = colnames(m_calib_res)[2])

# Pairwise comparison of top 10 sets
pairs.panels(m_calib_res[1:10,v_param_names])


# Data visualization ------------------------------------------------------

load("C:/Users/JT936/Dropbox/Analysis/NSDUH/depsmkprevs_2005-2020.rda")

nsduh <- subset(depsmkprevs_by_year, gender==whichgender & subpopulation=="totalpop" & (status=="currentsmoker"|status=="neversmoker" |status=="formersmoker"))
nsduh$agegroup[nsduh$age=="total"]=18.99
nsduh$agegroup[nsduh$age=="18to25"]=18.25
nsduh$agegroup[nsduh$age=="26to34"]=26.34
nsduh$agegroup[nsduh$age=="35to49"]=35.49
nsduh$agegroup[nsduh$age=="50to64"]=50.64
nsduh$agegroup[nsduh$age=="65plus"]=65.99

ns_age <-ggplot() +
  geom_pointrange(data= subset(nsduh,status=="neversmoker"), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=as.factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = as.data.frame(model_res$N),  aes(x=year, y= prev, colour=as.factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Never smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_age <-ggplot() +
  geom_pointrange(data= subset(nsduh,status=="currentsmoker"), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=as.factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = as.data.frame(model_res$C),  aes(x=year, y= prev, colour=as.factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(nsduh,status=="formersmoker"), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=as.factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = as.data.frame(model_res$F),  aes(x=year, y= prev, colour=as.factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

model <- rbind(subset(as.data.frame(model_res$C), agegroup==18.99),
              subset(as.data.frame(model_res$N), agegroup==18.99),
              subset(as.data.frame(model_res$F), agegroup==18.99))
model$status <- c(rep("currentsmoker",16),rep("neversmoker",16),rep("formersmoker",16))

ncf_total <- ggplot() +
  geom_pointrange(data=subset(nsduh, age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = model,  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

library(gridBase)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(plots,columns,titletext) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(arrangeGrob(grobs= lapply(plots, function(x)
    x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),ncol=columns),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    top=textGrob(titletext,just="top", vjust=1,check.overlap=TRUE,gp=gpar(fontsize=9, fontface="bold"))
  )
}

pdf(file = paste0("Figs_", namethisrun,".pdf"),width=10, height=6,onefile = FALSE)
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"")
ncf_total
dev.off()

############################################################################################
## The calibration code was adapted from the DARTH workgroup (www.darthworkgroup.com). 

# - Alarid-Escudero F, Maclehose RF, Peralta Y, Kuntz KM, Enns EA. 
#   Non-identifiability in model calibration and implications for 
#   medical decision making. Med Decis Making. 2018; 38(7):810-821.

# - Jalal H, Pechlivanoglou P, Krijkamp E, Alarid-Escudero F, Enns E, 
#   Hunink MG. An Overview of R in Health Decision Sciences. 
#   Med Decis Making. 2017; 37(3): 735-746. 

# For more information: https://darth-git.github.io/calibSMDM2018-materials/

## The microsimulation model code was adapted from Appendix A of the article: 
#
# - Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
#   Microsimulation modeling for health decision sciences using R: A tutorial. 
#   Med Decis Making. 2018;38(3):400-22.
												 
# For more information: https://github.com/DARTH-git/Microsimulation-tutorial
 
############################################################################################