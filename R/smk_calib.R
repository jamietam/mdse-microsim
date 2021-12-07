rm(list = ls())  # remove any variables in R's memory
library(plyr)
library(matrixStats)
library(stringr)
library(lhs) # Load packages for calibration
library(IMIS) # calibration functionality
library(matrixStats) # package used for sumamry statistics
library(plotrix) # visualization
library(psych)
library(scatterplot3d) # now that we have three inputs to estimate, we'll need higher dimension visualization
library(doParallel) # set up model to run in parallel
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

mainDir <- "C:/Users/jamietam/Dropbox/GitHub/mds-microsim"
setwd(file.path(mainDir))
namethisrun <- "smk_microsim_11.24"

## Inputs
whichgender <- "females"
load(paste0("data/smk_inputs_",whichgender,".RData")) # Load all smoking and mortality inputs as matrices
n.i   <- 10                      # number of simulated individuals per run (cohort)
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 

## Calibration Targets
load(paste0("data/smk_microsim_targets_",whichgender,".RData"))

## Load model functions
source("R/smk_microsim.R", echo = FALSE) # microsimulation model and probability functions

## Run model for parameter calibration
smk_calib_out <- function(smkinit_SF, smkcess_SF) {

  smk_init = smkinit_SF*smk_init_cisnet # adjusted smoking inputs
  smk_cess = smkcess_SF*smk_cess_cisnet
  
  cohorts <- 1900:2100
  
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
  rownames(m.cohortbyyear) <- paste(sort(rep(cohorts,n.i)),1:n.i, sep = ".") 
  
  # Output prevalence results as a list
  m.M.prevs <- NULL 
  for (i in c(v.n)){
    m.M.prevs = rbind(m.M.prevs, get_prevs(i, m.cohortbyyear,2005,2020))
  }
  m.M.prevs <- as.data.frame(m.M.prevs)
  m.M.prevs$prev <- as.numeric(m.M.prevs$prev)
  m.M.prevs$year <- as.numeric(m.M.prevs$year)
  model_res <- vector(mode = "list")
  model_res$cstotal <- subset(m.M.prevs, agegroup=="total" & state=="C")[,c("year","prev")] 
  model_res$cs18to25 <- subset(m.M.prevs, agegroup=="18to25" & state=="C")[,c("year","prev")] 
  model_res$cs26to34 <- subset(m.M.prevs, agegroup=="26to34" & state=="C")[,c("year","prev")] 
  model_res$cs35to49 <- subset(m.M.prevs, agegroup=="35to49" & state=="C")[,c("year","prev")] 
  model_res$cs50to64 <- subset(m.M.prevs, agegroup=="50to64" & state=="C")[,c("year","prev")] 
  model_res$cs65plus <- subset(m.M.prevs, agegroup=="65plus" & state=="C")[,c("year","prev")] 
  model_res$nstotal <- subset(m.M.prevs, agegroup=="total" & state=="N")[,c("year","prev")] 
  model_res$ns18to25 <- subset(m.M.prevs, agegroup=="18to25" & state=="N")[,c("year","prev")] 
  model_res$ns26to34 <- subset(m.M.prevs, agegroup=="26to34" & state=="N")[,c("year","prev")] 
  model_res$ns35to49 <- subset(m.M.prevs, agegroup=="35to49" & state=="N")[,c("year","prev")] 
  model_res$ns50to64 <- subset(m.M.prevs, agegroup=="50to64" & state=="N")[,c("year","prev")] 
  model_res$ns65plus <- subset(m.M.prevs, agegroup=="65plus" & state=="N")[,c("year","prev")] 
  model_res$fstotal <- subset(m.M.prevs, agegroup=="total" & state=="F")[,c("year","prev")] 
  model_res$fs18to25 <- subset(m.M.prevs, agegroup=="18to25" & state=="F")[,c("year","prev")] 
  model_res$fs26to34 <- subset(m.M.prevs, agegroup=="26to34" & state=="F")[,c("year","prev")] 
  model_res$fs35to49 <- subset(m.M.prevs, agegroup=="35to49" & state=="F")[,c("year","prev")] 
  model_res$fs50to64 <- subset(m.M.prevs, agegroup=="50to64" & state=="F")[,c("year","prev")] 
  model_res$fs65plus <- subset(m.M.prevs, agegroup=="65plus" & state=="F")[,c("year","prev")] 
  
  return(model_res)
}

t_init <- Sys.time()
system.time(
  model_res<- smk_calib_out(2.0,0.5)		
)
Sys.time() - t_init
																									
save(model_res, file=paste0(namethisrun,"_n",n.i,"_",whichgender,".Rdata"))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of random samples
n_resamp <- 10

# names and number of input parameters to be calibrated
v_param_names <- c("smkinit_SF","smkcess_SF")
n_param <- length(v_param_names)

# range on input search space
lb <- c(smkinit_SF = 1, smkcess_SF = 0.05) # lower bound
ub <- c(smkinit_SF = 5, smkcess_SF = 1) # upper bound

# number of calibration targets
v_target_names <- names(lst_smktargets)
n_target <- length(v_target_names)

## Calibration Functions ---------------------------------------------------

# Write function to sample from prior
sample_prior <- function(n_samp){
  m_lhs_unit   <- randomLHS(n = n_samp, k = n_param)
  m_param_samp <- matrix(nrow = n_samp, ncol = n_param)
  colnames(m_param_samp) <- v_param_names
  for (i in 1:n_param){
    m_param_samp[, i] <- qunif(m_lhs_unit[,i],
                               min = lb[i],
                               max = ub[i])
  }
  return(m_param_samp)
}

# view resulting parameter set samples
pairs.panels(sample_prior(1000))

###  PRIOR  ### 
# Write functions to evaluate log-prior and prior

# function that calculates the log-prior
calc_log_prior <- function(v_params){
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_samp <- nrow(v_params)
  colnames(v_params) <- v_param_names
  lprior <- rep(0, n_samp)
  for (i in 1:n_param){
    lprior <- lprior + dunif(v_params[, i],
                             min = lb[i],
                             max = ub[i], 
                             log = T)
  }
  return(lprior)
}


v_params_test = c(smkinit_SF = 1, smkcess_SF = 1)
# Run simulation by passing arguments as a numeric vector to the model function
do.call(smk_calib_out, as.list(v_params_test)) # It works!

calc_log_prior(v_params = v_params_test)
calc_log_prior(v_params = sample_prior(10))


# function that calculates the (non-log) prior
calc_prior <- function(v_params) { 
  exp(calc_log_prior(v_params)) 
}
calc_prior(v_params = v_params_test)
calc_prior(v_params = sample_prior(10))


###  LIKELIHOOD  ###
# Write functions to evaluate log-likelihood and likelihood

# function to calculate the log-likelihood
calc_log_lik <- function(v_params){
  # par_vector: a vector (or matrix) of model parameters 
  if(is.null(dim(v_params))) { # If vector, change to matrix
    v_params <- t(v_params) 
  }
  n_samp <- nrow(v_params)
  v_llik <- matrix(0, nrow = n_samp, ncol = n_target) 
  llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( { 
      ###   Run model for parametr set "v_params" ###
      model_res <- do.call(smk_calib_out, as.list(v_params[j, ]))
      
      ###  Calculate log-likelihood of model outputs to targets  ###
      # Loop through all calibration targets in lst_smktargets (18 total):
      # "cstotal"  "cs18to25" "cs26to34" "cs35to49" "cs50to64" "cs65plus" 
      # "nstotal"  "ns18to25" "ns26to34" "ns35to49" "ns50to64" "ns65plus" 
      # "fstotal"  "fs18to25" "fs26to34" "fs35to49" "fs50to64" "fs65plus"
      # log likelihood 
      for (r in 1:length(lst_smktargets)){
        v_llik[j, r] <- sum(dnorm(x = lst_smktargets[[r]]$prev,
                                  mean = model_res[[r]]$prev,
                                  sd = lst_smktargets[[r]]$se,
                                  log = T))
      }
      
      # OVERALL 
      llik_overall[j] <- sum(v_llik[j, ])
    }, error = function(e) NA) 
    if(is.na(jj)) { llik_overall <- -Inf }
  } # End loop over sampled parameter sets
  # return LLIK
  return(llik_overall)
}
calc_log_lik(v_params = v_params_test)
calc_log_lik(v_params = sample_prior(10))


# function to calculate the (non-log) likelihood
calc_likelihood <- function(v_params){ 
  exp(calc_log_lik(v_params)) 
}
calc_likelihood(v_params = v_params_test)
calc_likelihood(v_params = sample_prior(10))


###  POSTERIOR  ###
# Write functions to evaluate log-posterior and posterior

# function that calculates the log-posterior
calc_log_post <- function(v_params) { 
  lpost <- calc_log_prior(v_params) + calc_log_lik(v_params)
  return(lpost) 
}
calc_log_post(v_params = v_params_test)
calc_log_post(v_params = sample_prior(10))


# function that calculates the (non-log) posterior
calc_post <- function(v_params) { 
  exp(calc_log_post(v_params)) 
}
calc_post(v_params = v_params_test)
calc_post(v_params = sample_prior(10))


####################################################################
######  Calibrate!  ######
####################################################################
# record start time of calibration
t_init <- Sys.time()

###  Bayesian calibration using IMIS  ###
# define three functions needed by IMIS: prior(x), likelihood(x), sample.prior(n)
prior <- calc_prior
likelihood <- calc_likelihood
sample.prior <- sample_prior

# run IMIS
fit_imis <- IMIS(B = 1000, # the incremental sample size at each iteration of IMIS
                 B.re = n_resamp, # the desired posterior sample size
                 number_k = 10, # the maximum number of iterations in IMIS
                 D = 0) 

# obtain draws from posterior
m_calib_res <- fit_imis$resample

# Calculate log-likelihood (overall fit) and posterior probability of each sample
m_calib_res <- cbind(m_calib_res, 
                     "Overall_fit" = calc_log_lik(m_calib_res[,v_param_names]),
                     "Posterior_prob" = calc_post(m_calib_res[,v_param_names]))

# normalize posterior probability
m_calib_res[,"Posterior_prob"] <- m_calib_res[,"Posterior_prob"]/sum(m_calib_res[,"Posterior_prob"])

# Calculate computation time
comp_time <- Sys.time() - t_init


####################################################################
######  Exploring best-fitting input sets  ######
####################################################################

# Plot the 1000 draws from the posterior
v_post_color <- scales::rescale(m_calib_res[,"Posterior_prob"])
s3d <- scatterplot3d(x = m_calib_res[, 1],
                     y = m_calib_res[, 2],
                     z = m_calib_res[, 3],
                     color = scales::alpha("black", v_post_color),
                     xlim = c(lb[1],ub[1]), ylim = c(lb[2],ub[2]), zlim = c(lb[3],ub[3]),
                     xlab = v_param_names[1], ylab = v_param_names[2], zlab = v_param_names[3])
# add center of Gaussian components
s3d$points3d(fit_imis$center, col = "red", pch = 8)

# Plot the 1000 draws from the posterior with marginal histograms
pairs.panels(m_calib_res[,v_param_names])

# Compute posterior mean
v_calib_post_mean <- colMeans(m_calib_res[,v_param_names])
v_calib_post_mean

# Compute posterior median and 95% credible interval
m_calib_res_95cr <- colQuantiles(m_calib_res[,v_param_names], probs = c(0.025, 0.5, 0.975))
m_calib_res_95cr

# Compute maximum-a-posteriori (MAP) parameter set
v_calib_map <- m_calib_res[which.max(m_calib_res[,"Posterior_prob"]),]


### Plot model-predicted output at best set vs targets ###
v_out_best <- run_sick_sicker_markov(v_calib_map[v_param_names])

# TARGET 1: Survival ("Surv")
plotrix::plotCI(x = lst_targets$Surv$time, y = lst_targets$Surv$value, 
                ui = lst_targets$Surv$ub,
                li = lst_targets$Surv$lb,
                ylim = c(0, 1), 
                xlab = "Time", ylab = "Pr Survive")
points(x = lst_targets$Surv$time, 
       y = v_out_best$Surv, 
       pch = 8, col = "red")
legend("topright", 
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

# TARGET 2: "Prev"
plotrix::plotCI(x = lst_targets$Prev$time, y = lst_targets$Prev$value,
                ui = lst_targets$Prev$ub,
                li = lst_targets$Prev$lb,
                ylim = c(0, 1),
                xlab = "Time", ylab = "Prev")
points(x = lst_targets$Prev$time,
       y = v_out_best$Prev,
       pch = 8, col = "red")
legend("topright",
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

# TARGET 3: "PropSick"
plotrix::plotCI(x = lst_targets$PropSick$time, y = lst_targets$PropSick$value,
                ui = lst_targets$PropSick$ub,
                li = lst_targets$PropSick$lb,
                ylim = c(0, 1),
                xlab = "Time", ylab = "PropSick")
points(x = lst_targets$PropSick$time,
       y = v_out_best$PropSick,
       pch = 8, col = "red")
legend("topright",
       legend = c("Target", "Model-predicted output"),
       col = c("black", "red"), pch = c(1, 8))

####################################################################
######  Propagate calibrated parameter uncertainty  ######
####################################################################
#### 04.5.3 Compute IMIS posterior predicted outputs ####
m_out_surv     <- matrix(NA, nrow = n_resamp, ncol = length(lst_targets$Surv$value))  
m_out_prev     <- matrix(NA, nrow = n_resamp, ncol = length(lst_targets$Prev$value))  
m_out_propsick <- matrix(NA, nrow = n_resamp, ncol = length(lst_targets$PropSick$value))   

### Run model for each posterior parameter set
for(i in 1:n_resamp){ # i = 1
  model_res_temp <- run_sick_sicker_markov(m_calib_res[i, ])
  m_out_surv[i, ] <- model_res_temp$Surv
  m_out_prev[i, ] <- model_res_temp$Prev
  m_out_propsick[i, ] <- model_res_temp$PropSick
  if(i/100==round(i/100,0)) { 
    cat('\r',paste(i/n_resamp*100,"% done",sep=""))
  }
}

## Posterior predictedmean
m_out_surv_postmean <- colMeans(m_out_surv)
m_out_prev_postmean <- colMeans(m_out_prev)
m_out_propsick_postmean <- colMeans(m_out_propsick)

# Data visualization ------------------------------------------------------

library(ggplot2)

load("C:/Users/jamietam/Dropbox/Analysis/NSDUH/depsmkprevs_2005-2020.rda")


nsduh <- subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop")

model <- as.data.frame(m.M.prevs)
model$prev <- as.numeric(model$prev)
model$year <- as.numeric(model$year)

cs_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop" &age!="total"), 
             aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ns_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Never smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ns <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Never smokers - Women")+
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
grid_arrange_shared_legend(list(ns, cs, fs),3,"")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"")
dev.off()


## PROBABILITY CHECKS ------------------------------------------------------
cohorts = c(1900:2100)
for (bc in cohorts){
  p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
  p.CF <- round(diag(as.matrix(smk_cess)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
  p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
  p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
  p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
  
  p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
  p.NC[100] <- p.CF[100] <- 0 
  
    for (t in c(1:n.t)){
      if (bc+t>2100){ # exit for loop if going past the year 2100
        break
      }
      
    N = c((1-p.NX[t])*(1 - p.NC[t]), 
          (1-p.NX[t])*p.NC[t], 
           0, 	
           p.NX[t]) 
    C = c( 0,
          (1-p.CX[t])*(1- p.CF[t]), 
          (1-p.CX[t])*p.CF[t], 
           p.CX[t]) 
    F = c( 0,
           0, 
          (1 - p.FX[t]), 
           p.FX[t])
    
    
    allprobs = rbind(N, C, F)
    # Check for any negative, missing probabilities, or probability sets that do not sum to 1
    if(any(is.na(allprobs))){
      print(paste("NA probability! bc: ", bc, ", age: ",t))
      print(allprobs)
    }
    if(any(allprobs<0)){
      print(paste("Negative probability! bc: ", bc, ", age: ",t))
      print(allprobs)
      }
    if(any(round(rowSums(allprobs),8) != 1)){
      print(paste("Probabilities do not sum to 1! ", "bc:",bc,"age:",t))
      print (rowSums(allprobs))
      }
    }
}

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