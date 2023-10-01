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
here::i_am("R/mds_calib.R")

####### For HPC runs ###########################################################
library(doParallel) ## Run on a single node
n_cores = Sys.getenv("SLURM_CPUS_PER_TASK")
cl <- makeCluster(as.numeric(n_cores),type="FORK")
registerDoParallel(cl)

####### For Personal Computer and Open On Demand Interface runs ################
# library(doParallel) # set up model to run in parallel
# n_cores = Sys.getenv("SLURM_CPUS_PER_TASK")
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)

## INPUTS 
whichgender <- "females"

load(paste0("data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0("data/smk_precomputed_inputs_",whichgender,".RData")) #lst_smktargets																			  
cohorts <- 1900:2020            # Change from 2015 to 2020
n_i   <- 1000                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n_t   <- 100                    # time horizon per person, number of years
v_n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), Dead (X)
n_s   <- length(v_n)            # the number of health states
v_M_1 <- rep("NH", n_i)         # everyone begins in the Never smoker Never MD state 

## CALIBRATION PARAMETERS
param_names <- c("s.NC_9.17","s.NC_18.25",
                 "s.CF_18.25", "s.CF_26.34", "s.CF_35.49" ,"s.CF_50.64"  ,"s.CF_65.99" , 
                 "s.HD_12.17", "s.HD_18.25", 
                 "rr.DX_18.25","rr.DX_26.34", "rr.DX_35.49","rr.DX_50.64","rr.DX_65.99", 
                 "rr.ND.CD", "rr.CH.CD","rr.CR.CD","rr.CD.CR","rr.CD.FD")
value<-c(2.181656, 0, 
         0.953952552750707, 0.87277384842746,  0.747292307089083, 0.725445074085355,  0.568203055122699, 
         1.363110, 4.844015,    
         5.523897, 7.9721387, 4.574927, 5.191135,  1.001617, 
         3.715614, 1.039868, 4.350371, 1, 1.0005584)
upper <- c(5,1,rep(1,5),rep(10,2),rep(8,5),rep(5,5))
lower <- c(rep(0,2),rep(0,5),rep(1,2),rep(1,5),rep(1,5))

## Specify which parameters you want to calibrate
calib <-c(0,0, rep(0,5), rep(1,2),rep(1,5),1,1,1,0,1)
calib_inputs <- cbind(value,lower,upper,calib)  
rownames(calib_inputs) <- param_names
v_params <- calib_inputs[calib_inputs[,"calib"]==1,][,"value"]  
n_param <- length(v_params) # number of parameters to calibrate

# Number of initial starting points
n_init <- 40 

# Provide ranges for input search space
lb <- calib_inputs[calib_inputs[,"calib"]==1,][,"lower"] # lower bound
ub <- calib_inputs[calib_inputs[,"calib"]==1,][,"upper"]  # upper bound

## CALIBRATION TARGETS
load(paste0("data/smk_calib_targets_",whichgender,".RData")) #lst_smktargets
load(paste0("data/dep_calib_targets_",whichgender,".RData")) #lst_deptargets
load(paste0("data/smkdep_calib_targets_",whichgender,".RData")) #lst_smkdeptargets
lst_targets <- c(lst_smktargets,lst_deptargets[2],lst_smkdeptargets)
lst_calibtargets <- lapply(lst_targets,function(x) x[x[,"survey_year"]<=max(cohorts),]) # keep survey years based on last cohort 2015 vs 2020

v_target_names <- names(lst_calibtargets) # number of calibration targets
n_target <- length(v_target_names)

## MODEL FUNCTIONS
source("R/mds_microsim.R", echo = FALSE) # microsimulation model and probability functions

## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
  
  t_init <- Sys.time() # Start timer
  
  s.NC_9.17 <- ifelse(calib_inputs["s.NC_9.17","calib"]==1,v_params["s.NC_9.17"],calib_inputs["s.NC_9.17","value"])
  s.NC_18.25 <- ifelse(calib_inputs["s.NC_18.25","calib"]==1,v_params["s.NC_18.25"],calib_inputs["s.NC_18.25","value"])

  s.CF_18.25 <- ifelse(calib_inputs["s.CF_18.25","calib"]==1,v_params["s.CF_18.25"],calib_inputs["s.CF_18.25","value"])
  s.CF_26.34 <- ifelse(calib_inputs["s.CF_26.34","calib"]==1,v_params["s.CF_26.34"],calib_inputs["s.CF_26.34","value"])
  s.CF_35.49 <- ifelse(calib_inputs["s.CF_35.49","calib"]==1,v_params["s.CF_35.49"],calib_inputs["s.CF_35.49","value"])
  s.CF_50.64 <- ifelse(calib_inputs["s.CF_50.64","calib"]==1,v_params["s.CF_50.64"],calib_inputs["s.CF_50.64","value"])
  s.CF_65.99 <- ifelse(calib_inputs["s.CF_65.99","calib"]==1,v_params["s.CF_65.99"],calib_inputs["s.CF_65.99","value"])

  s.HD_12.17 <-  ifelse(calib_inputs["s.HD_12.17","calib"]==1,v_params["s.HD_12.17"],calib_inputs["s.HD_12.17","value"])
  s.HD_18.25 <-  ifelse(calib_inputs["s.HD_18.25","calib"]==1,v_params["s.HD_18.25"],calib_inputs["s.HD_18.25","value"])

  rr.DX_18.25 <- ifelse(calib_inputs["rr.DX_18.25","calib"]==1,v_params["rr.DX_18.25"],calib_inputs["rr.DX_18.25","value"])
  rr.DX_26.34 <- ifelse(calib_inputs["rr.DX_26.34","calib"]==1,v_params["rr.DX_26.34"],calib_inputs["rr.DX_26.34","value"])
  rr.DX_35.49 <- ifelse(calib_inputs["rr.DX_35.49","calib"]==1,v_params["rr.DX_35.49"],calib_inputs["rr.DX_35.49","value"])
  rr.DX_50.64 <- ifelse(calib_inputs["rr.DX_50.64","calib"]==1,v_params["rr.DX_50.64"],calib_inputs["rr.DX_50.64","value"])
  rr.DX_65.99 <- ifelse(calib_inputs["rr.DX_65.99","calib"]==1,v_params["rr.DX_65.99"],calib_inputs["rr.DX_65.99","value"])

  rr.ND.CD <- ifelse(calib_inputs["rr.ND.CD","calib"]==1,v_params["rr.ND.CD"],calib_inputs["rr.ND.CD","value"])
  rr.CH.CD <- ifelse(calib_inputs["rr.CH.CD","calib"]==1,v_params["rr.CH.CD"],calib_inputs["rr.CH.CD","value"])
  rr.CR.CD <- ifelse(calib_inputs["rr.CR.CD","calib"]==1,v_params["rr.CR.CD"],calib_inputs["rr.CR.CD","value"])
  rr.CD.CR <- ifelse(calib_inputs["rr.CD.CR","calib"]==1,v_params["rr.CD.CR"],calib_inputs["rr.CD.CR","value"])
  rr.CD.FD <- ifelse(calib_inputs["rr.CD.FD","calib"]==1,v_params["rr.CD.FD"],calib_inputs["rr.CD.FD","value"])

  ## Incidence
  for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
    bc1 = bc-1899
    for (age in 0:25){ # increase applies to youth and young adults ages 0-25
      if ((bc+age)>=2016 & age>=18){ # starting in 2016
        p.HD[(age+1),bc1] = s.HD_18.25*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=2016 & age<18){
        p.HD[(age+1),bc1] = s.HD_12.17*p.HD[(age+1),bc1]
      }
    }
  }
  ## Initiation - No initiation after 25
  p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))
  
  ## Cessation - No cessation before 18
  p.CF = smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))
  
  rr.DX = c(rep(1,18),rep(rr.DX_18.25,8),rep(rr.DX_26.34,9),rep(rr.DX_35.49,15),rep(rr.DX_50.64,15),rep(rr.DX_65.99,34),1)
  
  # Simulate for each birth cohort with parallelization
  m_cohortbyage<-foreach (i=cohorts, .combine='rbind', .packages='darthtools',
                          .export=c('mds_microsim','probs','get_prevs',
                                    'n_i','n_t','v_n','n_s','v_M_1',
                                    'p.NC','p.CF','p.NX','p.CX','a_p.FX.ysq',
                                    'rr.DX','p.HD', 'p.DR', 'p.RD',
                                    'rr.ND.CD','rr.CH.CD','rr.CR.CD','rr.CD.CR','rr.CD.FD')) %dopar% {
                                      mds_microsim(i, v_M_1, n_i, n_t, v_n)$m_M
  }
  # run in serial for debugging:
  # m_cohortbyage <- do.call(rbind, lapply(cohorts, function(i) { mds_microsim(i, v_M_1, n_i, n_t, v_n)$m_M }))
  
  
  # Convert matrix from cohort-age to cohort-year
  m_cohortbyyear <- matrix(nrow = n_i*length(cohorts), ncol = (length(cohorts)+100))
  for (b in 1:length(cohorts)){
    m_cohortbyyear[(n_i*(b-1)+1):(n_i*b),b:(100+b)] <- m_cohortbyage[(n_i*(b-1)+1):(n_i*b),]
  }
  colnames(m_cohortbyyear) <- c(min(cohorts):(max(cohorts)+100))
  
  # Output prevalence results as a list
  model_res <- lapply(c("N","C","F","D"), get_prevs, m_cohortbyyear=m_cohortbyyear, minyear=2005, maxyear=max(cohorts)) # denominator is everyone still alive
  model_res <- c(model_res, lapply(c("ND","CD","FD"), get_subgroup_prevs, denom="D",m_cohortbyyear=m_cohortbyyear, minyear=2005, maxyear=max(cohorts))) # denominator is everyone in "D" subpopulation
  names(model_res) <- c("N","C","F","D","ND","CD","FD")
  for (l in 1:length(model_res)){
    model_res[[l]] <- model_res[[l]][order(model_res[[l]][,"age"],decreasing=FALSE),] # re-order the age groups from 18.25, 18.99, 26.34, etc
  }
  
  cat(paste0("\n  ", v_params," "))
  print(Sys.time() - t_init) # End timer
  return(model_res)
}

source("R/mds_calib.R", echo = TRUE) # run calibration 

# Run the model (again) ---------------------------------------------------
model_res<-main(v_params)

v_GOF <- numeric(n_target)   # Calculate goodness-of-fit of model outputs to targets
for (r in 1:length(lst_calibtargets)){ # sum of squared differences
  gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
  v_GOF[r] <-gof 
}
names(v_GOF) <- paste0(names(lst_targets),".fit_value")
fit_value <- sum(v_GOF)
print(v_GOF)
# Data visualization ------------------------------------------------------

## Figures for initiation and cessation
s.NC_9.17 <- ifelse(calib_inputs["s.NC_9.17","calib"]==1,v_params["s.NC_9.17"],calib_inputs["s.NC_9.17","value"])
s.NC_18.25 <- ifelse(calib_inputs["s.NC_18.25","calib"]==1,v_params["s.NC_18.25"],calib_inputs["s.NC_18.25","value"])

s.CF_18.25 <- ifelse(calib_inputs["s.CF_18.25","calib"]==1,v_params["s.CF_18.25"],calib_inputs["s.CF_18.25","value"])
s.CF_26.34 <- ifelse(calib_inputs["s.CF_26.34","calib"]==1,v_params["s.CF_26.34"],calib_inputs["s.CF_26.34","value"])
s.CF_35.49 <- ifelse(calib_inputs["s.CF_35.49","calib"]==1,v_params["s.CF_35.49"],calib_inputs["s.CF_35.49","value"])
s.CF_50.64 <- ifelse(calib_inputs["s.CF_50.64","calib"]==1,v_params["s.CF_50.64"],calib_inputs["s.CF_50.64","value"])
s.CF_65.99 <- ifelse(calib_inputs["s.CF_65.99","calib"]==1,v_params["s.CF_65.99"],calib_inputs["s.CF_65.99","value"])

s.HD_12.17 <-  ifelse(calib_inputs["s.HD_12.17","calib"]==1,v_params["s.HD_12.17"],calib_inputs["s.HD_12.17","value"])
s.HD_18.25 <-  ifelse(calib_inputs["s.HD_18.25","calib"]==1,v_params["s.HD_18.25"],calib_inputs["s.HD_18.25","value"])

rr.DX_18.25 <- ifelse(calib_inputs["rr.DX_18.25","calib"]==1,v_params["rr.DX_18.25"],calib_inputs["rr.DX_18.25","value"])
rr.DX_26.34 <- ifelse(calib_inputs["rr.DX_26.34","calib"]==1,v_params["rr.DX_26.34"],calib_inputs["rr.DX_26.34","value"])
rr.DX_35.49 <- ifelse(calib_inputs["rr.DX_35.49","calib"]==1,v_params["rr.DX_35.49"],calib_inputs["rr.DX_35.49","value"])
rr.DX_50.64 <- ifelse(calib_inputs["rr.DX_50.64","calib"]==1,v_params["rr.DX_50.64"],calib_inputs["rr.DX_50.64","value"])
rr.DX_65.99 <- ifelse(calib_inputs["rr.DX_65.99","calib"]==1,v_params["rr.DX_65.99"],calib_inputs["rr.DX_65.99","value"])

rr.ND.CD <- ifelse(calib_inputs["rr.ND.CD","calib"]==1,v_params["rr.ND.CD"],calib_inputs["rr.ND.CD","value"])
rr.CH.CD <- ifelse(calib_inputs["rr.CH.CD","calib"]==1,v_params["rr.CH.CD"],calib_inputs["rr.CH.CD","value"])
rr.CR.CD <- ifelse(calib_inputs["rr.CR.CD","calib"]==1,v_params["rr.CR.CD"],calib_inputs["rr.CR.CD","value"])
rr.CD.CR <- ifelse(calib_inputs["rr.CD.CR","calib"]==1,v_params["rr.CD.CR"],calib_inputs["rr.CD.CR","value"])
rr.CD.FD <- ifelse(calib_inputs["rr.CD.FD","calib"]==1,v_params["rr.CD.FD"],calib_inputs["rr.CD.FD","value"])

## Initiation - No initiation after 25
p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))

## Cessation - No cessation before 18
p.CF = smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))

rr.DX = c(rep(1,18),rep(rr.DX_18.25,8),rep(rr.DX_26.34,9),rep(rr.DX_35.49,15),rep(rr.DX_50.64,15),rep(rr.DX_65.99,34),1)
modelprev <- rbind(cbind(data.frame(model_res$N),status="neversmoker"),
                   cbind(data.frame(model_res$C),status="currentsmoker"),
                   cbind(data.frame(model_res$F),status="formersmoker"),
                   cbind(data.frame(model_res$D),status="depressed"),
                   cbind(data.frame(model_res$ND),status="neversmokerD"),
                   cbind(data.frame(model_res$CD),status="currentsmokerD"),
                   cbind(data.frame(model_res$FD),status="formersmokerD"))

calibtargets = rbind(cbind(data.frame(lst_targets[["N"]]),status="neversmoker"),
                     cbind(data.frame(lst_targets[["C"]]),status="currentsmoker"),
                     cbind(data.frame(lst_targets[["F"]]),status="formersmoker"),
                     cbind(data.frame(lst_targets[["D"]]),status="depressed"),
                     cbind(data.frame(lst_targets[["ND"]]),status="neversmokerD"),
                     cbind(data.frame(lst_targets[["CD"]]),status="currentsmokerD"),
                     cbind(data.frame(lst_targets[["FD"]]),status="formersmokerD"))

p.NCsmk_init <- as.data.frame(cbind(c(p.NC[,100],rr.ND.CD*p.NC[,100],smk_init[,100]),c(rep("calibrated",100),rep("rr.ND.CD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(p.NCsmk_init) <- c("prob","inputs","age")
p.NCsmk_init$prob<-as.numeric(as.character(p.NCsmk_init$prob))
p.NCsmk_init$age<-as.numeric(as.character(p.NCsmk_init$age))
p.NC_age <- ggplot(data=p.NCsmk_init) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Initiation probabilities")

p.CFsmk_cess <- as.data.frame(cbind(c(p.CF[,100],rr.CD.FD*p.CF[,100],smk_cess[,100]),c(rep("calibrated",100),rep("rr.CD.FD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(p.CFsmk_cess) <- c("prob","inputs","age")
p.CFsmk_cess$prob<-as.numeric(as.character(p.CFsmk_cess$prob))
p.CFsmk_cess$age<-as.numeric(as.character(p.CFsmk_cess$age))
p.CF_age <- ggplot(data=p.CFsmk_cess) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Cessation probabilities")

ns_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="neversmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="neversmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Never smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Current smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Former smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99 & (status=="neversmoker" | status=="currentsmoker" | status=="formersmoker")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, age==18.99 & (status=="neversmoker" | status=="currentsmoker" | status=="formersmoker") ),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figure for incidence inputs
## Incidence
for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
  bc1 = bc-1899
  for (age in 0:25){ # increase applies to youth and young adults ages 0-25
    if ((bc+age)>=2016 & age>=18){ # starting in 2016
      p.HD[(age+1),bc1] = s.HD_18.25*p.HD[(age+1),bc1]
    }
    if ((bc+age)>=2016 & age<18){
      p.HD[(age+1),bc1] = s.HD_12.17*p.HD[(age+1),bc1]
    }
  }
}
p.HD_age <- ggplot() + geom_line(aes(x=0:99,y=p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD)", limits=c(0,0.25), breaks=seq(0,0.25,0.01)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence, calibrated estimates",color=NULL)

p.HD_ageC <- ggplot() + geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD, rr.CH.CD)", limits=c(0,0.25), breaks=seq(0,0.25,0.01)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence among current smokers, rr.CH.CD*",color=NULL)

## Figure for recovery inputs
p.DR_age <- ggplot() +  geom_line( aes(x=0:99, y=p.DR)) + geom_point(aes(x=0:99),y=rr.CD.CR*p.DR)+ 
  scale_y_continuous(name="Probability of recovery (p.DR)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates, rr.CD.CR" )

## Figure for recurrence inputs
p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD)) + geom_point(aes(x=0:99),y=rr.CR.CD*p.RD)+
  scale_y_continuous(name="Probability of recurrence (p.RD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recurrence, calibrated estimates, rr.CR.CD")

D_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="depressed"&age!=18.99), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), 
                      shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="depressed" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, status=="depressed" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="depressed" & age==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figures for depressed population
ns_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="neversmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="neversmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Never smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Current smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Former smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_totalD <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99 & (status=="neversmokerD" | status=="currentsmokerD" | status=="formersmokerD")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, age==18.99 & (status=="neversmokerD" | status=="currentsmokerD" | status=="formersmokerD")),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figures for mortality by smoking and dep status  
p.NCFX <- as.data.frame(cbind(c(p.NX[,100],p.CX[,100],a_p.FX.ysq[,100,5]),c(rep("NX",100),rep("CX",100),rep("FX",100)),c(rep(0:99,3))))
names(p.NCFX) <- c("prob","status","age")
p.NCFX$prob <- as.numeric(p.NCFX$prob)
p.NCFX$age <- as.numeric(p.NCFX$age)
p.NCFX_age <- ggplot(data=p.NCFX) +  geom_line( aes(x=age, y=prob, color=status)) + 
  geom_line(aes(x=age,y=prob*rr.DX,linetype=status,color=status))+
  scale_y_continuous(name="Annual mortality by smoking status (rr.DX)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Mortality probabilities by smoking and MDE status, rr.DX")


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

df.calib <- merge(as.data.frame(v_params),as.data.frame(calib_inputs),by="row.names",all.x=TRUE,all.y=TRUE,sort=FALSE)
colnames(df.calib)[1:3] <- c("parameters", "est","initial")

pdf(file = paste0(whichgender,"_mds_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I:%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
grid.table(c(v_GOF, fit_value),rows=c(names(lst_targets),"Overall Fit"))
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration parameters", font=2, cex=1.5)
grid.table(df.calib)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"Smoking distribution")
ncf_total
grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
grid.arrange(p.DR_age,p.RD_age,ncol=2)
D_age
D_total
grid_arrange_shared_legend(list(ns_ageD, cs_ageD, fs_ageD),3,"Smoking distribution among people with depression")
ncf_totalD
p.NCFX_age
dev.off()

## For MPI ONLY: 
## Put this at beginning:
# library(doMPI) ## Run on multiple nodes
# cl<-startMPIcluster()
# registerDoMPI(cl)
## Put this at the end of the R script:
# closeCluster(cl)
# mpi.quit()
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
# Latin Hypercube Sampling Code: https://lhs.r-forge.r-project.org/lhs_questions.html
# Fit by age group # gof <- sum((lst_targets[[r]][lst_targets[[r]][,"age"]<=27,][,"prev"] - model_res[[r]][model_res[[r]][,"age"]<=27,][,"prev"])^2) # only fit to ages groups 18.25, 18.99, 26.34
