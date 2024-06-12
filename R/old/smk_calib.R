rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(lbfgsb3c)
library(foreach) # parallelization is in the foreach loop
library(ggplot2)
library(gridBase)
library(gridExtra)
library(grid)
setwd(file.path("/gpfs/gibbs/project/tam_jamie/shared/mds-microsim/"))
here::i_am("R/smk_calib.R")
## For HPC runs
n_cores = Sys.getenv("SLURM_CPUS_PER_TASK")
cl <- makeCluster(as.numeric(n_cores),type="FORK")
## For Personal Computer and Open On Demand Interface runs
# cl <- makeCluster(detectCores())
registerDoParallel(cl)

## INPUTS
whichgender <- "females" 
s.NC_9.17=2.181656 # females best fit_value=0.1005130
s.NC_18.25= 0

# whichgender <- "males" 
# s.NC_9.17=1.62823408465733 # males fit_value=0.168578616625688
# s.NC_18.25= 0.744987876855674

load(paste0(here("data/smk_precomputed_inputs_"),whichgender,".RData")) # p.CX, p.FX.ysq, p.NX, smk_cess, smk_init
cohorts <- 1900:2020
n.i   <- 1000                    # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 

## CALIBRATION PARAMETERS
v_params <- c("s.CF_18.25"= 0.0710292148869485, "s.CF_26.34"= 1.06624004784143, "s.CF_35.49" = 0.581030531786382,"s.CF_50.64" = 1.09528614839045 ,"s.CF_65.99" = 1.04268235996959) 
n_param <- length(v_params)
set.seed(072218) # Specify seed (for reproducible sequence of random numbers)
n_init <- 15 # number of initial starting points
# range on input search space
lb <- c(0,0,0,0,0)# lower bound
ub <- c(1,1.3,1.3,1.3,1.3)# upper bound


## CALIBRATION TARGETS
load(paste0(here("data/smk_calib_targets_"),whichgender,".RData")) #lst_smktargets																			  
v_target_names <- names(lst_smktargets) # number of calibration targets
n_target <- length(v_target_names)

## MODEL FUNCTIONS
source("R/smk_microsim.R", echo = FALSE) # microsimulation model and probability functions

## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
    
  t_init <- Sys.time() # Start timer
  
  ## Initiation - No initiation after 25
  p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),  rep(0,74))

  ## Cessation - No cessation before 18
  p.CF = smk_cess*c(rep(0,18),rep(v_params["s.CF_18.25"],8), rep(v_params["s.CF_26.34"],9),rep(v_params["s.CF_35.49"],15),rep(v_params["s.CF_50.64"],15),rep(v_params["s.CF_65.99"],35))
    
  # Simulate for each birth cohort with parallelization
  m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                          .export=c('smk_microsim','smk_probs','get_prevs', 
                                    'n.i','n.t','v.n','n.s','v.M_1',
                                    'p.NC','p.CF','p.NX','p.CX','p.FX.ysq')) %dopar% {
                                      smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M    
                                      }
    ### Serial:
    # m.cohortbyage <- do.call(rbind, lapply(cohorts, function(i) { smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))

    # Convert matrix from cohort-age to cohort-year
    m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = (length(cohorts)+100))
    for (b in 1:length(cohorts)){
      m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
    }
    colnames(m.cohortbyyear) <- c(min(cohorts):(max(cohorts)+100))
    
    # Output prevalence results as a list
    model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=max(cohorts))
    names(model_res) <- v.n
    model_res$N <- model_res$N[order(model_res$N[,"agegroup"],decreasing=FALSE),]
    model_res$C <- model_res$C[order(model_res$C[,"agegroup"],decreasing=FALSE),]
    model_res$F <- model_res$F[order(model_res$F[,"agegroup"],decreasing=FALSE),]
    
    cat(paste0("\n  ", v_params," "))
    print(Sys.time() - t_init) # End timer
    return(model_res)
}

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){

  model_res <- main(v_params)   # Run model for parameter set "v_params"
  v_GOF <- numeric(n_target)   # Calculate goodness-of-fit of model outputs to targets

  # Calibrate to never smoker prevalence only
  # r=1
  # v_GOF<- sum((lst_smktargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group

  # Calibrate to never, current, and former smoker prevalences
  for (r in 1:length(lst_smktargets)){ # sum of squared differences
    gof<- sum((lst_smktargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
    # gof18.99<-sum((lst_smktargets18.99[[r]][,"prev"] - model_res18.99[[r]][,"prev"])^2)  # prevalence for all adults ages 18-99
    v_GOF[r] <-gof #+gof18.99
  }
  
  # OVERALL
  # can assign targets different weights
  v_weights <- rep(1,n_target)
  # weighted sum
  GOF_overall <- sum(v_GOF[1:n_target] * v_weights)
  cat(GOF_overall)
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
  
}

# Calculate computation time
comp_time <- Sys.time() - t_init

### Arrange parameter sets in order of fit
m_calib_res <- m_calib_res[order(m_calib_res[,"Overall_fit"]),]

v_params = m_calib_res[1,1:length(v_params)]

model_res<-main(v_params)

v_GOF <- numeric(n_target)   # Calculate goodness-of-fit of model outputs to targets
for (r in 1:length(lst_smktargets)){ # sum of squared differences
  gof<- sum((lst_smktargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
  v_GOF[r] <-gof #+gof18.99
}

fit_value <- sum(v_GOF)

print(whichgender)
print(v_params_init)
print(m_calib_res)
print(v_GOF)

# Data visualization ------------------------------------------------------

## Figures for initiation and cessation
## Initiation 
p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),  rep(0,74))

## Cessation
## scale and calibrate cessation probabilities p.CF 
p.CF = smk_cess*c(rep(0,18),rep(v_params["s.CF_18.25"],8), rep(v_params["s.CF_26.34"],9),rep(v_params["s.CF_35.49"],15),rep(v_params["s.CF_50.64"],15),rep(v_params["s.CF_65.99"],35))

p.NCsmk_init <- as.data.frame(cbind(c(p.NC[,100],smk_init[,100]),c(rep("calibrated",100),rep("CISNET",100)),c(rep(0:99,2))))
names(p.NCsmk_init) <- c("prob","inputs","age")
p.NCsmk_init$prob<-as.numeric(as.character(p.NCsmk_init$prob))
p.NCsmk_init$age<-as.numeric(as.character(p.NCsmk_init$age))
p.NC_age <- ggplot(data=p.NCsmk_init) +  geom_line( aes(x=age, y=prob, linetype=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Initiation probabilities")

p.CFsmk_cess <- as.data.frame(cbind(c(p.CF[,100],smk_cess[,100]),c(rep("calibrated",100),rep("CISNET",100)),c(rep(0:99,2))))
names(p.CFsmk_cess) <- c("prob","inputs","age")
p.CFsmk_cess$prob<-as.numeric(as.character(p.CFsmk_cess$prob))
p.CFsmk_cess$age<-as.numeric(as.character(p.CFsmk_cess$age))
p.CF_age <- ggplot(data=p.CFsmk_cess) +  geom_line( aes(x=age, y=prob, linetype=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Cessation probabilities")

modelprev <- rbind(cbind(data.frame(model_res$N),status="neversmoker"),
                   cbind(data.frame(model_res$C),status="currentsmoker"),
                   cbind(data.frame(model_res$F),status="formersmoker"))

calibtargets = rbind(cbind(data.frame(lst_smktargets[["N"]]),status="neversmoker"),
                     cbind(data.frame(lst_smktargets[["C"]]),status="currentsmoker"),
                     cbind(data.frame(lst_smktargets[["F"]]),status="formersmoker"))
calibtargets$agegroup <-calibtargets$age
calibtargets$year <-calibtargets$survey_year

ns_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="neversmoker"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="neversmoker" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Never smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())
 
cs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmoker"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmoker" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Current smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmoker"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmoker" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Former smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99), 
                  aes(x = year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, agegroup==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

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

pdf(file = paste0(whichgender,"_smk_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I:%M%p"),".pdf"),width=10, height=6,onefile = TRUE)

plot.new()
text(.5, 1.0, "Calibration parameters - smk_microsim", font=2, cex=1.5)
grid.table(c(v_params,v_GOF, fit_value),rows=c(names(v_params),names(lst_smktargets),"Overall Fit"))
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"")
ncf_total
dev.off()

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

# lst_smktargets18.99<-lapply(lst_smktargets, function(x) subset(x, x[,1]==18.99)) # remove prevalences for ages 18-99
# lst_smktargets<-lapply(lst_smktargets, function(x) subset(x, x[,1]!=18.99)) # remove prevalences for ages 18-99
# p.NC[p.NC>0.65]<-0.65 # all transition probabilities must be positive. (1-p.NX[t] - p.NC[t]) ==> 1- max(p.NX) - p.NC >0. max(p.NX[0:99,]) = 0.3457545 ==> , so max value for p.NC is 0.65
# p.CF[p.CF>0.32]<-0.32 # all transition probabilities must be positive. (1-p.CX[t]- p.CF[t]) ==> 1-max(p.CX)-p.CF > 0. max(p.CX[0:99,])=0.67 ==> so max value for p.CF is 0.32

