rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(splines)
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)
setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim"))
here::i_am("R/smk_calib.R")

## INPUTS
whichgender <- "females"
n.i   <- 100                    # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 
cohorts <- 1900:2100
load(paste0(here("data/smk_precomputed_inputs_"),whichgender,".RData")) # p.CX, p.FX, p.NX, smk_cess, smk_init

## Parameters for calibration
v_params_names <- c("s.NC_0.17", "s.NC_18.25", "s.NC_26.34","s.CF_35.49","s.CF_50.64","s.CF_65.99")
n_param <- length(v_params_names)
s.NC_0.17 = 1 # scaling factor for smoking initiation ages 0-17
s.NC_18.25 = 1
s.NC_26.34 = 1
s.CF_35.49 = 1 # scaling factor for smoking cessation ages 35-49
s.CF_50.64 = 1
s.CF_65.99 = 1
v_params <- c(s.NC_0.17, s.NC_18.25, s.NC_26.34,s.CF_35.49,s.CF_50.64,s.CF_65.99)

## CALIBRATION TARGETS
load(paste0(here("data/smk_calib_targets_"),whichgender,".RData")) #lst_smktargets																			  
lst_smktargets18.99<-lapply(lst_smktargets, function(x) subset(x, x[,1]==18.99)) # remove prevalences for ages 18-99
lst_smktargets<-lapply(lst_smktargets, function(x) subset(x, x[,1]!=18.99)) # remove prevalences for ages 18-99

## MODEL FUNCTIONS
source("R/smk_microsim.R", echo = FALSE) # microsimulation model and probability functions

## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
    
    ## v_params 1-6: initiation, 7-11: cessation

    ## scale and calibrate initiation probabilities p.NC 
    p.NC = smk_init*c(rep(v_params[1],18),rep(v_params[2],8),rep(v_params[3],9), rep(1,65))
    ## scale and calibrate cessation probabilities p.CF 
    p.CF = smk_cess*c(rep(1,35),rep(v_params[4],15),rep(v_params[5],15),rep(v_params[6],35))
    p.NC[p.NC>0.65]<-0.65 # all transition probabilities must be positive. (1-p.NX[t] - p.NC[t]) ==> 1- max(p.NX) - p.NC >0. max(p.NX[0:99,]) = 0.3457545 ==> , so max value for p.NC is 0.65
    p.CF[p.CF>0.32]<-0.32 # all transition probabilities must be positive. (1-p.CX[t]- p.CF[t]) ==> 1-max(p.CX)-p.CF > 0. max(p.CX[0:99,])=0.67 ==> so max value for p.CF is 0.32
    
    t_init <- Sys.time() # Start timer
    
    # Simulate for each birth cohort with parallelization
    m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                            .export=c('smk_microsim','smk_probs','get_prevs', 
                                      'n.i','n.t','v.n','n.s','v.M_1',
                                      'p.NC','p.CF','p.NX','p.CX','p.FX')) %dopar%
        {
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
    model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=2020)
    names(model_res) <- v.n
    model_res$N <- model_res$N[order(model_res$N[,"agegroup"],decreasing=FALSE),]
    model_res$C <- model_res$C[order(model_res$C[,"agegroup"],decreasing=FALSE),]
    model_res$F <- model_res$F[order(model_res$F[,"agegroup"],decreasing=FALSE),]
    
    cat(paste0("  ", v_params_names,": ", v_params," "))
    print(Sys.time() - t_init) # End timer
    return(model_res)
}

model_res<-main(v_params)

# save(model_res, file=paste0(here("model_res_n",n.i,"_",whichgender,"_052422.Rdata")))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n_init <- 2

# names and number of input parameters to be calibrated
v_params_names <- c("s.NC_0.17", "s.NC_18.25", "s.NC_26.34","s.CF_35.49","s.CF_50.64","s.CF_65.99")
n_param <- length(v_param_names)

# range on input search space
lb <- c(0.01,0.01,0.01,0.01,0.01,0.01) # lower bound
ub <- c(5,5,5,5,5,5) # upper bound

# number of calibration targets
v_target_names <- names(lst_smktargets)
n_target <- length(v_target_names)

v_params <- c(1,1,1,1,1,1)

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){

  # Run model for parameter set "v_params"
  model_res <- main(v_params)
  
  model_res<-lapply(model_res, function(x) subset(x, x[,1]!=18.99)) # remove prevalences for ages 18-99
  model_res <- model_res[-4] # remove 'X' dead population
  
  # Calculate goodness-of-fit of model outputs to targets
  v_GOF <- numeric(n_target)

  for (r in 1:length(lst_smktargets)){ # sum of squared differences
    v_GOF[r] <- sum((lst_smktargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2)
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
  v_params_init[,i] <- 1 #runif(n_init,min=lb[i],max=ub[i]) # This should probably be LHS to cover parameter space evenly
}
colnames(v_params_init) <- v_params_names

# record start time of calibration
t_init <- Sys.time()

###  Run Nelder-Mead for each starting point  ###
m_calib_res <- matrix(nrow = n_init, ncol = n_param+1)
colnames(m_calib_res) <- c(v_params_names, "Overall_fit")
for (j in 1:n_init){ # j <- 1

  # use optim() as Nelder-Mead, default is minimization
  fit_nm <- optim(v_params_init[j,], f_gof, hessian = T)
  m_calib_res[j,] <- c(fit_nm$par, fit_nm$value)

  fit_nm <- optim(v_params, f_gof,control = list(fnscale = 1, maxit = 1000), hessian = T)

}

# Calculate computation time
comp_time <- Sys.time() - t_init

save(m_calib_res,file=paste0("smk_m_calib_res_",whichgender,n.i,".Rda"))
# ####################################################################
# ######  Exploring best-fitting input sets  ######
# ####################################################################
# 
# # Arrange parameter sets in order of fit
# m_calib_res <- m_calib_res[order(-m_calib_res[,"Overall_fit"]),]
# 
# # Examine the top 10 best-fitting sets
# m_calib_res[1:10,]
# 
# # Plot the top 10 (top 10%)
# plot(m_calib_res[1:10,1],m_calib_res[1:10,2],
#      xlim=c(lb[1],ub[1]),ylim=c(lb[2],ub[2]),
#      xlab = colnames(m_calib_res)[1],ylab = colnames(m_calib_res)[2])
# 
# # Pairwise comparison of top 10 sets
# pairs.panels(m_calib_res[1:10,v_param_names])
# 

v_params = m_calib_res[1,-7]
model_res<-main(v_params)

# Data visualization ------------------------------------------------------
library(ggplot2)

## Figures for initiation and cessation
p.NC = smk_init*c(rep(v_params[1],18),rep(v_params[2],8),rep(v_params[3],9), rep(1,65))
p.CF = smk_cess*c(rep(1,35),rep(v_params[4],15),rep(v_params[5],15),rep(v_params[6],35))
p.NC[p.NC>0.65]<-0.65 # all transition probabilities must be positive. (1-p.NX[t] - p.NC[t]) ==> 1- max(p.NX) - p.NC >0. max(p.NX[0:99,]) = 0.3457545 ==> , so max value for p.NC is 0.65
p.CF[p.CF>0.32]<-0.32 # all transition probabilities must be positive. (1-p.CX[t]- p.CF[t]) ==> 1-max(p.CX)-p.CF > 0. max(p.CX[0:99,])=0.67 ==> so max value for p.CF is 0.32

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

##
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
  labs(title="Never smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmoker"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmoker" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmoker"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmoker" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

calibtargets18.99 = rbind(cbind(data.frame(lst_smktargets18.99[["N"]]),status="neversmoker"),
                     cbind(data.frame(lst_smktargets18.99[["C"]]),status="currentsmoker"),
                     cbind(data.frame(lst_smktargets18.99[["F"]]),status="formersmoker"))
calibtargets18.99$agegroup <-calibtargets18.99$age
calibtargets18.99$year <-calibtargets18.99$survey_year

ncf_total <- ggplot() +
  geom_pointrange(data=calibtargets18.99, 
                  aes(x = year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, agegroup==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Smoking distribution - Women, ages 18-99")+
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

pdf(file = "smk_calib_093022.pdf",width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 0.9, "Calibration parameters - smk_microsim", font=2, cex=1.5)
grid.table(c(v_params),rows=c(v_params_names))
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