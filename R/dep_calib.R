rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(splines)
library(openxlsx)
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)
setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim"))
here::i_am("R/dep_calib.R")

## INPUTS 
whichgender ="females"
load(paste0(here("data/dep_precomputed_inputs_"),whichgender,".RData")) # p.CX, p.FX, p.NX, smk_cess, smk_init
cohorts  <- 1990:2100
n.i   <- 10                    # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100
v.n   <- c( "H","D","R","U","X") 
n.s   <- length(v.n)
v.M_1 <- rep("H", n.i) 

# parameters for calibration
# p.HD[,91:201] = 2.3823137*p.HD[,90] # for birth cohorts born 1990-2100, scale up incidence probabilities by inc_SF = 2.3823137
p.DX = c(rep(1.71,99),1)*p.HX
p.RX = c(rep(1.50,99),1)*p.HX
p.UX = p.RX
# p.RU = c(rep(0,25),rep(0.152,9),rep(0.101,15),rep(0.120,15),rep(0.923,35),0) # probability to Recall Error (U) when Former MD (R)
# v_params = c(1.71,1.50,0.152,0.101,0.120,0.7)
hr.D # hazard ratio of death in D vs H
hr.R # hazard ratio of death in R vs H
p.RU.26to34
p.RU.35to49
p.RU.50to64
p.RU.65to99


## CALIBRATION TARGETS
load(paste0(here("data/dep_calib_targets_"),whichgender,".RData")) #lst_smktargets

# Calibrate to 2005-2015 survey data, remove 2016-2020 survey data
lst_deptargets$H <- subset(lst_deptargets$H,lst_deptargets$H[,"survey_year"]<=2015)
lst_deptargets$D <- subset(lst_deptargets$D,lst_deptargets$D[,"survey_year"]<=2015)
lst_deptargets$R <- subset(lst_deptargets$R,lst_deptargets$R[,"survey_year"]<=2015)
lst_deptargets$E <- subset(lst_deptargets$E,lst_deptargets$E[,"survey_year"]<=2015)


## MODEL FUNCTIONS
source("R/dep_microsim.R", echo = FALSE) # microsimulation model and probability functions


## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
  
  t_init <- Sys.time() # Start timer

  p.DX = v_params[1]*p.HX
  p.RX = v_params[2]*p.HX
  p.UX = p.RX
  p.RU = c(rep(0,25),rep(v_params[3],9),rep(v_params[4],15),rep(v_params[5],15),rep(v_params[6],35),0) # probability to Recall Error (U) when Former MD (R)
  
  # Simulate for each birth cohort with parallelization
  m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                          .export=c('dep_microsim','dep_probs','get_prevs', 
                                    'n.i','n.t','v.n','n.s','v.M_1',
                                    'p.HX','p.DX','p.RX','p.UX', 'p.HD', 'p.DR', 'p.RD','p.UD','p.RU')) %dopar%
    {
      dep_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
    }
  
  ### Serial:
  # m.cohortbyage <- do.call(rbind, lapply(cohorts, function(i) { dep_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Convert matrix from cohort-age to cohort-year
  m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = 301)
  for (b in 1:length(cohorts)){
    m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
  }
  colnames(m.cohortbyyear) <- c(1900:2200)
  
  # Output prevalence results as a list
  model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=2015) # calibrate to survey years 2005-2015
  names(model_res) <- v.n
  model_res$H <- model_res$H[order(model_res$H[,"agegroup"],decreasing=FALSE),]
  model_res$D <- model_res$D[order(model_res$D[,"agegroup"],decreasing=FALSE),]
  model_res$R <- model_res$R[order(model_res$R[,"agegroup"],decreasing=FALSE),]
  model_res$U <- model_res$U[order(model_res$U[,"agegroup"],decreasing=FALSE),]
  
  
  cat("Time: ")
  print(Sys.time() - t_init) # End timer
  return(model_res)
}

model_res<-main(v_params)

# save(model_res, file=paste0(here("model_res_n",n.i,"_",whichgender,"_052422.Rdata")))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n_init <- 6

# names and number of input parameters to be calibrated
v_param_names <- c("hr.D", "hr.R", "p.RU.26to34", "p.RU.35to49", "p.RU.50to64", "p.RU.65to99")
n_param <- length(v_param_names)

# range on input search space
lb <- c(1,1,0,0,0,0) # lower bound
ub <- c(5,3,0.2,0.2,0.7) # upper bound

# number of calibration targets
v_target_names <- names(lst_smktargets)
n_target <- length(v_target_names)

v_params = c(1.71,1.50,0.152,0.101,0.120,0.7)

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){

  # Run model for parameter set "v_params"
  model_res <- main(v_params)

  # Calculate goodness-of-fit of model outputs to targets
  v_GOF <- numeric(n_target)
  
  v_GOF[1] <- sum((lst_deptargets[["D"]][,"prev"] - model_res[["D"]][,"prev"])^2) # Current MD
  v_GOF[2] <- sum((lst_deptargets[["H"]][,"prev"] - (model_res[["H"]][,"prev"] + model_res[["U"]][,"prev"]))^2) # Never MD (includes recall error)
  v_GOF[3] <- sum((lst_deptargets[["E"]][,"prev"] - (model_res[["D"]][,"prev"] + model_res[["R"]][,"prev"]))^2) # Former MD (excludes recall error)
  
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

  fit_nm <- optim(v_params, f_gof,control = list(fnscale = 1, maxit = 1000), hessian = T)

}

# Calculate computation time
comp_time <- Sys.time() - t_init
# 
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

# Data visualization ------------------------------------------------------
library(ggplot2)

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


ncf_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, agegroup==18.99), 
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

pdf(file = "Figs_Calib_052422.pdf",width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 0.9, "Calibration parameters - smk_microsim", font=2, cex=1.5)
grid.table(v_params,rows=names(v_params))
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
