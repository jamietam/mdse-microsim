rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(lbfgsb3c)
library(splines)
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)
setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim"))
here::i_am("R/dep_calib.R")

## INPUTS 
whichgender ="females"
load(paste0(here("data/dep_precomputed_inputs_"),whichgender,".RData")) 
cohorts  <- 1900:2020
n.i   <- 100                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100
v.n   <- c( "H","D","R","X") 
n.s   <- length(v.n)
v.M_1 <- rep("H", n.i) 

# parameters for calibration
v_params_names <- c("hr.D", "hr.R", "p.RD_13.25","p.RD_26.34", "p.RD_35.49", "p.RD_50.64","p.RD_65.99")
n_param <- length(v_params_names)
v_params = c(1,1,0.058,0.058, 0.058, 0.058,0.058)
# inc_SF.bc = 1995
# p.HD_SF = 4 # increased incidence of 1st MD episode starting with the 1990 birth cohort

## CALIBRATION TARGETS
load(paste0(here("data/dep_calib_targets_"),whichgender,".RData")) #lst_smktargets
lst_deptargetsD <- as.data.frame(lst_deptargets[["D"]])
lst_deptargetsD <- subset(lst_deptargetsD, survey_year<2016) ## COMMENT OR DELETE THIS LINE WHEN READY TO CALIBRATE 2016-2020
lst_deptargetsD <- as.matrix(lst_deptargetsD)

## MODEL FUNCTIONS
source("R/dep_microsim.R", echo = FALSE) # microsimulation model and probability functions


## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
  
  t_init <- Sys.time() # Start timer
  
  p.DX = c(rep(v_params[1],99),1)*p.HX
  p.RX = c(rep(v_params[2],99),1)*p.HX
  
  #p.HD = v_params[4]*p.HD # p.HD_SF
  
  #v_params[5] = inc_SF.bc
  #v_params_names[5] <- "inc_SF.bc"
  
  # p.HD = v_params[3]*p.HD
  # p.HD[,round(v_params[5]-1900):201] = v_params[3]*p.HD[,round(v_params[5]-1900)] # for birth cohorts born 1990-2100, scale up incidence probabilities by inc_SF = 2.3823137
  p.RD[1:12]= p.RD[100] = 0
  p.RD[13:26] = v_params[3]
  p.RD[27:36] = v_params[4]
  p.RD[37:50] = v_params[5]
  p.RD[51:65] = v_params[6]
  p.RD[66:99] = v_params[7]
  
  # Replace parameter values that lead to negative transition probabilities - JAMIE WILL REVISE THIS CODE
  # H to H: (1 - p.HX[t,bc1] - p.HD[t,bc1])
  # D to D: (1 - p.DX[t,bc1] - p.DR[t])
  # R to R: (1 - p.RX[t,bc1] - p.RD[t])
  p.DX[100,] = p.RX[100,] = 1 # everyone dies at age 99
  
  # Simulate for each birth cohort with parallelization
  m.cohortbyage<-foreach (i=cohorts, .combine='rbind',
                          .export=c('dep_microsim','dep_probs','get_prevs',
                                    'n.i','n.t','v.n','n.s','v.M_1',
                                    'p.HX','p.DX','p.RX', 'p.HD', 'p.DR', 'p.RD')) %dopar%
    {
      dep_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
    }
  
  ### Serial:
  # m.cohortbyage <- do.call(rbind, lapply(cohorts, function(i) { dep_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))
  
  # Convert matrix from cohort-age to cohort-year
  m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = (length(cohorts)+100))
  for (b in 1:length(cohorts)){
    m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
  }
  colnames(m.cohortbyyear) <- c(min(cohorts):(max(cohorts)+100))
  
  # Output prevalence results as a list
  model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=2015) # calibrate to survey years 2005-2020  ## ADJUST WHEN READY TO CALIBRATE 2016-2020
  names(model_res) <- v.n
  model_res$H <- model_res$H[order(model_res$H[,"agegroup"],decreasing=FALSE),]
  model_res$D <- model_res$D[order(model_res$D[,"agegroup"],decreasing=FALSE),]
  model_res$R <- model_res$R[order(model_res$R[,"agegroup"],decreasing=FALSE),]
  
  cat(paste0("\n  ", v_params_names,": ", v_params," "))
  print(Sys.time() - t_init) # End timer
  return (model_res)
}

model_res<-main(v_params)

# save(model_res, file=paste0(here("model_res_n",n.i,"_",whichgender,"_052422.Rdata")))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n_init <- 1

# range on input search space
lb <- c(1,1,0,0,0,0,0) # lower bound
ub <- c(7,7,1,1,1,1,1) # upper bound

# number of calibration targets
v_target_names <- names(lst_deptargets[2])
n_target <- length(v_target_names)

## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){
  
  # Run model for parameter set "v_params"
  model_res <- main(v_params)
  
  # Calculate goodness-of-fit of model outputs to targets
  v_GOF <- numeric(n_target)
  
  v_GOF[1] <- sum((lst_deptargetsD[,"prev"] - model_res[["D"]][,"prev"])^2) # Current MD
  
  # OVERALL
  # can give different targets different weights
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
colnames(v_params_init) <- v_params_names

# record start time of calibration
t_init <- Sys.time()

###  Run optimization algorithm for each starting point  ###
m_calib_res <- matrix(nrow = n_init, ncol = n_param+1)
colnames(m_calib_res) <- c(v_params_names, "Overall_fit")
for (j in 1:n_init){ # j <- 1
  
  # Use optim() as box-constraint method with upper and lower bounds. Default is minimization.
  # fit_nm <- optim(v_params_init[j,], f_gof, hessian = T, method="L-BFGS-B", lower=lb, upper=ub)
  
  # Use L-BFGS-B package for box-constraint optimization method
  fit_nm <- lbfgsb3c(par = v_params_init[j,], fn = f_gof, lower=lb, upper=ub)

  m_calib_res[j,] <- c(fit_nm$par, fit_nm$value)
  
}

# Calculate computation time
comp_time <- Sys.time() - t_init
# 
# ####################################################################
# ######  Exploring best-fitting input sets  ######
# ####################################################################
# 
# # Arrange parameter sets in order of fit
m_calib_res <- m_calib_res[order(m_calib_res[,"Overall_fit"]),]
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
# pairs.panels(m_calib_res[1:10,v_params_names])
# 
v_params_new = m_calib_res[1:7]
fit_value <- m_calib_res[8]

model_res<-main(v_params_new)
model_res<-main(m_calib_res[1,1:8])
# Data visualization ------------------------------------------------------
library(ggplot2)

## Figures for mortality by MDE status
p.DX = c(rep(v_params_new[1],99),1)*p.HX
p.RX = c(rep(v_params_new[2],99),1)*p.HX
p.DX[100,] = p.RX[100,] = 1 # everyone dies at age 99
p.HDRX <- as.data.frame(cbind(c(p.HX[,100],p.DX[,100],p.RX[,100]),c(rep("HX",100),rep("DX",100),rep("RX",100)),c(rep(0:99,3))))
names(p.HDRX) <- c("prob","status","age")
p.HDRX$prob <- as.numeric(p.HDRX$prob)
p.HDRX$age <- as.numeric(p.HDRX$age)
p.HDRX_age <- ggplot(data=p.HDRX) +  geom_line( aes(x=age, y=prob, color=status)) + 
  scale_y_continuous(name="Annual mortality - Never MDE (p.DX)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Mortality probabilities by MDE status")

## Figure for recurrence inputs
p.RD[1:12]= p.RD[100] = 0
p.RD[13:26] = v_params_new[3]
p.RD[27:36] = v_params_new[4]
p.RD[37:50] = v_params_new[5]
p.RD[51:65] = v_params_new[6]
p.RD[66:99] = v_params_new[7]

p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD)) + 
  scale_y_continuous(name="Probability of recurrence (p.RD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recurrence, calibrated estimates")

modelprev <- rbind(cbind(data.frame(model_res$H),status="H"),
                   cbind(data.frame(model_res$D),status="D"),
                   cbind(data.frame(model_res$R),status="R"))

calibtargets = rbind(cbind(data.frame(lst_deptargets[["H"]]),status="H"),
                     cbind(data.frame(lst_deptargets[["D"]]),status="D"),
                     cbind(data.frame(lst_deptargets[["R"]]),status="R"))
calibtargets$agegroup <-calibtargets$age
calibtargets$year <-calibtargets$survey_year

H_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="H"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="H" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Never MDE - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="D"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="D" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current MDE - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

R_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="R"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="R" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former MDE - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


HDR_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, agegroup==18.99), 
                  aes(x = year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, agegroup==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="MDE distribution - Women, ages 18-99")+
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

pdf(file = "dep_calib_p.HD_102622-lbfgsb.pdf",width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 0.9, "Calibration parameters - dep_microsim", font=2, cex=1.5)
grid.table(c(m_calib_res[1:8]),rows=c(v_params_names,"Overall Fit"))
p.RD_age
p.HDRX_age
grid_arrange_shared_legend(list(H_age, D_age, R_age),3,"")
HDR_total
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