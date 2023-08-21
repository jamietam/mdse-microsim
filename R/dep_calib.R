rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(lbfgsb3c)
library(splines)
library(foreach) # parallelization is in the foreach loop
library(ggplot2)
library(gridBase)
library(gridExtra)
library(grid)
setwd(file.path("/gpfs/gibbs/project/tam_jamie/shared/mds-microsim/"))
here::i_am("R/dep_calib.R")

####### For HPC runs ###########################################################
n_cores = Sys.getenv("SLURM_CPUS_PER_TASK")
cl <- makeCluster(as.numeric(n_cores),type="FORK")

####### For Personal Computer and Open On Demand Interface runs ################
# cl <- makeCluster(detectCores())
registerDoParallel(cl)

## INPUTS 
whichgender <- "males"
load(paste0(here("data/dep_precomputed_inputs_"),whichgender,".RData")) 
cohorts <- 1900:2020
n.i   <- 1000                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100
v.n   <- c( "H","D","R","X") 
n.s   <- length(v.n)
v.M_1 <- rep("H", n.i) 

## CALIBRATION PARAMETERS
# v_params = c("hr.D_18.25"=1.31210596091114,"hr.D_26.34"=1.89825790957548,"hr.D_35.49"=1.04619248825327,"hr.D_50.64"=3.88783711846918,"hr.D_65.99"=5.83391820097817) #best fit females 0.0104712904454185
v_params = c("hr.D_18.25"=1.31210596091114,"hr.D_26.34"=1.89825790957548,"hr.D_35.49"=1.04609939386137,"hr.D_50.64"=3.88783711846918,"hr.D_65.99"=5.82027339516208) #best fit males 0.00746310659342863
n_param <- length(v_params)
set.seed(072218) # Specify seed (for reproducible sequence of random numbers)
n_init <- 20 # number of initial starting points
# range on input search space ## needs to match the number of params
lb <- c(1,1,1,1,1) # lower bound
ub <- c(6,6,6,6,6) # upper bound

## CALIBRATION TARGETS
load(paste0(here("data/dep_calib_targets_"),whichgender,".RData")) #lst_smktargets
lst_deptargetsD <- as.data.frame(lst_deptargets[["D"]])
lst_deptargetsD <- as.matrix(lst_deptargetsD)
v_target_names <- names(lst_deptargets[2]) # number of calibration targets
n_target <- length(v_target_names)

## MODEL FUNCTIONS
source("R/dep_microsim.R", echo = FALSE) # microsimulation model and probability functions

## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration
  
  t_init <- Sys.time() # Start timer
  
  ## Incidence
  # for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
  #   bc1 = bc-1899
  #   for (age in 0:25){ # increase applies to youth and young adults ages 0-25
  #     if ((bc+age)>=2016 & age>=18){ # starting in 2016
  #       p.HD[(age+1),bc1] = v_params["incSF_18.25"]*p.HD[(age+1),bc1]
  #     }
  #     if ((bc+age)>=2016 & age<18){
  #       p.HD[(age+1),bc1] = v_params["incSF_12.17"]*p.HD[(age+1),bc1]
  #     }
  #   }
  # }
   
  ## Recovery 
  # p.DR[1:12]= p.DR[100] = 0
  # p.DR[13:99] = 0.173 # assumes constant recovery by age
  
  ## Mortality
  # p.DX = c(rep(1,18),rep(1.710,82))*p.HX
  p.DX = c(rep(1,18),rep(v_params["hr.D_18.25"],8),rep(v_params["hr.D_26.34"],9),rep(v_params["hr.D_35.49"],15),rep(v_params["hr.D_50.64"],15),rep(v_params["hr.D_65.99"],35))*p.HX
  p.RX = p.HX
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
  model_res <- lapply(v.n, get_prevs, m.cohortbyyear=m.cohortbyyear, minyear=2005, maxyear=max(cohorts))
  names(model_res) <- v.n
  model_res$H <- model_res$H[order(model_res$H[,"agegroup"],decreasing=FALSE),]
  model_res$D <- model_res$D[order(model_res$D[,"agegroup"],decreasing=FALSE),]
  model_res$R <- model_res$R[order(model_res$R[,"agegroup"],decreasing=FALSE),]
  
  cat(paste0("\n  ", v_params," "))
  print(Sys.time() - t_init) # End timer
  return (model_res)
}

## CALIBRATION FUNCTIONS
f_gof <- function(v_params){ # Write goodness-of-fit function to pass to algorithm
  
  model_res <- main(v_params) # Run model for parameter set "v_params"
  v_GOF <- numeric(n_target)
  v_GOF[1] <- sum((lst_deptargetsD[,"prev"] - model_res[["D"]][,"prev"])^2) # Current MD # Calculate goodness-of-fit of model outputs to targets
  
  v_weights <- rep(1,n_target)   # can give different targets different weights
  GOF_overall <- sum(v_GOF[1:n_target] * v_weights) # weighted sum
  
  cat(GOF_overall)
  return(GOF_overall)
}

######  Calibrate!  ############################################################
# Sample multiple random starting values 
v_params_init <- matrix(nrow=n_init,ncol=n_param)
for (i in 1:n_param){
  v_params_init[,i] <- runif(n_init,min=lb[i],max=ub[i]) # This should probably be LHS to cover parameter space evenly
}
colnames(v_params_init) <- names(v_params)

t_init <- Sys.time() # record start time of calibration

# Run optimization algorithm for each starting point
m_calib_res <- matrix(nrow = n_init, ncol = n_param+1)
colnames(m_calib_res) <- c(names(v_params), "Overall_fit")
for (j in 1:n_init){ # j <- 1
  
  # Use optim() as box-constraint method with upper and lower bounds. Default is minimization.
  # fit_nm <- optim(v_params_init[j,], f_gof, hessian = T, method="L-BFGS-B", lower=lb, upper=ub)
  
  # Use L-BFGS-B package for box-constraint optimization method
  fit_nm <- lbfgsb3c(par = v_params_init[j,], fn = f_gof, lower=lb, upper=ub)
  
  m_calib_res[j,] <- c(fit_nm$par, fit_nm$value)
  
}

# Calculate computation time
comp_time <- Sys.time() - t_init
 
# Arrange parameter sets in order of fit
m_calib_res <- m_calib_res[order(m_calib_res[,"Overall_fit"]),]
print(m_calib_res)

v_params = m_calib_res[1,1:length(v_params)]

# Run the model using best fit calibrated parameters                
model_res<-main(v_params) 
fit_value <- sum((lst_deptargetsD[,"prev"] - model_res[["D"]][,"prev"])^2) 

####### Data visualization #####################################################

## Figures for mortality by MDE status
p.DX = c(rep(1,18),rep(v_params["hr.D_18.25"],8),rep(v_params["hr.D_26.34"],9),rep(v_params["hr.D_35.49"],15),rep(v_params["hr.D_50.64"],15),rep(v_params["hr.D_65.99"],35))*p.HX
p.RX = p.HX
p.DX[100,] = p.RX[100,] = 1 # everyone dies at age 99
p.HDRX <- as.data.frame(cbind(c(p.HX[,100],p.DX[,100],p.RX[,100]),c(rep("HX",100),rep("DX",100),rep("RX",100)),c(rep(0:99,3))))
names(p.HDRX) <- c("prob","status","age")
p.HDRX$prob <- as.numeric(p.HDRX$prob)
p.HDRX$age <- as.numeric(p.HDRX$age)
p.HDRX_age <- ggplot(data=p.HDRX) +  geom_line( aes(x=age, y=prob, color=status)) + 
  scale_y_continuous(name="Annual mortality - Never MDE (p.DX)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Mortality probabilities by MDE status")

## Figure for incidence inputs
p.HD_age <- ggplot() + geom_line(aes(x=0:99,y=p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD)")+#, limits=c(0,0.1), breaks=seq(0,0.1,0.01)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence, calibrated estimates",color=NULL)

## Figure for recovery inputs
p.DR_age <- ggplot() +  geom_line( aes(x=0:99, y=p.DR)) + 
  scale_y_continuous(name="Probability of recovery (p.DR)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates")

## Figure for recurrence inputs
p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD)) + 
  scale_y_continuous(name="Probability of recurrence (p.RD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates")

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
  scale_y_continuous(name="Prevalence (%)",limits=c(0.6,1),breaks=seq(0.6,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Never MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="D"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="D" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

R_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="R"&agegroup!=18.99), aes(x = year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(agegroup), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="R" & agegroup!=18.99),  aes(x=year, y= prev, colour=factor(agegroup)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Former MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

HDR_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, agegroup==18.99), 
                  aes(x = year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, agegroup==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
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

pdf(file = paste0(whichgender,"_dep_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I:%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 1.0, "Calibration parameters - dep_microsim", font=2, cex=1.5)
grid.table(c(v_params,fit_value),rows=c(names(v_params),"Overall Fit"))
p.HD_age
grid.arrange(p.DR_age,p.RD_age,ncol=2)
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
# v_params = c("depinc1"=4.832766251,"depinc2"=-6.513158237, "depinc3"=3.289480518) # best fit females
# v_params = c("depinc1"=3.10610469809184,"depinc2"=8.34323146575445, "depinc3"=0.160023210647931) # best fit males

# if (whichgender=="females"){
#   MP=ns(0:21,knots=c(13,18)) ### Matrix of X's
#   Rps=predict(MP,21)[1,] ## Predicts y value given a set of X's for age 22  # 0.0051285304 anchor at age 22
#   y=c()
#   for (j in 0:21){
#     y=c(y,0.0051285304*exp(sum((MP[j,]-Rps)*c(v_params["depinc1"],v_params["depinc2"],v_params["depinc3"])))) ## multiply by coefficients and sum , exp makes it positive for incidence
#   }
#   p.HD[0:22,]<-y
#   p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
# }
# 
# if (whichgender=="males"){
#   MP=ns(0:28,knots=c(13,18))
#   Rps=predict(MP,28)[1,] ## Predicts y value given a set of X's for age 22  # 0.0019072600 anchor at age 29
#   y=c()
#   for (j in 0:28){
#     y=c(y,0.0019072600*exp(sum((MP[j,]-Rps)*c(v_params["depinc1"],v_params["depinc2"],v_params["depinc3"])))) ## multiply by coefficients and sum , exp makes it positive for incidence
#   }
#   p.HD[0:29,]<-y
#   p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
# }

# scale up incidence by cohort
# p.HD[,floor(v_params["bc"]-1900):201] = v_params["incSF.bc"]*p.HD[,floor(v_params["bc"]-1900)]   

# scale up incidence by year (p.HD IS ALREADY IN AGE COHORT FORMAT)
# for (bc in floor(v_params["bc"]):max(cohorts)){
#   diag(p.HD[,(bc-1900):201]) = v_params["incSF.bc"]*diag(p.HD[,(bc-1900):201])
# } 
# v_params_init[1,] = v_params # replace first row of initial starting points
# lst_deptargetsD <- subset(lst_deptargetsD, survey_year<2016) ## COMMENT OR DELETE THIS LINE WHEN READY TO CALIBRATE 2016-2020
