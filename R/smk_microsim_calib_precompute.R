rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(splines)
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)
setwd(file.path("C:/Users/jamietam/Dropbox/GitHub/mds-microsim"))
here::i_am("R/smk_microsim_calib.R")

#Rprof(filename = here("test_runs/Rprof.out"),memory.profiling = TRUE, gc.profiling = TRUE, line.profiling = TRUE)

# mainDir <- file.path(here("."))
# setwd(file.path(mainDir))

## Inputs
whichgender <- "females"
n.i   <- 100                    # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years 
v.n   <- c( "N","C","F","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), Dead (X)
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("N", n.i)          # everyone begins in the Never smoker state  # v.M_1:   vector of initial states for individuals 
cohorts <- 1900:2100

load(paste0(here("data/smk_inputs_"),whichgender,".RData")) # Load all smoking and mortality inputs as matrices
precompute_diag <- function(statdata, cohorts, finval) {
  precomp = matrix(nrow=100, ncol=201)
  for (bc in cohorts) {
    ## Transition probabilities (per cycle) by birth cohort
    bc_i = bc - 1899
    precomp[1:min(100,202-bc_i),bc_i] <- diag(statdata[,bc_i:201])
    precomp[100,bc_i] <- finval
  }
  return(precomp)
}
smk_init <- precompute_diag(smk_init_cisnet,cohorts[-length(cohorts)],0) # probability to become Current smoker when Never smoker
smk_cess <- precompute_diag(smk_cess_cisnet,cohorts[-length(cohorts)],0) # probability to become Former smoker when Current smoker
p.NX <- precompute_diag(death_ns,cohorts[-length(cohorts)],1) # probability to die when Never smoker
p.CX <- precompute_diag(death_cs,cohorts[-length(cohorts)],1) # probability to die when Current smoker
p.FX <- precompute_diag(death_fs,cohorts[-length(cohorts)],1) # probability to die when Former smoker
p.CX[,121:201] <- p.CX[,120] # hold mortality rates constant from 2020 onwards
p.NX[,121:201] <- p.NX[,120] 
p.FX[,121:201] <- p.FX[,120] 

smk_init <- as.data.frame(smk_init)
smk_init[is.na(smk_init)] <- 0
smk_init <- as.matrix(smk_init)

smk_cess <- as.data.frame(smk_cess)
smk_cess[is.na(smk_cess)] <- 0
smk_cess <- as.matrix(smk_cess)

p.NX <- as.data.frame(p.NX)
p.NX[is.na(p.NX)] <- 0
p.NX <- as.matrix(p.NX)

p.CX <- as.data.frame(p.CX)
p.CX[is.na(p.CX)] <- 0
p.CX <- as.matrix(p.CX)

p.FX <- as.data.frame(p.FX)
p.FX[is.na(p.FX)] <- 0
p.FX <- as.matrix(p.FX)

rm(smk_cess_cisnet,smk_init_cisnet,death_cs,death_fs,death_ns)

## Calibration Targets
load(paste0(here("data/smk_calib_targets_"),whichgender,".RData"))

## MICROSIMULATION MODEL FUNCTION
smk_microsim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
  # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- matrix(nrow = n.i, ncol = n.t + 1, 
                dimnames = list(paste(bc, 1:n.i, sep = "."), # each individual, year of birth
                                paste(0:n.t, sep = " ")))  
  m.M[, 1] <- v.M_1                                         # indicate the initial health state   
  
  for (i in 1:n.i) {
    set.seed(seed + i)                                      # set the seed for every individual for the random number generator
    for (t in 1:n.t) {
      if ((bc+t>2100)|(m.M[i, t]=="X")){ # exit for loop if going past the year 2100
        break
      }
      v.p <- smk_probs(bc, t, m.M[i, t])           # calculate the transition probabilities at cycle t 
      m.M[i, t + 1] <- sample(v.n, size=1, prob = v.p)      # sample the next health state and store that state in matrix m.M 
    }                                                       # close the loop for the time points 
    if (i/100 == round(i/100,0)) {                          # display the progress of the simulation
      cat('\r', paste(i/n.i * 100, "% done", sep = " "))
    }
  } # close the loop for the individuals 
  
  if (TS.out == TRUE) {  # create a  matrix of transitions across states
    TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n.i)
    rownames(TS) <- paste(bc, 1:n.i, sep = ".")   # name the rows 
    
    colnames(TS) <- paste(0:n.t, sep = " ")   # name the columns (age)
  } else {
    TS <- NULL
  }
  
  if (TR.out == TRUE) { # create a trace from the individual trajectories
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
    TR <- TR / n.i                                       # create a distribution trace
    rownames(TR) <- paste(bc:(bc+n.t), sep = ".")        # name the rows by birth cohort
    colnames(TR) <- v.n                                  # name the columns 
  } else {
    TR <- NULL
  }
  
  results <- list(m.M = m.M, TS = TS, TR = TR) # store the results from the simulation in a list  
  return(results)  # return the results
} # end of the smk_microsim function  


## PROBABILITY FUNCTION 
smk_probs <- function(bc, t, M_it) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  
  bc1 = bc-1899
    
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # Never
  v.p.it[M_it == "N"] <- 
    c((1-p.NX[t,bc1])*(1 - p.NC[t,bc1]), 
      (1-p.NX[t,bc1])*p.NC[t,bc1], 
      0, 	
      p.NX[t,bc1]) 
  
  v.p.it[M_it == "C"] <- 
    c(0, 
      (1-p.CX[t,bc1])*(1- p.CF[t,bc1]),
      (1-p.CX[t,bc1])*p.CF[t,bc1],
      p.CX[t,bc1]) 
  
  v.p.it[M_it == "F"] <- 
    c(0,
      0,
      (1 - p.FX[t,bc1]),
      p.FX[t,bc1])
  
  v.p.it[M_it == "X"]  <- c(0,0,0, 1)					#X = DEAD
  # return the transition probabilities or produce an error
  ifelse(any(is.na(v.p.it)), print(paste0(paste0(v.p.it,collapse=", ")," - NA probability! bc: ", bc,", age: ",t,", M_it: ",M_it)),return(v.p.it)) 
  ifelse(any(v.p.it<0),print(paste0(paste0(v.p.it,collapse=", ")," - Negative probability! bc: ", bc, ", age: ",t,", M_it: ", M_it)),return(v.p.it))
  ifelse(round(sum(v.p.it),8) == 1, return(v.p.it), print(paste("Probabilities do not sum to 1:", sum(v.p.it), "bc:",bc,"age:",t,"M_it:",M_it))) # rounds off to the eigth digit because otherwise you get 0.000000001 instead of 0
  return(v.p.it) 
}       


## GET MODEL PREVALENCE RESULTS
get_prevs <- function(state,m.cohortbyyear,minyear,maxyear){ # Get counts/prevalence of individuals in a health state by age group and year
  agerownames<-c(18.99,18.25, 26.34, 35.49, 50.64, 65.99)
  agegroupstart <- c(18, 18,26,35,50,65)
  agegroupend <- c(99,25,34,49,64,99,99)
  m.M.prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
      m.M.prevs<-rbind(m.M.prevs,c(agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("agegroup","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}


## RUN THE MODEL FOR ALL BIRTH COHORTS  ---------------------------------
main = function(v_params) { # v_params: run model for parameter calibration

    t_init <- Sys.time() # Start timer
    
    ## Use splines to calibrate initiation probabilities p.NC
    MP = ns(10:100,knots=c(18,30)) ### Matrix of X's
    Rps = predict(MP,10)[1,] ## Predicts y value given a set of X's
    y=c()
    for (j in 10:100){ # smk_init[10,120] = 0.000574301 is initiation for females age 10 in recent cohorts
      y=c(y,smk_init[10,120]*exp(sum((MP[j-10,]-Rps)*c(v_params[1],v_params[2],v_params[3])))) ## multiply by coefficients and sum , exp makes it positive for incidence
    }
    y=c(rep(0,9),y) # zero initiation below age 10
    p.NC = smk_init*y # scales the initiation probabilities based on the spline estiamtes
    
    ## Use splines to calibrate cessation probabilities p.CF
    MP = ns(16:100,knots=c(40,50))
    Rps = predict(MP,16)[1,]
    y=c()
    for (j in 16:100){ # smk_cess[16,120] = 0.03203529 is cessation for females age 16 in recent cohorts
      y=c(y,smk_cess[16,120]*exp(sum((MP[j-16,]-Rps)*c(v_params[4],v_params[5],v_params[6])))) ## multiply by coefficients and sum , exp makes it positive for incidence
    }
    y=c(rep(0,15),y) # zero cessation below age 16
    p.CF = smk_cess*y # scales the cessation probabilities based on the spline estiamtes
    
    # # cohorts <- 1900:2100
    # test = NULL
    # for (i  in cohorts){
    #   rbind(test,smk_microsim(i, v.M_1, n.i, n.t, v.n))
    # }
    
    # Simulate for each birth cohort with parallelization
    m.cohortbyage<-foreach (i=cohorts, .combine='rbind', 
                            .export=c('smk_microsim','smk_probs','get_prevs', 
                                      # 'smk_init','smk_cess','death_cs','death_ns','death_fs',
                                      'n.i','n.t','v.n','n.s','v.M_1',
                                      'p.NC','p.CF','p.NX','p.CX','p.FX')) %dopar%
        {
            smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M
        }
    
### Serial:
## m.cohortbyage <- do.call(rbind, lapply(cohorts, function(i) { smk_microsim(i, v.M_1, n.i, n.t, v.n)$m.M }))

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
    
    cat("Time: ")
    print(Sys.time() - t_init) # End timer
    return(model_res)
}

model_res<-main(v_params)
save(model_res, file=here("test_runs/model_res.RData"))

save(model_res, file=paste0(namethisrun,"_n",n.i,"_",whichgender,".Rdata"))

## Specify calibration parameters ------------------------------------------

# Specify seed (for reproducible sequence of random numbers)
set.seed(072218)

# number of initial starting points
n_init <- 10

# names and number of input parameters to be calibrated
v_param_names <- c("sp1","sp2","sp3","sp4","sp5","sp6")
n_param <- length(v_param_names)

# range on input search space
lb <- rep(-10,6) # lower bound
ub <- rep(10,6) # upper bound

# number of calibration targets
v_target_names <- names(lst_smktargets)
n_target <- length(v_target_names)

v_params <- rep(1,6)
  
## Calibration Functions ---------------------------------------------------

# Write goodness-of-fit function to pass to Nelder-Mead algorithm
f_gof <- function(v_params){
  
  # Run model for parameter set "v_params"
  model_res <- main(v_params)
  
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

load("C:/Users/jamietam/Dropbox/Analysis/NSDUH/depsmkprevs_2005-2020.rda")

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