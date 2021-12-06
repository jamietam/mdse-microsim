rm(list = ls())  # remove any variables in R's memory
library(openxlsx)
library(plyr)
library(matrixStats)
library(stringr)
library(doParallel) # set up model to run in parallel
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

# mainDir <- "C:/Users/JT936/Dropbox/GitHub/Microsimulation-tutorial"
mainDir <- "C:/Users/jamietam/Dropbox/GitHub/smk-dep-model"
setwd(file.path(mainDir))
namethisrun <- "11.23.2021"

## MODEL INPUTS ------------------------------------------------------------
n.i   <- 100                      # number of simulated individuals
n.t   <- 100                    # time horizon per person, number of years
whichgender <- "females"
cohorts <- 1900:2100

# model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), "Underreport" (U), Dead (X)
v.n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","NU","CU","FU","X")

n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NH", n.i)          # everyone begins in the Never smoker Never MD state 
v.Trt <- c("No Treatment", "Treatment") # store the strategy names

# mortality inputs
death_ns = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("ns_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Never Smoker
death_cs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("cs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Current Smoker
death_fs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("fs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Former Smoker
rr.DX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION #c(rep(1,17),rep(5.68,100-18)) # RR of death when ever MD
rr.RX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION
rr.UX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION - leading to negative probabilities for specific birth cohorts/ages

# smoking inputs
smk_init_cisnet = read.xlsx("cisnet_smkrates_07022020.xlsx",sheet=paste0(whichgender,"_init"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
smk_cess_cisnet = read.xlsx("cisnet_smkrates_07022020.xlsx",sheet=paste0(whichgender,"_cess"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
smkinit_SF = as.matrix(c(rep(1.96,18),rep(0.00,17),rep(1.00,30), rep(1.00,35))) # smkinit_youthSF, smkinit_SF_18to34, smkinit_SF_35to64, smkinit_SF_65plus
smkcess_SF = as.matrix(c(rep(1.00,18),rep(0.55,17),rep(0.97,30), rep(0.39,35))) # smkcess_youthSF, smkcess_SF_18to34, smkcess_SF_35to64, smkcess_SF_65plus
smk_init= smk_init_cisnet*smkinit_SF # scale smoking initiation rates 
smk_cess = smk_cess_cisnet*smkcess_SF # scale smoking cessation rates

# depression inputs
p.HD = read.xlsx("incidence_eaton.xlsx",sheet=paste0(whichgender),rowNames=TRUE, colNames=FALSE, check.names=FALSE)$X2 
p.HD[0:12]<-0
p.HD.2016 = p.HD # NEED TO FIGURE OUT HOW TO scale up MDE incidence for those ages <25 starting in 2016
p.HD.2016[0:26]<-p.HD.2016[0:26]*2.3823137 # inc_SF = 2.3823137

p.DR = c(rep(0.173,99),0) # probability to recover
p.RD = p.UD = c(rep(0.058,99),0) # probability of recurrent MD if Former MD or Recall Error (R, E) # ESTIMATE DURING CALIBRATION - leading to negative probabilities for specific birth cohorts/ages
p.RU = rep(0,100) # as.matrix(c(rep(0,25),rep(0.152,9),rep(0.101,15),rep(0.120,15),rep(0.923,35))) # probability to Recall Error (E) when Former MD (R)

# interaction effects # RE-ESTIMATE THESE PARAMETERS DURING CALIBRATION??
rr.CH.CD = 1.41 # increased probability of depression if current smoker (RRcs_dep1)
rr.CD.FD = rr.CR.FR = rr.CE.FE = 0.98 # decreased probability of quitting smoking if history of depression (D, R, E) (ORhdep_quit)

# ESTIMATE DURING CALIBRATION # increased probability of smoking initiation if Depressed (D) (Edepr_smkinit) == 5.19
rr.ND.CD = 4.72 # 4.73 leads to negative probabilities, must be less than or equal to 4.726452   

rr.CD.CR = 0.73 # decreased probability of recovery from depression if current smoker (C) (deprecovSF_cs)
#Ecs_depr	 1.00 
#RRfs_dep1	 1.00 
#Efs_depr	 1.00 
#deprecovSF_fs	 1.00 

# RRcs_dep1	 1.41 
# ORhdep_quit	 0.98 
# Ecs_depr	 1.00 
# Edepr_smkinit	 4.73 
# deprecovSF_cs	 0.73 
# RRdepr_death	 5.54 
# RRfs_dep1	 1.00 
# Efs_depr	 1.00 
# deprecovSF_fs	 1.00 


##################################### Functions ###########################################

# The MicroSim function keeps track of what happens to each individual during each cycle. 
# Arguments:  
# v.M_1:   vector of initial states for individuals 
# n.i:     number of individuals
# n.t:     total number of cycles to run the model
# v.n:     vector of health state names
# TR.out:  should the output include a microsimulation trace? (default is TRUE)
# TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
# Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
# Makes use of:
# Probs:   function for the estimation of transition probabilities

MicroSim <- function(bc,v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
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
      v.p <- Probs(bc, t, m.M[i, t])           # calculate the transition probabilities at cycle t 
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
}  # end of the MicroSim function  


## PROBABILITY FUNCTION ----------------------------------------------------

Probs <- function(bc, t, M_it) { # updates the transition probabilities of every cycle
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  
  # Transition probabilities (per cycle) by birth cohort
  p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
  p.CF <- round(diag(as.matrix(smk_cess)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
  p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
  p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
  p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
  
  p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
  p.NC[100] <- p.CF[100] <- 0 
  
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # Happy
  v.p.it[M_it == "NH"] <- 
    c((1-p.NX[t])*(1-p.NC[t]-p.HD[t]), (1-p.NX[t])*p.NC[t], 0, #H = Happy
      (1-p.NX[t])*p.HD[t],0,0, 	#D = Depressed
      0,0,0,				#R = Recovered
      0,0,0, 				#U = Underreport
      p.NX[t]) 			#X = DEAD
  
  v.p.it[M_it == "CH"] <- 
    c(0,(1-p.CX[t])*(1-p.CF[t]-rr.CH.CD*p.HD[t]), (1-p.CX[t])*p.CF[t], #H = Happy
      0,(1-p.CX[t])*rr.CH.CD*p.HD[t],0, #D = Depressed
      0,0,0, 				#R = Recovered
      0,0,0, 				#U = Underreport
      p.CX[t]) 			#X = DEAD
  
  v.p.it[M_it == "FH"] <- 
    c(0,0, (1 - p.FX[t])*(1-p.HD[t]), #H = Happy
      0,0, (1 - p.FX[t])*p.HD[t],	#D = Depressed
      0,0,0,				#R = Recovered
      0,0,0,				#U = Underreport
      p.FX[t])			#X = DEAD
  
  # Depressed
  v.p.it[M_it == "ND"] <- 
    c(0,0,0, 	#H = Happy
      (1-rr.DX[t]*p.NX[t])*(1-rr.ND.CD*p.NC[t]-p.DR[t]), (1-rr.DX[t]*p.NX[t])*rr.ND.CD*p.NC[t], 0, #D = Depressed
      (1-rr.DX[t]*p.NX[t])*p.DR[t],0,0, 		#R = Recovered
      0,0,0, 				#U = Underreport
      rr.DX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CD"] <- c(0,0,0,				#H = Happy
                            0,(1-rr.DX[t]*p.CX[t])*(1-p.CF[t]-p.DR[t]), (1-rr.DX[t]*p.CX[t])*p.CF[t], #D = Depressed
                            0,(1-rr.DX[t]*p.CX[t])*p.DR[t],0,			#R = Recovered
                            0,0,0,				#U = Underreport
                            rr.DX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FD"] <- c(0,0,0,				#H = Happy
                            0,0,(1-rr.DX[t]*p.FX[t])*(1 - p.DR[t]),#D = Depressed
                            0,0,(1-rr.DX[t]*p.FX[t])*p.DR[t],			#R = Recovered
                            0,0,0,				#U = Underreport
                            rr.DX[t]*p.FX[t])		#X = DEAD
  # Recovered
  v.p.it[M_it == "NR"] <- c(0,0,0,				#H = Happy
                            (1-rr.RX[t]*p.NX[t])*p.RD[t],0,0, 			#D = Depressed
                            (1-rr.RX[t]*p.NX[t])*(1-p.NC[t]-p.RD[t]-p.RU[t]), (1-rr.RX[t]*p.NX[t])*p.NC[t],0, #R = Recovered
                            (1-rr.RX[t]*p.NX[t])*p.RU[t],0,0, 		#U = Underreport
                            rr.RX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CR"] <- c(0,0,0,				#H = Happy
                            0,(1-rr.RX[t]*p.CX[t])*p.RD[t],0,			#D = Depressed
                            0,(1-rr.RX[t]*p.CX[t])*(1-p.RD[t]-p.RU[t]-rr.CR.FR*p.CF[t]), (1-rr.RX[t]*p.CX[t])*rr.CR.FR*p.CF[t],	#R = Recovered
                            0,(1-rr.RX[t]*p.CX[t])*p.RU[t],0,		#U = Underreport
                            rr.RX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FR"] <- c(0,0,0,				#H = Happy
                            0,0,(1-rr.RX[t]*p.FX[t])*p.RD[t],			#D = Depressed
                            0,0,(1-rr.RX[t]*p.FX[t])*(1-p.RU[t]-p.RD[t]),	#R = Recovered
                            0,0,(1-rr.RX[t]*p.FX[t])*p.RU[t],		#U = Underreport
                            rr.RX[t]*p.FX[t])		#X = DEAD
  
  # Underreport
  v.p.it[M_it == "NU"] <- c(0,0,0,				#H = Happy
                            (1-rr.UX[t]*p.NX[t])*p.UD[t],0,0, 			#D = Depressed
                            0,0,0, 				#R = Recovered
                            (1-rr.UX[t]*p.NX[t])*(1-p.UD[t]-p.NC[t]), (1-rr.UX[t]*p.NX[t])*p.NC[t] ,0, #U = Underreport
                            rr.UX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CU"] <- c(0,0,0,				#H = Happy
                            0,(1-rr.UX[t]*p.CX[t])*p.UD[t],0,			#D = Depressed
                            0,0,0 ,				#R = Recovered
                            0,(1-rr.UX[t]*p.CX[t])*(1-p.UD[t]-p.CF[t]), (1-rr.UX[t]*p.CX[t])*p.CF[t],		#U = Underreport
                            rr.UX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FU"] <- c(0,0,0,				#H = Happy
                            0,0,(1-rr.UX[t]*p.FX[t])*p.UD[t],			#D = Depressed
                            0,0,0,				#R = Recovered
                            0,0,(1-rr.UX[t]*p.FX[t])*(1-p.UD[t]),		#U = Underreport
                            rr.UX[t]*p.FX[t])		#X = DEAD
  
  v.p.it[M_it == "X"]  <- c(0,0,0,				#H = Happy
                            0,0,0,				#D = Depressed
                            0,0,0,				#R = Recovered
                            0,0,0,				#U = Underreport
                            1)					#X = DEAD
  # return the transition probabilities or produce an error
  ifelse(any(is.na(v.p.it)), print(paste0(paste0(v.p.it,collapse=", ")," - NA probability! bc: ", bc,", age: ",t,", M_it: ",M_it)),return(v.p.it)) 
  ifelse(any(v.p.it<0),print(paste0(paste0(v.p.it,collapse=", ")," - Negative probability! bc: ", bc, ", age: ",t,", M_it: ", M_it)),return(v.p.it))
  ifelse(round(sum(v.p.it),8) == 1, return(v.p.it), print(paste("Probabilities do not sum to 1:", sum(v.p.it), "bc:",bc,"age:",t,"M_it:",M_it))) # rounds off to the eigth digit because otherwise you get 0.000000001 instead of 0
  return(v.p.it) 
}       


## RUN THE SIMULATION ------------------------------------------------------

# test with a single cohort
# sim_1980  <- MicroSim(1980, v.M_1, n.i, n.t, v.n) 
# MicroSim(1980, v.M_1, n.i, n.t, v.n)$m.M
t_init <- Sys.time()
system.time(
  m.cohortbyage<-foreach (i=cohorts, .combine='rbind') %dopar% 
    {
      MicroSim(i, v.M_1, n.i, n.t, v.n)$m.M
    }
)
Sys.time() - t_init

save(m.cohortbyage, file=paste0(namethisrun,"_n",n.i,"_",whichgender,".Rdata"))
# load("testing_females.Rdata")

# Convert matrix from cohort-age to cohort-year
m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = 301)
for (b in 1:length(cohorts)){
  m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
}
colnames(m.cohortbyyear) <- c(1900:2200)
rownames(m.cohortbyyear) <- paste(sort(rep(cohorts,n.i)),1:n.i, sep = ".") 


# Get counts/prevalence of individuals in a health state by age group and year
generate_prev_counts <- function(state,m.cohortbyyear,minyear,maxyear){
  agerownames<-c("18to25", "26to34", "35to49", "50to64",  "65plus", "total")
  agegroupstart <- c(18,26,35,50,65,18)
  agegroupend <- c(25,34,49,64,99,99)
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
      m.M.prevs<-rbind(m.M.prevs,c(state,whichgender,agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("state","gender","agegroup","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}
m.M.prevs <- NULL 
for (i in c(v.n,"N","C","F","H","D","U","R")){
  m.M.prevs = rbind(m.M.prevs, generate_prev_counts(i, m.cohortbyyear,1900,2100))
}

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
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.8),breaks=seq(0,0.8,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.8),breaks=seq(0,0.8,0.05)) +
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
  labs(title="never smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ns <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="never smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dep_age <-ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="dep" & subpopulation=="totalpop" & age!="total"), 
             aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="D"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current MDE - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dep <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="dep" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="D"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current MDE - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# never smokers - current MD
m.C <-generate_prev_counts("C", m.cohortbyyear,1900,2018)

# current smokers - current MD
generate_prev_counts(18,25,"CD", m.cohortbyyear,2005,2018)

# former smokers - current MD
generate_prev_counts(18,25,"FD", m.cohortbyyear,2005,2018)

# never smokers - never MD
generate_prev_counts(18,25,"NH", m.cohortbyyear,2005,2018)

# current smokers - never MD
generate_prev_counts(18,25,"CH", m.cohortbyyear,2005,2018)

# former smokers - never MD
generate_prev_counts(18,25,"FH", m.cohortbyyear,2005,2018)



generate_prev_counts(18,25,"NH", m.cohortbyyear,2005,2018)

#Neversmokers = c("NH", "ND","NR","NU")
#Currentsmokers = c("CH","CD","CR","CU")
#Formersmokers = c("FH","FD","FR","FU")

#Happy = c("NH","CH","FH")
#Depressed = c("ND","CD","FD")
#Recovered = c("NR","CR","FR","NU","CU","FU")


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
    # Happy
    NH <- 
      c((1-p.NX[t])*(1-p.NC[t]-p.HD[t]), (1-p.NX[t])*p.NC[t], 0, #H = Happy
        (1-p.NX[t])*p.HD[t],0,0, 	#D = Depressed
        0,0,0,				#R = Recovered
        0,0,0, 				#U = Underreport
        p.NX[t]) 			#X = DEAD
    
    CH <- 
      c(0,(1-p.CX[t])*(1-p.CF[t]-rr.CH.CD*p.HD[t]), (1-p.CX[t])*p.CF[t], #H = Happy
        0,(1-p.CX[t])*rr.CH.CD*p.HD[t],0, #D = Depressed
        0,0,0, 				#R = Recovered
        0,0,0, 				#U = Underreport
        p.CX[t]) 			#X = DEAD
    
    FH <- 
      c(0,0, (1 - p.FX[t])*(1-p.HD[t]), #H = Happy
        0,0, (1 - p.FX[t])*p.HD[t],	#D = Depressed
        0,0,0,				#R = Recovered
        0,0,0,				#U = Underreport
        p.FX[t])			#X = DEAD
    
    # Depressed
    ND <- 
      c(0,0,0, 	#H = Happy
        (1-rr.DX[t]*p.NX[t])*(1-rr.ND.CD*p.NC[t]-p.DR[t]), (1-rr.DX[t]*p.NX[t])*rr.ND.CD*p.NC[t], 0, #D = Depressed
        (1-rr.DX[t]*p.NX[t])*p.DR[t],0,0, 		#R = Recovered
        0,0,0, 				#U = Underreport
        rr.DX[t]*p.NX[t]) 		#X = DEAD
    
    CD <- c(0,0,0,				#H = Happy
                              0,(1-rr.DX[t]*p.CX[t])*(1-p.CF[t]-p.DR[t]), (1-rr.DX[t]*p.CX[t])*p.CF[t], #D = Depressed
                              0,(1-rr.DX[t]*p.CX[t])*p.DR[t],0,			#R = Recovered
                              0,0,0,				#U = Underreport
                              rr.DX[t]*p.CX[t])  	#X = DEAD
    
    FD <- c(0,0,0,				#H = Happy
                              0,0,(1-rr.DX[t]*p.FX[t])*(1 - p.DR[t]),#D = Depressed
                              0,0,(1-rr.DX[t]*p.FX[t])*p.DR[t],			#R = Recovered
                              0,0,0,				#U = Underreport
                              rr.DX[t]*p.FX[t])		#X = DEAD
    # Recovered
    NR <- c(0,0,0,				#H = Happy
                              (1-rr.RX[t]*p.NX[t])*p.RD[t],0,0, 			#D = Depressed
                              (1-rr.RX[t]*p.NX[t])*(1-p.NC[t]-p.RD[t]-p.RU[t]), (1-rr.RX[t]*p.NX[t])*p.NC[t],0, #R = Recovered
                              (1-rr.RX[t]*p.NX[t])*p.RU[t],0,0, 		#U = Underreport
                              rr.RX[t]*p.NX[t]) 		#X = DEAD
    
    CR <- c(0,0,0,				#H = Happy
                              0,(1-rr.RX[t]*p.CX[t])*p.RD[t],0,			#D = Depressed
                              0,(1-rr.RX[t]*p.CX[t])*(1-p.RD[t]-p.RU[t]-rr.CR.FR*p.CF[t]), (1-rr.RX[t]*p.CX[t])*rr.CR.FR*p.CF[t],	#R = Recovered
                              0,(1-rr.RX[t]*p.CX[t])*p.RU[t],0,		#U = Underreport
                              rr.RX[t]*p.CX[t])  	#X = DEAD
    
    FR <- c(0,0,0,				#H = Happy
                              0,0,(1-rr.RX[t]*p.FX[t])*p.RD[t],			#D = Depressed
                              0,0,(1-rr.RX[t]*p.FX[t])*(1-p.RU[t]-p.RD[t]),	#R = Recovered
                              0,0,(1-rr.RX[t]*p.FX[t])*p.RU[t],		#U = Underreport
                              rr.RX[t]*p.FX[t])		#X = DEAD
    
    # Underreport
    NU <- c(0,0,0,				#H = Happy
                              (1-rr.UX[t]*p.NX[t])*p.UD[t],0,0, 			#D = Depressed
                              0,0,0, 				#R = Recovered
                              (1-rr.UX[t]*p.NX[t])*(1-p.UD[t]-p.NC[t]), (1-rr.UX[t]*p.NX[t])*p.NC[t] ,0, #U = Underreport
                              rr.UX[t]*p.NX[t]) 		#X = DEAD
    
    CU <- c(0,0,0,				#H = Happy
                              0,(1-rr.UX[t]*p.CX[t])*p.UD[t],0,			#D = Depressed
                              0,0,0 ,				#R = Recovered
                              0,(1-rr.UX[t]*p.CX[t])*(1-p.UD[t]-p.CF[t]), (1-rr.UX[t]*p.CX[t])*p.CF[t],		#U = Underreport
                              rr.UX[t]*p.CX[t])  	#X = DEAD
    
    FU <- c(0,0,0,				#H = Happy
                              0,0,(1-rr.UX[t]*p.FX[t])*p.UD[t],			#D = Depressed
                              0,0,0,				#R = Recovered
                              0,0,(1-rr.UX[t]*p.FX[t])*(1-p.UD[t]),		#U = Underreport
                              rr.UX[t]*p.FX[t])		#X = DEAD
    
    
    allprobs = rbind(NH, CH, FH, ND, CD, FD, NR, CR, FR, NU, CU, FU)
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
# The MDS microsimulation model was developed by Jamie Tam and last updated on 11/3/2021
#
# The code was adapted from Appendix A of the article: 
#
# Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400-22.
# 
# See GitHub for more information: https://github.com/DARTH-git/Microsimulation-tutorial
############################################################################################
