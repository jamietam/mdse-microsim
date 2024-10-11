## INPUTS 
whichgender <- args[1]

load(paste0(mainDir,"data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0(mainDir,"data/smk_precomputed_inputs_",whichgender,".RData")) #l.smktargets
load(paste0(mainDir,"data/cuw_inputs_",whichgender,".RData"))
load(paste0(mainDir,"data/pop_",whichgender,".RData")) # Read in Census population for SAD calculation
load(paste0(mainDir,"data/ecig_precomputed_inputs_",whichgender,".RData"))


cohorts <- 1900:as.numeric(args[3])           # Change from 2016 to 2022 or 2100
calib_startyear <-2005
calib_endyear <- 2022
n.i   <- as.numeric(args[2])                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years
v.n <- c("NOH","COH","FOH","NOD","COD","FOD","NOR","COR","FOR","NEH","CEH","FEH","NED","CED","FED","NER","CER","FER","NQH","CQH","FQH","NQD","CQD","FQD","NQR","CQR","FQR", "X")
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NOH", n.i)         # everyone begins in the Never smoker Never MD state

d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%
d.year <- 2023 # which year to start discounting from
# CALIBRATION TARGETS
load(paste0(mainDir,"data/nsduh_calib_targets_",whichgender,".RData")) 
l.calib_targets <- lapply(l.calib_targets,function(x) x[x[,"survey_year"]<=calib_endyear & x[,"survey_year"]>=calib_startyear,]) # keep survey years for calibration targets only

## CALIBRATION PARAMETERS - Specify which parameters you want to calibrate (0 vs 1 in column 4), and provide upper and lower bounds for the search algorithm
if (whichgender == "males") {
  m.calib_inputs <-rbind( 
    "s.NC_9.17" = c(1.998, 2.0, 2.5, 1),
    "s.NC_18.25" = c(0, 0, 1, 1),
    "s.CF_18.25" = c(0.483608280971776, 0.30, 1.0, 1),
    "s.CF_26.34" = c(0.456131185046594, 0.30, 1.0, 1),
    "s.CF_35.49" = c(0.852501801853833, 0.30, 1.0, 1),  
    "s.CF_50.64" = c(0.583999477054824, 0.30, 1.0, 1), 
    "s.CF_65.99" = c(0.503133126398933, 0.30, 1.0, 1),  
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65.99" = c(0.803050446315691, 0.8, 0.85, 0),
    "s.HD_12.17" = c(2.75964326462325, 1, 3, 0),
    "s.HD_18.25" = c(2.6749967677134, 2.5, 4, 0), 
    "s.HD_26.34" = c(3.40799465951787, 2, 4, 0),
    "rr.ND.CD" = c(3.91210330415228, 1.0, 4.0, 0),
    "rr.CH.CD" = c(1.35368578518526, 1.0, 4.0, 0),
    "rr.CR.CD" = c(1.21112237244282, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1.58679257003318, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0),
    "rr.CE.FE_1.17" = c(1, 0, 4, 1),
    "rr.CE.FE_18.34" = c(1, 0, 4, 1),
    "rr.CE.FE_35.64" = c(1, 0, 4, 1),
    "rr.CE.FE_65.99" = c(1, 0, 4, 1),
    "rr.NE.CE_1.17" = c(1, 0, 4, 1),
    "rr.NE.CE_18.34" = c(1, 0,4, 1),
    "rr.NE.CE_35.64" = c(1, 0, 4, 1),
    "rr.NE.CE_65.99" = c(1, 0, 4, 1))
} else if (whichgender == "females") {
  m.calib_inputs <-rbind( 
    "s.NC_9.17" = c(1.999, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0, 0, 1, 0),
    "s.CF_18.25" = c(0.533405964435913, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.6715885419285, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.769270653272708, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.592565705660546, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.701643230463244, 0.50, 1.0, 0),
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65.99" = c(0.65, 0.6, 0.65, 0),
    "s.HD_12.17" = c(2, 1.0, 3.0, 1),
    "s.HD_18.25" = c(2.5, 2.0, 4.0, 1), 
    "s.HD_26.34" = c(2, 1.0, 3.0, 1),
    "rr.ND.CD" = c(3.97573317048955 , 1.0, 4.0, 0),
    "rr.CH.CD" = c(1.49641597032344, 1.0, 4.0, 0),
    "rr.CR.CD" = c(1.31121594579336, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1.21159854532057, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0),
    "rr.CE.FE_1.17" = c(1, 0, 4, 1),
    "rr.CE.FE_18.34" = c(1, 0, 4, 1),
    "rr.CE.FE_35.64" = c(1, 0, 4, 1),
    "rr.CE.FE_65.99" = c(1, 0, 4, 1),
    "rr.NE.CE_1.17" = c(1, 0, 4, 1),
    "rr.NE.CE_18.34" = c(1, 0,4, 1),
    "rr.NE.CE_35.64" = c(1, 0, 4, 1),
    "rr.NE.CE_65.99" = c(1, 0, 4, 1))
} 
colnames(m.calib_inputs) =c("value","lower","upper","calib")  
v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate