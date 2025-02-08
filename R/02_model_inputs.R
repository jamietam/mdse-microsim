## INPUTS 
whichgender <- args[1]

load(paste0(mainDir,"data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0(mainDir,"data/smk_precomputed_inputs_",whichgender,".RData")) #l.smktargets
load(paste0(mainDir,"data/cuw_inputs_",whichgender,".RData"))
load(paste0(mainDir,"data/pop_",whichgender,".RData")) # Read in Census population for SAD calculation
load(paste0(mainDir,"data/ecig_precomputed_inputs_",whichgender,".RData"))

calib_startyear <-2005
endyear <- as.numeric(args[3]) 
cohorts <- 1900:as.numeric(args[3])            # last cohort is the last calendar year
n.i   <- as.numeric(args[2])                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years
v.n <- c("NOH","COH","FOH","NOD","COD","FOD","NOR","COR","FOR","NEH","CEH","FEH","NED","CED","FED","NER","CER","FER","NQH","CQH","FQH","NQD","CQD","FQD","NQR","CQR","FQR", "X")
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NOH", n.i)         # everyone begins in the Never smoker Never MD state


# CALIBRATION TARGETS
load(paste0(mainDir,"data/nsduh_calib_targets_",whichgender,".RData")) 

#l.calib_targets <-l.calib_targets[c("N","C","F","D","N_D","C_D","F_D")]
l.calib_targets <- lapply(l.calib_targets,function(x) x[x[,"survey_year"]<=endyear & x[,"survey_year"]>=calib_startyear,]) # keep survey years for calibration targets only

## CALIBRATION PARAMETERS - Specify which parameters you want to calibrate (0 vs 1 in column 4), and provide upper and lower bounds for the search algorithm
if (whichgender == "males") {
  m.calib_inputs <-rbind( 
    #scaling for smoking for non depressed
    "s.NC_9.17" = c(1.826712, 1.5, 3, 1),#run this
    "s.NC_18.25" = c(0.03747093, 0, 0.1, 1),#run this
    "s.CF_18.25" = c(0.483608280971776, 0.30, 1.0, 1),
    "s.CF_26.34" = c(0.456131185046594, 0.30, 1.0, 1),
    "s.CF_35.49" = c(0.852501801853833, 0.30, 1.0, 1),  
    "s.CF_50.64" = c(0.583999477054824, 0.30, 1.0, 1), 
    "s.CF_65.99" = c(0.503133126398933, 0.30, 1.0, 1), 
    #scaling for smoking for depressed
    "s.NC_D_9.17" = c(2.123080, 1.5, 4, 0), #run this
    "s.NC_D_18.25" = c(3.541852, 2, 4, 0),#run this
    "s.NC_D_26.34" = c(3.931820, 2, 4, 0),#run this
    #re-estimate vaping initiation
    "p.NO.NE_20.21_18.25" = c(0.06076851, 0.05, 0.1, 0), 
    "p.NO.NE_22.23_18.25" = c(0.12746416, 0.05, 0.2, 0),

    "p.CO.CE_22.23_26.34" = c(0.36481464, 0.3, 0.6, 0),
    "p.CO.CE_22.23_35.49" = c(0.16375421, 0.1, 0.5, 0),

    "p.FO.FE_22.23_26.34" = c(0.88590871, 0.7, 1, 0),
    "p.FO.FE_22.23_35.49" = c(0.08382439, 0.05, 0.2, 0),

    #probability for depressed to recovered
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65.99" = c(0.803050446315691, 0.8, 0.85, 0),
    #scaling for healthy to depressed
    "s.HD_12.17" = c(2.75964326462325, 1, 3, 0),
    "s.HD_18.25" = c(2.6749967677134, 2.5, 4, 0), 
    "s.HD_26.34" = c(3.40799465951787, 2, 4, 0),
    #rr of depression and recovery among smokers
    "rr.CH.CD" = c(1.842347, 1.0, 2, 0),
    "rr.CR.CD" = c(1.079901 , 1.0, 2, 0),
    "rr.CD.FD" = c(0.90922571 , 0, 1.0, 0),
    "rr.OD.ED" = c(1.78941321, 1, 2, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0))
} else if (whichgender == "females") {
  m.calib_inputs <-rbind( 
    #healthy (not depressed) scaling
    "s.NC_9.17" = c(1.677217, 1.5, 2, 1),
    "s.NC_18.25" = c(0.0014241223, 0, 0.1, 1),
    "s.CF_18.25" = c(0.533405964435913, 0.50, 1.0, 1),
    "s.CF_26.34" = c(0.6715885419285, 0.50, 1.0, 1),
    "s.CF_35.49" = c(0.769270653272708, 0.50, 1.0, 1),
    "s.CF_50.64" = c(0.592565705660546, 0.50, 1.0, 1),
    "s.CF_65.99" = c(0.701643230463244, 0.50, 1.0, 1),
    
    #depressed scaling factors
    "s.NC_D_9.17" = c(3.273167 , 1.5, 4, 0), 
    "s.NC_D_18.25" = c(4, 2, 4, 0),
    "s.NC_D_26.34" = c(2.759139, 2, 4, 0),
              
    #re-estimate vaping initiation 
    "p.NO.NE_20.21_18.25" = c(0.06269568, 0.05, 0.1, 0), 
    "p.NO.NE_22.23_18.25" = c(0.12128167, 0.05, 0.2, 0),
    
    "p.CO.CE_22.23_26.34" = c(0.31232274, 0.2, 0.5, 0),
    "p.CO.CE_22.23_35.49" = c(0.20463309, 0.1, 0.5, 0),
    
    "p.FO.FE_22.23_26.34" = c(0.81429117, 0.7, 1, 0),
    "p.FO.FE_22.23_35.49" = c(0.08742362, 0.05, 0.2, 0),
    
    # recovery 
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65.99" = c(0.65, 0.6, 0.65, 0),
    
    # incidence
    "s.HD_12.17" = c(2, 1.0, 3.0, 0),
    "s.HD_18.25" = c(2.5, 2.0, 4.0, 0), 
    "s.HD_26.34" = c(2, 1.0, 3.0, 0),
    
    # interaction effects
    "rr.CH.CD" = c(1.577963, 1.0, 2, 0),
    "rr.CR.CD" = c(1.275544, 1.0, 2, 0),
    "rr.CD.FD" = c(1.0000000, 0, 1.0, 0),
    "rr.OD.ED" = c(1.32982605, 1, 2, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0))
} 
colnames(m.calib_inputs) =c("value","lower","upper","calib")  
v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate