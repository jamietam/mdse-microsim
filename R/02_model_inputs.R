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

# CALIBRATION TARGETS
load(paste0(mainDir,"data/nsduh_calib_targets_",whichgender,".RData")) 
load(paste0(mainDir,"data/brfss_calib_targets_",whichgender,".RData")) 
load(paste0(mainDir,"data/nhis_calib_targets_",whichgender,".RData")) 

l.calib_targets <- c(l.calib_targets, l.brfss_targets, l.nhis_targets)

#reorder list
desired_order <- c("N", "C", "F", "D", "NO", "CO", "FO", "O", "NE", "CE", "FE", "E", "NQ", "CQ", "FQ", "Q", 
                   "ND", "CD", "FD", "OD", "ED", "QD", "vap5", "vap7", "vap10", "brfssO", "brfssE", "brfssQ",      
                   "b_exclsmk", "b_exclvap", "b_neither", "b_dual", "nhisO", "nhisE", "nhisQ",       
                   "n_exclsmk", "n_exclvap", "n_neither", "n_dual", "nsduhexclcig", "nsduhexclvap", "nsduhneither",
                   "nsduhdual")
custom_reorder <- function(lst, order) {
  return(lst[order])
}
l.calib_targets <- custom_reorder(l.calib_targets, desired_order)
l.calib_targets <- lapply(l.calib_targets,function(x) x[x[,"survey_year"]<=calib_endyear & x[,"survey_year"]>=calib_startyear,]) # keep survey years for calibration targets only

## CALIBRATION PARAMETERS - Specify which parameters you want to calibrate (0 vs 1 in column 4), and provide upper and lower bounds for the search algorithm
if (whichgender == "males") {
  m.calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.05287465042159, 2.0, 2.5, 1),
    "s.NC_18.25" = c(0.0848159716005523, 0, 1, 1),
    "s.CF_18.25" = c(0.758981274877442, 0.30, 1.0, 1),
    "s.CF_26.34" = c(0.616667582480333, 0.30, 1.0, 1),
    "s.CF_35.49" = c(0.746427943601139, 0.30, 1.0, 1),  
    "s.CF_50.64" = c(0.912583380690363, 0.30, 1.0, 1), 
    "s.CF_65.99" = c(0.720470979706167, 0.30, 1.0, 1),  
    "p.DR_12.17" = c(0.173, 0.0, 1.0, 0),
    "p.DR_18.25" = c(0.173, 0.0, 1.0, 0),
    "p.DR_26.34" = c(0.173, 0.0, 1.0, 0),
    "p.DR_35.49" = c(0.173, 0.0, 1.0, 0),
    "p.DR_50_64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65_99" = c(0.8, 0.8, 0.85, 0),
    "s.HD_12.17" = c(2, 1, 3, 0),
    "s.HD_18.25" = c(3, 2.5, 4, 0), 
    "s.HD_26.34" = c(3, 2, 4, 0),
    "rr.ND.CD" = c(3.30921375882239, 1.0, 4.0, 0),
    "rr.CH.CD" = c(1, 1.0, 4.0, 0),
    "rr.CR.CD" = c(2.3571995968942, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1.57036702143777, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0),
    "rr.CE.FE_1.17" = c(1, 0, 4, 1),
    "rr.CE.FE_18.25" = c(1, 0, 4, 1),
    "rr.CE.FE_26.34" = c(1,0,4,1),
    "rr.CE.FE_35.49" = c(1,0,4,1),
    "rr.CE.FE_50.64" = c(1,0,4,1),
    "rr.CE.FE_65.99" = c(1,0,4,1),
    "rr.NE.CE_1.17" = c(2, 0, 4, 1),
    "rr.NE.CE_18.25" = c(0.5,0,4,1),
    "rr.NE.CE_26.34" = c(1,0,4,1),
    "rr.NE.CE_35.49" = c(1,0,4,1),
    "rr.NE.CE_50.64" = c(1,0,4,1),
    "rr.NE.CE_65.99" = c(1,0,4,1))
} else if (whichgender == "females") {
  m.calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.081761888, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0, 0, 1, 0),
    "s.CF_18.25" = c(0.993978012, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.564477195, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.87073767, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.654785173, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.664328015, 0.50, 1.0, 0),
    "p.DR_12.17" = c(0.173, 0.0, 1.0, 0),
    "p.DR_18.25" = c(0.173, 0.0, 1.0, 0),
    "p.DR_26.34" = c(0.173, 0.0, 1.0, 0),
    "p.DR_35.49" = c(0.173, 0.0, 1.0, 0),
    "p.DR_50_64" = c(0.173, 0.0, 1.0, 0),
    "p.DR_65_99" = c(0.6, 0.6, 0.65, 0),
    "s.HD_12.17" = c(2, 1.0, 3.0, 0),
    "s.HD_18.25" = c(2.5, 2.0, 4.0, 0), 
    "s.HD_26.34" = c(2, 1.0, 3.0, 0),
    "rr.ND.CD" = c(3.318324 , 1.0, 4.0, 0),
    "rr.CH.CD" = c(1.626041, 1.0, 4.0, 0),
    "rr.CR.CD" = c(1, 1.0, 4.0, 0),
    "rr.CD.FD" = c(1, 1.0, 4.0, 0),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 0),
    "rr.CE.FE_1.17" = c(1, 0, 4, 1),
    "rr.CE.FE_18.25" = c(1, 0, 4, 1),
    "rr.CE.FE_26.34" = c(1,0,4,1),
    "rr.CE.FE_35.49" = c(1,0,4,1),
    "rr.CE.FE_50.64" = c(1,0,4,1),
    "rr.CE.FE_65.99" = c(1,0,4,1),
    "rr.NE.CE_1.17" = c(2, 0, 4, 1),
    "rr.NE.CE_18.25" = c(0.5,0,4,1),
    "rr.NE.CE_26.34" = c(1,0,4,1),
    "rr.NE.CE_35.49" = c(1,0,4,1),
    "rr.NE.CE_50.64" = c(1,0,4,1),
    "rr.NE.CE_65.99" = c(1,0,4,1))
} 
colnames(m.calib_inputs) =c("value","lower","upper","calib")  
v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate