## INPUTS 
whichgender <- args[1]

load(paste0(mainDir,"data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0(mainDir,"data/smk_precomputed_inputs_",whichgender,".RData")) #lst_smktargets
load(paste0(mainDir,"data/cuw_inputs_",whichgender,".RData"))
load(paste0(mainDir,"data/pop_",whichgender,".RData")) # Read in Census population for SAD calculation

cohorts <- 1900:as.numeric(args[3])           # Change from 2016 to 2022 or 2100
calib_startyear <-2005
n.i   <- as.numeric(args[2])                   # number of simulated individuals per run (cohort) - eventually want to run 10,000
n.t   <- 100                    # time horizon per person, number of years
v.n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","X") # model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), Dead (X)
#v.n <- c("NOH","COH","FOH","NOD","COD","FOD","NOR","COR","FOR","NVH","CVH","FVH","NVD","CVD","FVD","NVR","CVR","FVR","NQH","CQH","FQH","NQD","CQD","FQD","NQR","CQR","FQR", "X")
n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NH", n.i)         # everyone begins in the Never smoker Never MD state

d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%

# CALIBRATION TARGETS
load(paste0(mainDir,"data/smk_calib_targets_",whichgender,".RData")) #lst_smktargets
load(paste0(mainDir,"data/dep_calib_targets_",whichgender,".RData")) #lst_deptargets
load(paste0(mainDir,"data/smkdep_calib_targets_",whichgender,".RData")) #lst_smkdeptargets
lst_targets <- c(lst_smktargets,lst_deptargets[2],lst_smkdeptargets)
lst_calibtargets <- lapply(lst_targets,function(x) x[x[,"survey_year"]<=max(cohorts) & x[,"survey_year"]>=calib_startyear,]) # keep survey years 2016-2020

## CALIBRATION PARAMETERS - Specify which parameters you want to calibrate (0 vs 1 in column 4), and provide upper and lower bounds for the search algorithm
if (whichgender == "males") {
  calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.05287465042159, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0.0848159716005523, 0, 1, 0),
    "s.CF_18.25" = c(0.758981274877442, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.616667582480333, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.746427943601139, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.912583380690363, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.720470979706167, 0.50, 1.0, 0),
    "p.DR_12.17" = c(0.173, 0.0, 1.0, 1),
    "p.DR_18.25" = c(0.173, 0.0, 1.0, 1),
    "p.DR_26.34" = c(0.173, 0.0, 1.0, 1),
    "p.DR_35.49" = c(0.173, 0.0, 1.0, 1),
    "p.DR_50_64" = c(0.173, 0.0, 1.0, 1),
    "p.DR_65_99" = c(0.173, 0.0, 1.0, 1),
    "p.RD_12.17" = c(0.058, 0.0, 1.0, 1),
    "p.RD_18.25" = c(0.058, 0.0, 1.0, 1),
    "p.RD_26.34" = c(0.058, 0.0, 1.0, 1),
    "p.RD_35.49" = c(0.058, 0.0, 1.0, 1),
    "p.RD_50_64" = c(0.058, 0.0, 1.0, 1),
    "p.RD_65_99" = c(0.058, 0.0, 1.0, 1),
    "s.HD_12.17" = c(7.078470, 0.0, 10.0, 0),
    "s.HD_18.25" = c(1.690519, 0.0, 10.0, 0), 
    "s.HD_26.34" = c(2.0, 0.0, 10.0, 0),
    "rr.DX_18.25" = c(1, 1.0, 8.0, 1), #c(1.08073196073528, 1.0, 8.0, 1),
    "rr.DX_26.34" = c(1, 1.0, 8.0, 1), #c(3.69236171640223, 1.0, 8.0, 1),
    "rr.DX_35.49" = c(1, 1.0, 8.0, 1), #c(3.77460941897007, 1.0, 8.0, 1),
    "rr.DX_50.64" = c(1, 1.0, 8.0, 1), #c(3.43143755495548, 1.0, 8.0, 1),
    "rr.DX_65.99" = c(1, 1.0, 8.0, 1), #c(2.49499687193893, 1.0, 8.0, 1),
    "rr.ND.CD" = c(2.70249504281674, 1.0, 4.0, 1),
    "rr.CH.CD" = c(1.4132489374518, 1.0, 4.0, 1),
    "rr.CR.CD" = c(2.41892186717596, 1.0, 4.0, 1),
    "rr.CD.FD" = c(1.54728820101549, 1.0, 4.0, 1),
    "yearinc_p.HD" = c(2015, 2012.5, 2016.5, 0))
} else if (whichgender == "females") {
  calib_inputs <-rbind( 
    "s.NC_9.17" = c(2.081761888, 2.0, 2.5, 0),
    "s.NC_18.25" = c(0, 0, 1, 0),
    "s.CF_18.25" = c(0.993978012, 0.50, 1.0, 0),
    "s.CF_26.34" = c(0.564477195, 0.50, 1.0, 0),
    "s.CF_35.49" = c(0.87073767, 0.50, 1.0, 0),
    "s.CF_50.64" = c(0.654785173, 0.50, 1.0, 0),
    "s.CF_65.99" = c(0.664328015, 0.50, 1.0, 0),
    "p.DR_12.17" = c(0.173, 0.0, 1.0, 1),
    "p.DR_18.25" = c(0.173, 0.0, 1.0, 1),
    "p.DR_26.34" = c(0.173, 0.0, 1.0, 1),
    "p.DR_35.49" = c(0.173, 0.0, 1.0, 1),
    "p.DR_50_64" = c(0.173, 0.0, 1.0, 1),
    "p.DR_65_99" = c(0.173, 0.0, 1.0, 1),
    "p.RD_12.17" = c(0.058, 0.0, 1.0, 1),
    "p.RD_18.25" = c(0.058, 0.0, 1.0, 1),
    "p.RD_26.34" = c(0.058, 0.0, 1.0, 1),
    "p.RD_35.49" = c(0.058, 0.0, 1.0, 1),
    "p.RD_50_64" = c(0.058, 0.0, 1.0, 1),
    "p.RD_65_99" = c(0.058, 0.0, 1.0, 1),
    "s.HD_12.17" = c(2.1019429, 0.0, 10.0, 0),
    "s.HD_18.25" = c(3.6714343, 0.0, 10.0, 0), 
    "s.HD_26.34" = c(4.0, 0.0, 10.0, 0),
    "rr.DX_18.25" = c(1, 1.0, 8.0, 1), #c(7.284504694, 1.0, 8.0, 1),
    "rr.DX_26.34" = c(1, 1.0, 8.0, 1), #c(5.400727017, 1.0, 8.0, 1),
    "rr.DX_35.49" = c(1, 1.0, 8.0, 1), #c(5.518611922, 1.0, 8.0, 1),
    "rr.DX_50.64" = c(1, 1.0, 8.0, 1), #c(6.34692571, 1.0, 8.0, 1),
    "rr.DX_65.99" = c(1, 1.0, 8.0, 1), #c(3.801531806, 1.0, 8.0, 1),
    "rr.ND.CD" = c(1.271321945, 1.0, 4.0, 1),
    "rr.CH.CD" = c(1.848291364, 1.0, 4.0, 1),
    "rr.CR.CD" = c(1.528161447, 1.0, 4.0, 1),
    "rr.CD.FD" = c(1.220915551, 1.0, 4.0, 1),
    "yearinc_p.HD" = c(2014, 2012.5, 2016.5, 0))
} 
colnames(calib_inputs) =c("value","lower","upper","calib")  
v.params <- calib_inputs[calib_inputs[,"calib"]==1,][,"value"]  
n.param <- length(v.params) # number of parameters to calibrate
