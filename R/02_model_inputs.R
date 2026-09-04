## INPUTS 
whichgender <- args[1]

load(paste0(mainDir,"data/dep_precomputed_inputs_",whichgender,".RData")) 
load(paste0(mainDir,"data/smk_precomputed_inputs_",whichgender,".RData")) #l.smktargets
load(paste0(mainDir,"data/cuw_inputs_",whichgender,".RData"))
load(paste0(mainDir,"data/pop_",whichgender,".RData")) # Read in Census population for SAD calculation
load(paste0(mainDir,"data/ecig_precomputed_inputs_",whichgender,".RData"))

if (whichgender=="females"){
  deaths<<-1473817
}else {
  deaths<<-1616765
}

calib_startyear <-2005
calib_splityear <- 2020
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
    "s.NC_18.23_9.17" = c(0.65, 1.5, 3, 1),
    "s.NC_18.23_18.25" = c(0.03747093, 0, 0.1, 1),
    "s.NC_18.23_26.34" = c(0.03747093, 0, 0.1, 1),
    "s.CF_18.23_15.25" = c(0.533356384, 0.30, 1.0, 1),
    "s.CF_18.23_26.34" = c(0.343975183, 0.30, 1.0, 1),
    "s.CF_18.23_35.49" = c(0.3, 0.30, 1.0, 1),  
    "s.CF_18.23_50.64" = c(0.7, 0.30, 1.0, 1), #0.429631986
    "s.CF_18.23_65.99" = c(0.65, 0.30, 1.0, 1), #0.259055186
    "s.NC_9.17" = c(1.826712, 1.5, 3, 1),
    "s.NC_18.25" = c(0.03747093, 0, 0.1, 1), 
    "s.NC_26.34" = c(0.03747093, 0, 0.1, 1),
    "s.CF_15.25" = c(0.483608280971776, 0.30, 1.0, 1),
    "s.CF_26.34" = c(0.456131185046594, 0.30, 1.0, 1),
    "s.CF_35.49" = c(0.852501801853833, 0.30, 1.0, 1),  
    "s.CF_50.64" = c(0.7, 0.30, 1.0, 1), #0.583999477054824
    "s.CF_65.99" = c(0.65, 0.30, 1.0, 1), #0.503133126398933
    #scaling for smoking for depressed
    "s.NC_D_9.17" = c(4, 1.5, 4, 1), 
    "s.NC_D_18.25" = c(4, 2, 4, 1),
    "s.NC_D_26.34" = c(2.027556398, 2, 4, 1),
    #re-estimate vaping initiation
    # "p.NO.NE_20.21_18.25" = c(0.054671431, 0.05, 0.1, 1), 
    # "p.NO.NE_22.23_18.25" = c(0.121997686, 0.05, 0.2, 1),
    
    # "p.NO.NE_20.21_12.17" = c(, 0.05, 0.4, 1), 
    # "p.NO.NE_22.23_12.17" = c(, 0.05, 0.5, 1),
    
    # "p.CO.CE_22.23_26.34" = c(0.90000000, 0.3, 1, 1),
    # "p.CO.CE_22.23_35.49" = c(0.17239039, 0.1, 0.5, 1),
    # 
    # "p.FO.FE_22.23_26.34" = c(0.65460774, 0.7, 1, 1),
    # "p.FO.FE_22.23_35.49" = c(0.05000000, 0.0, 0.2, 1),
    
    "p.NO.NE_20.21_12.17" = c(0.02, 0.05, 0.1, 1), 
    "p.NO.NE_22.23_12.17" = c(0.10, 0.05, 0.3, 1), 
    
    "p.NO.NE_20.21_18.25" = c(0.02, 0.05, 0.1, 1),
    "p.NO.NE_22.23_18.25" = c(0.10, 0.05, 0.2, 1),
    
    "p.CO.CE_22.23_18.25" = c(0.95, 0.2, 0.5, 1),
    "p.CO.CE_22.23_26.34" = c(0.6, 0.2, 0.5, 1),
    "p.CO.CE_22.23_35.49" = c(0.15, 0.1, 0.5, 1),
    "p.CO.CE_22.23_50.64" = c(0.05, 0.1, 0.5, 1),
    
    "p.FO.FE_22.23_26.34" = c(0.3, 0.7, 1, 1),
    "p.FO.FE_22.23_35.49" = c(0.05, 0.05, 0.2, 1),

    #probability for depressed to recovered
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 1),
    "p.DR_65.99" = c(0.803050446315691, 0.8, 0.85, 1),
    
    #scaling for happy to depressed
    "s.HD_12.17" = c(2.75964326462325, 1, 3, 1),
    "s.HD_18.25" = c(2.6749967677134, 2.5, 4, 1), 
    "s.HD_26.34" = c(3.40799465951787, 2, 4, 1),
    "calib.HD_2005_2015" = c(-0.0005,0,0.1,1),
    
    #rr of depression and recovery among smokers
    "rr.CH.CD" = c(1.842347, 1.0, 2, 1),
    "rr.CR.CD" = c(1.079901 , 1.0, 2, 1),
    "rr.CD.FD" = c(0.90922571 , 0, 1.0, 1),
    "rr.OD.ED" = c(1.78941321, 1, 2, 1),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 1))
  
} else if (whichgender == "females") {
  m.calib_inputs <-rbind( 
    #scaling for smoking for non depressed
    "s.NC_18.23_9.17" = c(0.5, 1.5, 2, 1),
    "s.NC_18.23_18.25" = c(0.0014241223, 0, 0.1, 1),  
    "s.NC_18.23_26.34" = c(0.0014241223, 0, 0.1, 1),
    "s.CF_18.23_15.25" = c(0.533405964435913, 0.50, 1.0, 1),
    "s.CF_18.23_26.34" = c(0.6715885419285, 0.50, 1.0, 1),
    "s.CF_18.23_35.49" = c(0.769270653272708, 0.50, 1.0, 1),
    "s.CF_18.23_50.64" = c(0.492565705660546, 0.50, 1.0, 1),
    "s.CF_18.23_65.99" = c(0.451643230463244, 0.50, 1.0, 1),
    #scaling for smoking for depressed
    "s.NC_9.17" = c(1.677217, 1.5, 2, 1),
    "s.NC_18.25" = c(0.0014241223, 0, 0.1, 1),
    "s.NC_26.34" = c(0.0014241223, 0, 0.1, 1),
    "s.CF_15.25" = c(0.533405964435913, 0.50, 1.0, 1),
    "s.CF_26.34" = c(0.6715885419285, 0.50, 1.0, 1),
    "s.CF_35.49" = c(0.769270653272708, 0.50, 1.0, 1),
    "s.CF_50.64" = c(0.592565705660546, 0.50, 1.0, 1),
    "s.CF_65.99" = c(0.651643230463244, 0.50, 1.0, 1),
    
    #depressed scaling factors
    "s.NC_D_9.17" = c(3.273167 , 2.5, 4, 1), 
    "s.NC_D_18.25" = c(4, 3, 5.5, 1),
    "s.NC_D_26.34" = c(2.759139, 2, 4, 1),
              
    #re-estimate vaping initiation 
    "p.NO.NE_20.21_12.17" = c(0.02, 0.05, 0.1, 1), 
    "p.NO.NE_22.23_12.17" = c(0.10, 0.05, 0.3, 1), 
    
    "p.NO.NE_20.21_18.25" = c(0.02, 0.05, 0.1, 1),
    "p.NO.NE_22.23_18.25" = c(0.10, 0.05, 0.2, 1),
    
    "p.CO.CE_22.23_18.25" = c(0.41232274, 0.2, 0.5, 1),
    "p.CO.CE_22.23_26.34" = c(0.41232274, 0.2, 0.5, 1),
    "p.CO.CE_22.23_35.49" = c(0.20463309, 0.1, 0.5, 1),
    "p.CO.CE_22.23_50.64" = c(0.08, 0.1, 0.5, 1),
    
    "p.FO.FE_22.23_26.34" = c(0.13, 0.7, 1, 1),
    "p.FO.FE_22.23_35.49" = c(0.05, 0.05, 0.2, 1),
    
    # recovery 
    "p.DR_12.64" = c(0.173, 0.0, 1.0, 1),
    "p.DR_65.99" = c(0.65, 0.6, 0.65, 1),
    
    # incidence
    "s.HD_12.17" = c(1.5, 1.0, 3.0, 1), 
    "s.HD_18.25" = c(2.5, 2.0, 4.0, 1), 
    "s.HD_26.34" = c(2, 1.0, 3.0, 1),
    "calib.HD_2005_2015" = c(0.002,0,0.1, 1),
    
    # interaction effects
    "rr.CH.CD" = c(1.577963, 1.0, 2, 1),
    "rr.CR.CD" = c(1.275544, 1.0, 2, 1),
    "rr.CD.FD" = c(1.0000000, 0, 1.0, 1),
    "rr.OD.ED" = c(1.62982605, 1, 2, 1),
    "yearinc_p.HD" = c(2016, 2012.5, 2018.5, 1))
} 
colnames(m.calib_inputs) =c("value","lower","upper","calib")  
# emews.top100 <- read.csv("data-raw/top100_params.csv")
# emews.top100 <-emews.top100[,-c(1,42)] # remove columns for generation and gof
# merge top and bottom row of emews results into inputs
# m.calib_inputs <- merge(m.calib_inputs,cbind(t(emews.top100[1,]),t(emews.top100[100,])),by="row.names")
# rownames(m.calib_inputs) <- m.calib_inputs[,1]
# m.calib_inputs[,"calib"] = 1
# choose which set of calibrated parameters to generate results for
v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"value"]  # run with original parameter values
# v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,] # [,"1"]  # run with top row of emews results
# v.params <- m.calib_inputs[m.calib_inputs[,"calib"]==1,][,"100"] # run model with bottom row of emews results  

n.param <- length(v.params) # number of parameters to calibrate