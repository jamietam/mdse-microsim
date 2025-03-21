## Clean up the workspace and set main working directory
rm(list = ls()) 

# Set working directory
mainDir = "/Users/srs249/University of Michigan Dropbox/Sarah Skolnick/GitHub/mdse-microsim/"
setwd(mainDir)
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" 

hpc = 0
calibration = 0 # need to set this to 0 so main_calib works and outputs proper matrix for main function
run_scenarios = 1 # set to 0 if you want to use pre-generated results, set to 1 to simulate all scenarios

vaping_mort_effect=0



policyyear <- 2028
v.affected_ages <- c(0:99) # affects all ages
d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%
d.year <- 2023 # which year to start discounting from



args <- c("females",500, 2100) #no need to change this for now
source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions
load(paste0(mainDir,"data/nsduh_calib_targets_both.RData")) # Load NSDUH data

# Policy effect sizes:
# Apelberg (2018): Experts estimate 50% (10-85%) decrease in smoking initiation https://doi.org/10.1056/NEJMsr1714617
# Hatsukami (2024): Significantly higher 12-week CO-verified abstinence among those in VLNC vs NNC condition (OR=3.10, 95%: 1.69-5.96) https://doi.org/10.1016/j.lana.2024.100796

# updated policy effects code:
# apply_policy <- function(rr.init_1,rr.init_s, cess_indicator,rr.cess_1,rr.cess_s, p.CO.CE_1,p.CO.CE_s,p.CO.FE_1,p.CO.FE_s,p.NO.NE_1,p.NO.NE_s, policyyear, v.affected_ages) {

params <- list(
  baseline = NULL,
  init_0.1_cess_0.69 = c(1-0.38,1-0.39, 0.11, 0.11, 0.9, 0.82, 0.22,0.25, 0.38*0.72,0.39*0.75,0), #worst case
  init_0.5_cess_2.10 = c(1-0.63,1-0.65,0.36,0.34, 0.61, 0.51, 0.56,0.58, 0.63*0.5, 0.65*0.5,0), #expected
  init_0.85_cess_4.96 = c(1-0.83,1-0.85, 0.61,0.56,0.25, 0.19, 0.84, 0.85, 0.85*0.21,0.85*0.2,0) #best case
  #One way Sensitivity analysis (Policy effects and vaping mortality)
  #  = c(1-0.38,1-0.39, 0.11, 0.11, 0.9, 0.82, 0.22,0.25, 0.38*0.72,0.39*0.75,0), #worst case
  #  = c(1-0.63,1-0.65,0.36,0.34, 0.61, 0.51, 0.56,0.58, 0.63*0.5, 0.65*0.5,0), #expected
  # init_0.85_cess_4.96 = c(1-0.83,1-0.85, 0.61,0.56,0.25, 0.19, 0.84, 0.85, 0.85*0.21,0.85*0.2,0)
  
  #Sensitivity analysis (Depression)
  
  
  #FDA
  #FDA_est = c(1-0.63,1-0.65,"FDA",0.36,0.34, 0.61, 0.51, 0.56,0.58, 0.63*0.5, 0.65*0.5), #expected
  #initiation (1st,subsequent): -0.63, -0.65
  #cessation:0.36,0.34
  #dual: 0.61, 0.51
  #switching:0.56,0.58
  #vape init:0.5, 0.5
  #FDA_best = c(1-0.83,1-0.85,"FDA", 0.61,0.56,0.25, 0.19, 0.84, 0.85, 0.85*0.21,0.85*0.2), #best
  #initiation (1st,subsequent): -0.83, -0.85
  #cessation:0.61,0.56
  #dual:0.25, 0.19
  #switching:0.84, 0.85
  #vape init: 0.21, 0.2
  #FDA_worst = c(1-0.38,1-0.39,"FDA", 0.11, 0.11, 0.9, 0.82, 0.22,0.25, 0.38*0.72,0.39*0.75) #worse case
  #initiation (1st,subsequent): -0.38, -0.39
  #cessation:0.11, 0.11
  #dual:0.9, 0.82
  #switching:0.22,0.25
  #vape init: 0.72, 0.75
  #Sensitivity analysis
  
  
)

scenarios <- names(params)

# NSDUH prevalence data
df.calib_targets <- do.call(rbind, lapply(names(l.calib_targets), function(status) {
  cbind(data.frame(l.calib_targets[[status]]), status = status)
}))

# run all scenarios and save results OR use pre-generated results
if (run_scenarios == 0) {  # choose the files you want to use for both genders here:
  femalefile="rnc_females_1000_02.21.25_05.13PM.RData" #0 mortality
  malefile= "rnc_males_1000_02.21.25_05.31PM.RData"
  # the below files are our best estimates of 2/7
  # femalefile="rnc_females_1000_02.06.25_07.36PM.Rda"
  
  # Load and format files for females
  load(paste0(mainDir, "output/",femalefile))
  dfF=reformat_model_outputs(l.results)
  dfF_D=reformat_model_outputs(l.results_D)
  dfF_ND=reformat_model_outputs(l.results_ND)
  
  # Load and format files for males
  load(paste0(mainDir, "output/",malefile))  
  dfM=reformat_model_outputs(l.results)
  dfM_D=reformat_model_outputs(l.results_D)
  dfM_ND=reformat_model_outputs(l.results_ND)
  
  ## GENERATE FIGURES AND TABLES
  source(paste0(mainDir,"R/07a_figures_tables_combined_gender.R"), echo=TRUE)
  
} else {
  
  # Run the model -----------------------------------------------------------
  t.init = Sys.time()
  # SIMULATE FEMALE POPULATION
  args <- c("females",100, 2100) # Parameters for HPC vs non-HPC setup
  source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE) 
  source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions
  

  allresults <- lapply(scenarios, run_policy)
  names(allresults) <- scenarios
  
  # organize data by population
  l.results <- list() # total population
  l.results_D <- list() # depressed population
  l.results_ND <- list() # not depressed population
  for (s in 1:length(scenarios)){
    l.results[[s]] <- allresults[s][[1]][[1]] # combine all scenario results for general US population into a list
    l.results_D[[s]] <- allresults[s][[1]][[2]] # combine all scenario results for depressed (D) population into a list
    l.results_ND[[s]] <- allresults[s][[1]][[3]] # combine all scenario results for NOT depressed (ND) population into a list
  }
  names(l.results) <- names(l.results_D) <- names(l.results_ND) <- scenarios
  
  save(l.results, l.results_D, l.results_ND, file = paste0("output/mort_",vaping_mort_effect,"_rnc_",whichgender,"_",n.i,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".RData"))
  dfF=reformat_model_outputs(l.results)
  dfF_D=reformat_model_outputs(l.results_D)
  dfF_ND=reformat_model_outputs(l.results_ND)
  
  # SIMULATE MALE POPULATION
  args <- c("males",100, 2100) # Parameters for HPC vs non-HPC setup
  source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE) 
  source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

  allresults <- lapply(scenarios, run_policy)
  names(allresults) <- scenarios
  
  # organize data by population
  l.results <- list() # total population
  l.results_D <- list() # depressed population
  l.results_ND <- list() # not depressed population
  for (s in 1:length(scenarios)){
    l.results[[s]] <- allresults[s][[1]][[1]] # combine all scenario results for general US population into a list
    l.results_D[[s]] <- allresults[s][[1]][[2]] # combine all scenario results for depressed (D) population into a list
    l.results_ND[[s]] <- allresults[s][[1]][[3]] # combine all scenario results for NOT depressed (ND) population into a list
  }
  names(l.results) <- names(l.results_D) <- names(l.results_ND) <- scenarios
  
  save(l.results, l.results_D, l.results_ND, file = paste0("output/mort_",vaping_mort_effect,"_rnc_",whichgender,"_",n.i,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".RData"))
  
  dfM=reformat_model_outputs(l.results)
  dfM_D=reformat_model_outputs(l.results_D)
  dfM_ND=reformat_model_outputs(l.results_ND)
  
  ## GENERATE FIGURES AND TABLES
  source(paste0(mainDir,"R/07a_figures_tables_combined_gender.R"), echo=TRUE)
  print(Sys.time() - t.init)
}
